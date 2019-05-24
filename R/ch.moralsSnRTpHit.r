#' A function to analyze the Morals data by subject
#'
#' This function analyzes the group  morals data.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param fitCol a string that specifies the name of the new column that will contain the predicted datapoints.
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @param overlapCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @keywords morals data analysis by subject
#' @return dataframe with the learning function fit and residuals
#' @export
#' @examples ch.moralsSnRTpHit (data=moralsData,"sn", "trial", "RT", "res.RT", "fit.RT", "overlap", "keyDef", c("Yes", "No"), "correct", params=parameters)


ch.moralsSnRTpHit <- function (data, snCol, trialCol, RTCol, fitCol, resCol, overlapCol, correctCol, correctVals = c(TRUE, FALSE), params) {

    #create new directories
    mainDir <- getwd()

    ch.newDir (mainDir, params$snSubDir)
    snDir <- getwd()
    setwd(mainDir)

		#calculate overlapRound values to use for individual subject analysis
    data$overlapRoundSN <- ch.round_any(data[[overlapCol]], params$roundThresholdSN, params$roundDirection)
		#prepare stats output file
		statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))
		#identify our subjects
		subs <- unique (data[[snCol]])

    op <- par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,2), cex=2, las=1)

		#calculate RT residuals
		if(params$RTresid & !params$RTresidSN) {
			outList <- ch.moralsRmLearningEffect(data, trialCol,RTCol, fitCol, resCol)
				learning.fit <- outList$nlsFit
				data <- outList$data
		} else {
			if (params$RTresidSN){
				subData <- NULL
				for (j in subs) {
					outList <- ch.moralsRmLearningEffect(data[data[[snCol]] == j,], trialCol,RTCol, fitCol, resCol)
					subData <- ch.rbind (subData, outList$data)
				}

        op1 <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1, cex=1)
  			setwd(snDir)
  			ReactionTimeLabel <- ch.getMoralsRTaxisName(params$keybRTtransform, params$RTresid)
  			ch.moralsPlotLearningEffect(subData, trialCol, RTCol, fitCol, resCol, yLabel = ReactionTimeLabel, filenameID = paste(params$dt.set,"snAll"), cex1 = 1)
  			setwd(mainDir)
        par(op1)

			} else {
        subData <- data
				subData[[resCol]] <- data[[RTCol]]
				subData[[fitCol]] <- 0.0
			}
		}

		### fit RT and pHit data for All data with Sn resids
		op2 <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.5, las=1)
		plotFilename = file.path(snDir,paste(params$dt.set, "snAll rt p(Hit).pdf"))
		outList <- ch.moralsRTpHitFit(subData, "overlapRoundSN", resCol, correctCol, correctVals, filename = plotFilename)

		sink(statsOutputFile, append = T)
			cat("\n\n********************************** Analysis By SN  **********************************\n\n")
			cat("\n\nRound Threshold for Subjects\n\n")
			cat(params$roundThresholdSN)
	    cat("\n\n********************************** Overall SN resids **********************************\n\n")
			cat("\n\n**** Average RT ****\n\n")
			print(summary(outList$RTfit))
			cat("\n\n**** p(Hit) ****\n\n")
			print(summary(outList$pHitFit))
			cat("r_square: ",outList$pHitR2)
		sink(NULL)
		par(op2)

		### fit RT and pHit data for individual Subjects
		op3 <- par(mfrow=c(params$numPlotRows,2),bty="n", font=1, family='serif', mar=c(5,6,4,2), cex=1, las=1)
		subOutData <- NULL
    rowNum <- 1
    for (j in subs)  {
			tmp <- subData[subData[[snCol]]==j,]
			outList <- ch.moralsRTpHitFit(tmp, "overlapRoundSN", resCol, correctCol, correctVals, plotTitle = paste("sn", j), printR2 = T, cex1=1)

	    if(rowNum %% params$numPlotRows == 0) {
				plotFilename <- file.path(snDir,paste(params$dt.set, "sn", j, "rt p(Hit).pdf"))
		    dev.copy(pdf, plotFilename, width=12, height=9)
		    dev.off();
	    }

			#make r2 negative if the RT slope is negative
	    rtR2 <- ifelse( coef(outList$RTfit)[2] < 0, -1*summary(outList$RTfit)$r.squared, summary(outList$RTfit)$r.squared)
			#store all the subject fit parameters in a dataframe
			tmp1 <- data.frame(sn = j, rtInt = coef(outList$RTfit)[1],rtSlo = coef(outList$RTfit)[2],rtR2 = rtR2 ,phB = outList$pHitBeta, phR2 = outList$pHitR2)
			subOutData <- ch.rbind (subOutData, tmp1)
	    rowNum <- rowNum + 1

	    sink(statsOutputFile, append = T)
		    cat("\n\n********************************** sn", j , "**********************************\n\n")
				cat("\n\n**** Average RT ****\n\n")
				print(summary(outList$RTfit))
				cat("\n\n**** p(Hit) ****\n\n")
				print(summary(outList$pHitFit))
				cat("r_square: ",outList$pHitR2)
	    sink(NULL)

    }
    #print last graph - just in case there was not a full set that finished
		plotFilename <- file.path(snDir,paste(params$dt.set, "sn", j, "rt p(Hit).pdf"))
    dev.copy(pdf, plotFilename, width=12, height=9)
    dev.off();

    write.table(subOutData, file="snOutParams.txt", quote=F, sep="\t", row.names=F)
    sink(statsOutputFile, append = T)
	    cat("\n\n********************************** Parameter Stats **********************************\n\n")
			cat("\n\n**** Summary Sn Fits ****\n\n")
	    print(summary(subOutData[2:length(subOutData)]))
			cat("\n\n**** Mean Sn Fits ****\n\n")
	    print(colMeans(subOutData[2:length(subOutData)], na.rm = T))
			cat("\n\n**** SD Sn Fits ****\n\n")
	    print(apply(subOutData[2:length(subOutData)], 2, sd, na.rm = T))
			cat("\n\n**** T-test Subjects' RT Intercept == 0 ****\n\n")
	    print(t.test(subOutData$rtInt, mu=0))
			cat("\n\n**** T-test Subjects' RT Slope == 0 ****\n\n")
	    print(t.test(subOutData$rtSlo, mu=0))
			cat("\n\n**** T-test Subjects' RT r2 == 0 ****\n\n")
	    print(t.test(subOutData$rtR2, mu=0))
			cat("\n\n**** T-test Subjects' p(Hit) Beta == 0 ****\n\n")
	    print(t.test(subOutData$phB, mu=0))
			cat("\n\n**** T-test Subjects' p(Hit) p(Hit) r2 == 0 ****\n\n")
	    print(t.test(subOutData$phR2, mu=0))
    sink(NULL)

		#plot all the subjects RT slopes, pHit curves, and respective r2s on a single graph for publication
		filename <- file.path(snDir,paste(params$dt.set, "RT pHit r2 sn.pdf"))
		xAll <- with(subData, tapply(overlapRoundSN, overlapRoundSN, mean))
		ch.moralsPlotSnRTpHitFits(subOutData, "sn", "rtSlo", "rtInt", "rtR2", "phB", "phR2", xAll, filename)

    par(op)
		#return a dataframe with the residuals based on fitting individual subjects learning functions
		return(subData)
}
