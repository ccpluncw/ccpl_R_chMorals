#' A function to analyze the Morals data by subject
#'
#' This function analyzes the group  morals data.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param itemCols a vector of strings that specifies the names of the columns in "data" that contains the the probes. This is all the columns for all of the groups
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param useTwoParameterModel A boolean that specifies whether to use a two parameter p(HOV) model.  If this is set to TRUE, then this function will fit a p(HVO) model whereby the rightmost point (overlap = 1.0) is not fixed at p(HVO) = 0.5. DEFAULT = FALSE.
#' @param minNperOverlap an integer that specifies the minimum number of trials necessary to include an overlap bin in the graph. DEFAULT = 0.
#' @param minUniqueOverlaps An integer specifying the minimum number of unique overlap bins necessary for the program to calculate the pHVO and RT function.  DEFAULT = 3.
#' @param statsOutputFile the filename that you want the statistics summary output written to. DEFAULT = NULL (construct filename from "params")
#' @param numPlotRows an integer that specifies the number of rows in the output figure, DEFAULT = 2)
#' @param dt.set A string that specifies the a short title to identify the output of these plots. DEFAULT = NULL)
#' @keywords morals data analysis by subject
#' @return dataframe with the learning function fit and residuals
#' @export
#' @examples ch.moralsSnRTpHit (data=moralsData,"sn", "trial", "RT", "res.RT", "fit.RT", "overlap", "keyDef", c("Yes", "No"), "correct", params=parameters)


ch.moralsItemRTpHit <- function (data, itemCols, resCol, overlapRoundCol, correctCol, correctVals = c(TRUE, FALSE), useTwoParameterModel= FALSE, minNperOverlap = 0, minUniqueOverlaps = 3, statsOutputFile = NULL, numPlotRows = 2, dt.set = NULL) {


    probes <- NULL
    for(i in length(itemCols)) {
      probes <- unique(c(probes, data[,itemCols[i]]))
    }

    sink(statsOutputFile, append = T)
      cat("\n\n*************************************** ITEM ANALYSIS ***************************************\n\n")
    sink(NULL)

    ### fit RT and pHit data for All data with Sn resids
		op2 <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.5, las=1)
		plotFilename = file.path(paste(dt.set, "All Items rt p(Hit).pdf"))
		outList <- ch.moralsRTpHitFit(data, overlapRoundCol, resCol, correctCol, correctVals, useTwoParameterModel = useTwoParameterModel, filename = plotFilename, minUniqueOverlaps = minUniqueOverlaps)

		sink(statsOutputFile, append = T)
	    cat("\n\n********************************** All Items **********************************\n\n")
			cat("\n\n**** Average RT ****\n\n")
			print(summary(outList$RTfit))
			cat("\n\n**** p(Hit) ****\n\n")
			print(summary(outList$pHitFit))
			cat("r_square: ",outList$pHitR2)
		sink(NULL)
		par(op2)

	  ### fit RT and pHit data for individual Subjects
		op <- par(mfrow=c(numPlotRows,2),bty="n", font=1, family='serif', mar=c(5,6,4,2), cex=1, las=1)
		itemOutData <- NULL
    rowNum <- 1
    for (j in probes)  {
      tmp <- data %>% filter_at(vars(itemCols), any_vars(. == j))
      ### filter out Overlaps with fewer than params$minOverlapNsn observations
      tmpOut <- ch.filterGrpByN(tmp, overlapRoundCol, grpCol=overlapRoundCol, minNperOverlap)
      tmp <- tmpOut$datKept
      uniqueOverlapItem <- length(unique(tmp[[overlapRoundCol]]))
      #run the analysis if there are at least three overlap levels
      if(uniqueOverlapItem >= minUniqueOverlaps) {
  			outList <- ch.moralsRTpHitFit(tmp, overlapRoundCol, resCol, correctCol, correctVals, useTwoParameterModel= useTwoParameterModel, plotTitle = paste("probe", j), printR2 = T, cex1=1)
  	    if(rowNum %% numPlotRows == 0) {
  				plotFilename <- file.path(paste(dt.set, "probe", j, "rt p(Hit).pdf"))
  		    dev.copy(pdf, plotFilename, width=12, height=9)
  		    dev.off();
  	    }

  			#make r2 negative if the RT slope is negative
  	    rtR2 <- ifelse( coef(outList$RTfit)[2] < 0, -1*summary(outList$RTfit)$r.squared, summary(outList$RTfit)$r.squared)
  			#store all the subject fit parameters in a dataframe
  			tmp1 <- data.frame(probe = j, rtInt = coef(outList$RTfit)[1],rtSlo = coef(outList$RTfit)[2],rtR2 = rtR2 ,phB = outList$pHitBeta,phA = outList$pHitAlpha, phR2 = outList$pHitR2)
  			itemOutData <- ch.rbind (itemOutData, tmp1)
  	    rowNum <- rowNum + 1

  	    sink(statsOutputFile, append = T)
  		    cat("\n\n********************************** probe:", j , "**********************************\n\n")
  				cat("\n\n**** Average RT ****\n\n")
  				print(summary(outList$RTfit))
  				cat("\n\n**** p(Hit) ****\n\n")

          tryCatch ({
              print(summary(outList$pHitFit))
            }, error = function(e) {
              print(paste("pHitFit error:", e))
          })

  				cat("r_square: ",outList$pHitR2)
  	    sink(NULL)
      }
    }
    #print last graph - just in case there was not a full set that finished
		plotFilename <- file.path(paste(dt.set, "probe", j, "rt p(Hit).pdf"))
    dev.copy(pdf, plotFilename, width=12, height=9)
    dev.off();


    write.table(itemOutData, file="probeOutParams.txt", quote=F, sep="\t", row.names=F)
    #remove subs that model did not fit
    itemOutData.noNA <- na.omit(itemOutData)
    sink(statsOutputFile, append = T)
	    cat("\n\n********************************** Parameter Stats Probes**********************************\n\n")
      cat("raw N: ", nrow(itemOutData), "\n\n")
      cat("Number of NAs in each Column:\n\n")
      print(colSums(is.na(itemOutData)))
      cat("\n\n**** Summary Probe Fits ****\n\n")
	    print(summary(itemOutData[2:length(itemOutData)]))
			cat("\n\n**** Mean Probe Fits ****\n\n")
	    print(colMeans(itemOutData[2:length(itemOutData)], na.rm = T))
			cat("\n\n**** SD Probe Fits ****\n\n")
	    print(apply(itemOutData[2:length(itemOutData)], 2, sd, na.rm = T))
			cat("\n\n**** T-test Probe' RT Intercept == 0 ****\n\n")
	    print(t.test(itemOutData$rtInt, mu=0))
			cat("\n\n**** T-test Probe' RT Slope == 0 ****\n\n")
	    print(t.test(itemOutData$rtSlo, mu=0))
			cat("\n\n**** T-test Probe' RT r2 == 0 ****\n\n")
	    print(t.test(itemOutData$rtR2, mu=0))
			cat("\n\n**** T-test Probe' p(Hit) Beta == 0 ****\n\n")
	    print(t.test(itemOutData$phB, mu=0))
			cat("\n\n**** T-test Probe' p(Hit) p(Hit) r2 == 0 ****\n\n")
	    print(t.test(itemOutData$phR2, mu=0))
    sink(NULL)

		#plot all the subjects RT slopes, pHit curves, and respective r2s on a single graph for publication
		filename <- file.path(paste(dt.set, "RT pHit r2 probe.pdf"))
		xAll <- with(data, tapply(overlapRound, overlapRound, mean))
		ch.moralsPlotSnRTpHitFits(itemOutData, "probe", "rtSlo", "rtInt", "rtR2", "phB", "phA", "phR2", xAll, filename)

    par(op)
		#return a dataframe with the residuals based on fitting individual subjects learning functions
		return(itemOutData)
}
