#' A function to calculate a dPrime analysis on the Morals data.
#'
#' This function analyzes the group  morals data.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap round column.
#' @param correctCol a string the specifies the column name in "data" that contains the variable whether the participant's response was correct.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param targetPresentCol a string that specifies the name of the column in "data" that contains the whether or not the target was presented.
#' @param targetPresentVals a vector of two values that specifies the "target present" value (index 1) and the "target absent" value (index 2). e.g, c("yes", "no")
#' @param resCol a string that specifies the name of the new column that will contain the residual datapoints.
#' @param addCorrection a boolean that specifies whether you want a .5 correction to be added the total hits, FAs, misses, and CRs. This corrects for 0 and 1 values for FA and Hits. DEFAULT = T.
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @keywords morals data analysis dPrime d prime
#' @return a dataframe with the dPrime tatistics.
#' @export
#' @examples ch.moralsDprimeAnalysis (data=moralsData,"overlapRound", "correct", c(1,0), "targetPresent", c(TRUE,FALSE), params = params)

ch.moralsDprimeAnalysis <- function (data, overlapRoundCol, correctCol, correctVals, targetPresentCol, targetPresentVals, resCol, addCorrection = TRUE, params, printR2 = TRUE, filenameID = NULL) {

		#create new directories
		mainDir <- getwd()

		ch.newDir (mainDir, params$gpSubDir)
		gpDir <- getwd()
  	setwd(mainDir)

		#prepare stats output file
		statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))

    #	create data.frame of dt.overall.mean.median.sd
		df.dPrime <- ch.calculateDprimeStats(data, overlapRoundCol, correctCol, correctVals, targetPresentCol, targetPresentVals, addCorrection = TRUE)

		#plot d prime and beta by overlap round

		filename <- NULL
		if (!is.null(filenameID)) {
			filename <- file.path(gpDir,paste(params$dt.set, filenameID, "d prime.pdf"))
		}
		dp.outList <- ch.moralsPlotDprimeBetaFits(data, overlapRoundCol, correctCol, correctVals, targetPresentCol, targetPresentVals, printR2 = printR2, filename = filename)

		#get RT summarized by overlap for plot
		df.sum <- ch.summariseBy(data, overlapRoundCol, resCol, "aveRT", mean)
		df.dPrime <- merge (df.dPrime, df.sum)

		#plot d prime and beta by RT
		op <- par(mfrow=c(2,1),bty="n", font=1, family='serif', mar=c(2,5,2,5), oma=c(3,0,3,0), cex=1.25, las=1)
		DprimeRTFit <- ch.plot.lm(df.dPrime$aveRT, df.dPrime$dPrime, cex1 = 1.5, printR2 = printR2, yLabel  = "d'")
		betaRTFit <- ch.plot.lm(df.dPrime$aveRT, df.dPrime$beta, cex1 = 1.5, printR2 = printR2, yLabel  = 'beta')

		if (!is.null(filenameID)) {
				filename <- file.path(gpDir,paste(params$dt.set, filenameID, "RT by d prime and beta.pdf"))
				dev.copy(pdf, filename, width=6, height=9)
				dev.off();
		}

		par(op)

		#assess how d prime and beta predict RT
		RTbyDp.fit <- with(df.dPrime, lm(aveRT~eval(parse(text=overlapRoundCol))+beta+(eval(parse(text=overlapRoundCol))*beta)))

		sink(statsOutputFile, append = T)
			cat("\n\n********************************** D Prime Statistic Output **********************************\n\n")
			cat("\n\n**** Summary d primes ****\n\n")
			print(df.dPrime)
			cat("\n\n**** D Prime by Overlap lm fit ****\n\n")
			print(summary(dp.outList$dPrimeFit))
			cat("\n\n**** Beta by Overlap lm fit ****\n\n")
			print(summary(dp.outList$betaFit))
			cat("\n\n**** D Prime by RT lm fit ****\n\n")
			print(summary(DprimeRTFit))
			cat("\n\n**** Beta by RT lm fit ****\n\n")
			print(summary(betaRTFit))
			cat("\n\n**** RT ~ overlapRound + beta + (overlapRound*beta) lm fit ****\n\n")
			print(summary(RTbyDp.fit))
		sink(NULL)

		return(df.dPrime)
}
