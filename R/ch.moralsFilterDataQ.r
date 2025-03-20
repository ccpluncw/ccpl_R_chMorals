#' A function to filter the morals data for analysis using quantiles rather than fixed values.
#'
#' This function filters the morals data for analysis by removing RT outliers and bad subjects based on RT and p(hit) using quantiles rather than fixed values. The thresholds for removal are set in the moralsDBfile that is read and stored in a parameter list.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the rounded overlaps for the item in each trial. Column occured in ch.moralsDataPrep().
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param statsOutputFile the filename that you want the statistics summary output written to. DEFAULT = NULL (construct filename from "params")
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r." Use quantiles (e.g., 0.05, rather than fixed values when filling in the lowAveThresholdRT, highAveThresholdRT, lowThresholdRT, and highThresholdRT)
#' @keywords morals data filter
#' @return a dataframe of filtered data.  It also writes the data to "analysisReadyData.txt," which will be used by other functions.
#' @export
#' @examples ch.moralsFilterData (data=moralsData, "sn", "RT", "overlapRound", "aveRT", "avePred", params=parameters)

ch.moralsFilterDataQ <- function (data, snCol, RTcol, overlapRoundCol, correctCol, correctVals = c(TRUE, FALSE), statsOutputFile = NULL, params) {

				if(is.null(statsOutputFile)) {
					statsOutputFile <- paste(params$dt.set, params$statsOutputFilePrefix)
				}

				total.rawsubs <-length(levels(factor(data[[snCol]])))

				sink(statsOutputFile, append = T)
					cat("\n\nTotal Raw Subjects:")
					cat("\n\t",total.rawsubs)
				sink(NULL)

				data <- ch.globalFilterByQuantile(data, snCol = snCol, RTcol = RTcol, CVlowQuantileThreshold = params$lowAveThresholdRT, CVhighQuantileThreshold = params$highAveThresholdRT, statsOutputFile = statsOutputFile)

				data <- ch.filterOverlapsByQuantile(data, snCol = snCol, RTcol = RTcol, overlapRoundCol = overlapRoundCol, correctCol = correctCol, correctVals = correctVals, chanceThreshold = params$chanceThreshold, lowRTquantileThreshold = params$lowRTthreshold, highRTquantileThreshold = params$highRTthreshold, minOverlapN = params$minOverlapN, statsOutputFile = statsOutputFile)

				final.numSbj <- length(unique(data[[snCol]]))

				substats <- as.data.frame(data %>% dplyr::group_by(across(all_of(snCol))) %>% dplyr::summarise(mlRT = mean(eval(parse(text=RTcol)), na.rm=T), sdlRT = sd(eval(parse(text=RTcol)), na.rm=T), cv = sd(eval(parse(text=RTcol)), na.rm = T)/mean(eval(parse(text=RTcol)), na.rm = T), avePred = mean(correct01, na.rm=T), N = length(correct01) ) )

				mean.avePred <-mean(substats$avePred)

			#	set dt.raw.merged for raw.table
				dt.raw.table <- as.data.frame(data %>% dplyr::summarise (MEAN = mean(eval(parse(text=RTcol)), na.rm=T), SD = sd(eval(parse(text=RTcol)), na.rm=T) ) )

			sink(statsOutputFile, append = T)
				cat("\n\nFinal Analyzed Subjects:")
				cat("\n\t",final.numSbj)
				cat("\n\nFinal Subject Stats:\n")
				print(substats)
				cat("\nMean Average p(hit):")
				cat("\n\t", mean.avePred)
				cat("\n\nRT Mean and SD of Entire Dataset:\n")
				print(dt.raw.table)
				cat("\n\nMean and SD number of trials of Entire Dataset:\n")
				cat("\n\t", mean(substats$N), "\t", sd(substats$N))
			sink(NULL)

			dataOutFile <- file.path(params$analysisReadyOutFile)
			write.table(data, file=dataOutFile, quote=F, sep="\t", row.names=F)

			subStatsOutFile <- file.path(paste(params$dt.set, "subStats.txt"))
			write.table(substats, file=subStatsOutFile, quote=F, sep="\t", row.names=F)

			return(data)
}
