#' A function to filter the morals data for analysis
#'
#' This function filters the morals data for analysis by removing RT outliers and bad subjects based on RT and p(hit). The thresholds for removal are set in the moralsDBfile that is read and stored in a parameter list.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the rounded overlaps for the item in each trial. Column occured in ch.moralsDataPrep().
#' @param aveRTcol a string that specifies the name of the column in "data" that contains the average RT by subject. Column created in ch.moralsDataPrep().
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param item1cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 1.
#' @param item2cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 2.
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @keywords morals data filter
#' @return a dataframe of filtered data.  It also writes the data to "analysisReadyData.txt," which will be used by other functions.
#' @export
#' @examples ch.moralsFilterData (data=moralsData, "sn", "RT", "overlapRound", "aveRT", "avePred", params=parameters)

ch.moralsFilterData <- function (data, snCol, RTcol, overlapRoundCol, aveRTcol, correctCol, correctVals = c(TRUE, FALSE), item1cols = c("Item1"), item2cols = c("Item2"), params) {

				#create new directories
				mainDir <- getwd()

				ch.newDir (mainDir, params$gpSubDir)
				gpDir <- getwd()
				setwd(mainDir)
				statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))

				#ensure correct/incorrect is coded as 1/0
				data$correct01 <- ifelse (data[[correctCol]]==correctVals[1], 1, 0)

				total.rawsubs <-length(levels(factor(data[[snCol]])))

				substats.raw <- as.data.frame(data %>% dplyr::group_by_(snCol) %>% dplyr::summarise(mRT = mean(eval(parse(text=RTcol))), sdRT = sd(eval(parse(text=RTcol))), avePred = mean(correct01), N = length(correct01) ) )

				data <- merge(data,substats.raw, by= snCol)

				#remove subs with average RT outside of thresholds
				outList <- ch.filterGrpBtwn(data, RTcol, snCol, params$lowAveThresholdRT, params$highAveThresholdRT, FUN=mean)
				data <- outList$datKeptRaw
				numSn.Removed.aveRT <- outList$numRemoved
				sn.removed.RT.sum <- outList$datRemoved
				#calculate the num of subject whose prediction is below the chance //
				outList <- ch.filterGrpBtwn(data, "correct01", snCol, lowThresh = params$chanceThreshold, FUN=mean)
				data <- outList$datKeptRaw
				numSb.belowchance <- outList$numRemoved
				sn.removed.belowchance	<- outList$datRemoved
				final.numSbj <- length(unique(data[[snCol]]))

				substats <- as.data.frame(data %>% dplyr::group_by_(snCol) %>% dplyr::summarise(mRT = mean(eval(parse(text=RTcol))), sdRT = sd(eval(parse(text=RTcol))), avePred = mean(correct01), N = length(correct01) ) )

				mean.avePred <-mean(substats$avePred)

				#remove overlapRound whose trials are fewer than the minimum
				outList <- ch.filterGrpBtwn(data, RTcol, overlapRoundCol, lowThresh = params$minOverlapN, FUN=length)
				data <- outList$datKeptRaw
				overlapsToRemove <- outList$datRemoved
				overlapStats <- as.data.frame(data %>% dplyr::group_by_(overlapRoundCol) %>% dplyr::summarise(mRT = mean(eval(parse(text=RTcol))), sdRT = sd(eval(parse(text=RTcol))), avePred = mean(correct01), N = length(eval(parse(text=RTcol)))))

				sink(statsOutputFile, append = F)
					cat("\nMinimum Overlap N:")
					cat("\n\t", params$minOverlapN)
					cat("\n\nOverlap Stats:\n")
					print(overlapStats)
					cat("\nOverlaps Removed:\n")
					print(overlapsToRemove)
				sink(NULL)

			#	calculate proportion of data points removed from RT thresholds
				outList <- ch.filterDataBetween(data, RTcol, params$lowRTthreshold, params$highRTthreshold)
				data <- outList$datKept
				dt.Removed <- outList$pRemoved

			#	set dt.raw.merged for raw.table
				dt.raw.table <- as.data.frame(data %>% dplyr::summarise (MEAN = mean(eval(parse(text=RTcol))), SD = sd(eval(parse(text=RTcol))) ) )

			#	log dt.merged$keybRT
				if ( params$keybRTtransform == "log") {
					data[[RTcol]] <- log(data[[RTcol]])
				}

				### output a list of items by overlap
				#itemCols <- ch.getMoralsItemColumnNames(params, colBaseName = "Item")
				itemCols <- c(item1cols, item2cols)

				outList <- ch.getMoralsItemsInAllOverlaps(data, overlapRoundCol, itemCols)
				overlapSummaryOutFile <- file.path(mainDir,"Probes in OverlapRound.txt")
				sink(overlapSummaryOutFile, append = F)
					print("**** Items by Overlap ****")
					print(outList)
				sink(NULL)

		# END DATA PREPERATION

			dataOutFile <- file.path(mainDir,params$analysisReadyOutFile)
			write.table(data, file=dataOutFile, quote=F, sep="\t", row.names=F)

			sink(statsOutputFile, append = T)
				cat("\nlow RT Threshold:")
				cat("\n\t", params$lowRTthreshold)
				cat("\n\nhigh RT Threshold:")
				cat("\n\t",params$highRTthreshold)
				cat("\n\nHigh Average RT Threshold:")
				cat("\n\t",params$highAveThresholdRT)
				cat("\n\nlow Average RT Threshold:")
				cat("\n\t",params$lowAveThresholdRT)
				cat("\n\nRound Threshold:")
				cat("\n\t",params$roundThreshold)
				cat("\n\nChance Threshold:")
				cat("\n\t",params$chanceThreshold)
				cat("\n\nMinimum N for Overlap:")
				cat("\n\t",params$minOverlapN)
				cat("\n\nTransform RT Into:")
				cat("\n\t",params$keybRTtransform)
				cat("\n\nUse RT Residuals:")
				cat("\n\t",params$RTresid)
				cat("\n\nTotal Raw Subjects:")
				cat("\n\t",total.rawsubs)
				cat("\n\nRaw Subject Stats:\n")
				print(substats.raw)
				cat("\n\nN Subjects Removed because of Average RT Threshold:")
				cat("\n\t",numSn.Removed.aveRT)
				cat("\n\nSummary of Subjects Removed because of Average RT Threshold:\n")
				print(sn.removed.RT.sum)
				cat("\n\nN Subjects Removed because of Below Chance Threshold:")
				cat("\n\t",numSb.belowchance)
				cat("\n\nSummary of Subjects Removed because of Below Chance Threshold:\n")
				print(sn.removed.belowchance)
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
				cat("\n\nProportion of Trials Removed because of Trial RT Thresholds:")
				cat("\n\t",dt.Removed)
				cat("\n\nSummary of Overlaps Removed because of Minimum N Overlap Threshold:\n")
				print(overlapsToRemove)
			sink(NULL)

			return(data)
}
