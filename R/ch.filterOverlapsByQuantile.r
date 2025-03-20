#' A function to filters the choice data by value overlaps using quantiles rather than fixed values.
#'
#' This function filters the choice data for analysis by removing RT outliers and bad subjects based on RT and p(HVO) using quantiles rather than fixed values.
#' @param data A dataframe containing the choice RT data. (typically after running through ch.moralsDataPrep()).
#' @param snCol A string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol A string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param overlapRoundCol A string that specifies the name of the column in "data" that contains the rounded overlaps for the item in each trial. Column created in ch.moralsDataPrep() via ch.mergeChoiceDataWithOverlapsData().
#' @param correctCol A string that specifies the name of the new column that will contains an indication of whether or not the response was correct.
#' @param correctVals A vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param chanceThreshold A number specifying the quantile that all subjects whose p(HVO) falls below it will be removed from the dataset. DEFAULT = 0 (none removed)
#' @param lowRTquantileThreshold A number specifying the quantile that, for each overlapRound, individual RTs that fall below it will be removed from the dataset. DEFAULT = 0.0 (none removed)
#' @param highRTquantileThreshold A number specifying the quantile that, for each overlapRound, individual RTs that fall above  it will be removed from the dataset. DEFAULT = 1.0 (none removed)
#' @param minOverlapN A number specifying the minimum number of responses an overlapRound condition must have to remain in the dataset. DEFAULT = 0 (none removed)
#' @param statsOutputFile the filename that you want the statistics summary output written to. DEFAULT = NULL (no file written)
#' @keywords choice data filter overlap
#' @return a dataframe of filtered data.
#' @export
#' @examples ch.filterOverlapsByQuantile (data=moralsData, "sn", "RT", "overlapRound", "avePred", correctVals = c(TRUE, FALSE), chanceThreshold = 0.5, lowRTquantileThreshold = 0.025, highRTquantileThreshold = 0.975, minOverlapN = 40)

ch.filterOverlapsByQuantile <- function (data, snCol, RTcol, overlapRoundCol, correctCol, correctVals = c(TRUE, FALSE), chanceThreshold = 1.0, lowRTquantileThreshold = 0, highRTquantileThreshold = 1.0, minOverlapN = 0, statsOutputFile = NULL) {

				#ensure correct/incorrect is coded as 1/0
				data$correct01 <- ifelse (data[[correctCol]]==correctVals[1], 1, 0)

				substats.raw <- as.data.frame(data %>% dplyr::group_by(across(all_of(snCol))) %>% dplyr::summarise(avePred = mean(correct01, na.rm = T), N = length(correct01) ) )

#				data <- merge(data,substats.raw, by= snCol)

				#calculate the num of subject whose prediction is below the chance and remove them//
				outList <- chutils::ch.filterDataBetween(substats.raw, "avePred", lowThresh=chanceThreshold, highThresh=NULL)

				### keep only good subjects
				data <- data[!(data[[snCol]] %in% outList$datRemoved[[snCol]]), ]
				numSn.belowchance <- outList$numRemoved
				sn.removed.belowchance <- outList$datRemoved

			#	Remove individual RTs based on quantiles grouped by overlapRound
				outList <- chutils::ch.filterGrpByQuantile(data, RTcol, overlapRoundCol, lowRTquantileThreshold, highRTquantileThreshold)
				data <- outList$datKept
				dt.Removed <- outList$pRemoved

				#remove overlapRound whose trials are fewer than the minimum
				outList <- chutils::ch.filterGrpBtwn(data, RTcol, overlapRoundCol, lowThresh = minOverlapN, FUN=length)
				data <- outList$datKeptRaw
				overlapsToRemove <- outList$datRemoved

				overlapStats <- as.data.frame(data %>% dplyr::group_by(across(all_of(overlapRoundCol))) %>% dplyr::summarise(mRT = mean(eval(parse(text=RTcol)), na.rm=T), sdRT = sd(eval(parse(text=RTcol)), na.rm=T), avePred = mean(correct01, na.rm=T), N = length(eval(parse(text=RTcol)))))

			if(!is.null(statsOutputFile)) {
				sink(statsOutputFile, append = T)
					cat("\n\nRemove individual RTs in each overlapRound that are below the following quantile:")
					cat("\n\t", lowRTquantileThreshold)
					cat("\n\nRemove individual RTs in each overlapRound that are above the following quantile:")
					cat("\n\t",highRTquantileThreshold)
					cat("\n\nProportion of Trials Removed because of individual trial RT Thresholds:")
					cat("\n\t",dt.Removed)
					cat("\n\nRemove all participants whose p(HVO) is below this Chance Threshold:")
					cat("\n\t",chanceThreshold)
					cat("\n\nN Subjects Removed because of Below Chance Threshold:")
					cat("\n\t",numSn.belowchance)
					cat("\n\nSummary of Subjects Removed because of Below Chance Threshold:\n")
					print(sn.removed.belowchance)
					cat("\n\nRemove overlapRound bins that have fewer trials across participants than this Minimum N:")
					cat("\n\t",minOverlapN)
					cat("\n\nSummary of Overlaps Removed because of Minimum N Overlap Threshold:\n")
					print(overlapsToRemove)
					cat("\n\nOverlap Stats:\n")
					print(overlapStats)
				sink(NULL)
			}

			return(data)
}
