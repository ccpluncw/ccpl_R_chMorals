#' A function to filter RT data based on the coefficient of variation.
#'
#' This function filters RT data based on the coefficient of variation.  Specifically, it removes subjects based on their variability of RTs relative to the subjects in the dataset (by quantile).
#' @param data A dataframe containing the choice RT data. (typically after running through ch.moralsDataPrep()).
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param CVlowQuantileThreshold A number specifying the quantile that all subjects whose coefficient of variation falls below it will be removed from the dataset. DEFAULT = 0 (none removed)
#' @param CVhighQuantileThreshold A number specifying the quantile that all subjects whose coefficient of variation falls above it will be removed from the dataset. DEFAULT = 1 (none removed)
#' @param statsOutputFile the filename that you want the statistic summary output written to. DEFAULT = NULL (no file written)
#' @keywords filter quantile global
#' @return a dataframe of filtered data.
#' @export
#' @examples ch.globalFilterByQuantile (data=moralsData, "sn", "RT", CVlowQuantileThreshold = 0, CVhighQuantileThreshold = 0.95)

ch.globalFilterByQuantile <- function (data, snCol, RTcol, CVlowQuantileThreshold = 0, CVhighQuantileThreshold = 1, statsOutputFile = NULL) {

			total.rawsubs <-length(levels(factor(data[[snCol]])))

			substats.raw <- as.data.frame(data %>% dplyr::group_by_(snCol) %>% dplyr::summarise(mlRT = mean(eval(parse(text=RTcol)), na.rm = T), sdlRT = sd(eval(parse(text=RTcol)), na.rm = T), cv = sd(eval(parse(text=RTcol)), na.rm = T)/mean(eval(parse(text=RTcol)), na.rm = T), N = length(eval(parse(text=RTcol))) ) )

#			data <- merge(data,substats.raw, by= snCol)

			#remove subs with coefficient of variations that are too high. This indicates too much variability in the data
			outList <- chutils::ch.filterGrpByQuantile(substats.raw, "cv", lowQuantileThreshold=CVlowQuantileThreshold, highQuantileThreshold=CVhighQuantileThreshold)

			### keep only good subjects
			data <- data[!(data[[snCol]] %in% outList$datRemoved[[snCol]]), ]
			substats.noCVoutliers	<- outList$datKept
			numSn.Removed.aveRT <- outList$numRemoved
			sn.removed.RT.sum <- outList$datRemoved

			if(!is.null(statsOutputFile)) {
				sink(statsOutputFile, append = T)
					cat("\n\nRemove participants whose coefficient of variation (cv) is above the following quantile:")
					cat("\n\t",CVhighQuantileThreshold)
					cat("\n\nRemove participants whose coefficient of variation (cv) is below the following quantile:")
					cat("\n\t",CVlowQuantileThreshold)
					cat("\n\nTotal Raw Subjects:")
					cat("\n\t",total.rawsubs)
					cat("\n\nRaw Subject Stats:\n")
					print(substats.raw)
					cat("\n\nN Subjects Removed because of RT Thresholds:")
					cat("\n\t",numSn.Removed.aveRT)
					cat("\n\nSummary of Subjects Removed because of Average RT Threshold:\n")
					print(sn.removed.RT.sum)
					cat("\n\nN Subjects Remaining:")
					cat("\n\t",total.rawsubs - numSn.Removed.aveRT)
				sink(NULL)
			}

			return(data)
}
