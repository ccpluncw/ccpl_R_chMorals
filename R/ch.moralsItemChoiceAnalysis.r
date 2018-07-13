#' A function to graph the response choice pattern per item in the morals experiment
#'
#' This function graphs the response choice pattern per item in the morals experiment.
#' @param data morals dataframe.
#' @param item1Col a string that specifies the name of the column in "data" that contains one of the items in the morals task.
#' @param item2Col a string that specifies the name of the column in "data" that contains the comparison item in the morals task.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param dirOverlapCol a string that specifies the name of the column in "data" that contains the directional Overlap for the item in each trial.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction of the overlap for each trial.
#' @param yesNoCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param yesNoVal a vector of two values that specifies the yes "take action" value (index 1) and the no "take no action" value (index 2).
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param saveFigures a boolean to state whether to save the figures
#' @keywords morals item analysis choice
#' @return the dataset used to create the graphs.  Because this dataset doubles the actual data (all A-B item combinations are duplicated to be B-A combinations as well), do not use it for analysis unless you keep this in mind. The duplication is needed because a graph showing the responses to Item A, needs to have all Item A responses, regardless of order.  It is expected that the graph showing responses to Item B will include some of Item A when there is an Item A-Item B combination.
#' @export
#' @examples ch.moralsItemChoiceAnalysis (analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", yesNoVal = c("Yes", "No"), params, printFigures = T)


ch.moralsItemChoiceAnalysis <- function (data, item1Col, item2Col, overlapRoundCol, dirOverlapCol,yesNoCol, yesNoVal = c("Yes", "No"), params, saveFigures = T) {

######_____PACKAGES NEEDED FOR CODE______######
	library(dplyr)
	library(chutils)

##### Functions ######

		#create new directories
		mainDir <- getwd()

		ch.newDir (mainDir, params$itemSubDir)
		itemDir <- getwd()
		setwd(mainDir)

# DATA PREPERATION

	items <-unique(unique(data[[item1Col]]), unique(data[[item2Col]]))

	itemOutData <- NULL
  # plots for each item
  		for (j in items)  {

				#find all the item_i in probe 1
  		  tmp1 <- data[data[[item1Col]]==j, ]
  		  tmp1$probe1 <- tmp1[[item1Col]]
  		  tmp1$probe2 <- tmp1[[item2Col]]
  		  tmp1$chose <- ifelse(tmp1[[yesNoCol]]==yesNoVal[1], 1, 0)
				tmp1$probeOrder <- "original"

				#find all the item_i in probe 2 and put it in probe 1
				#this way we are finding all instances of item_i
  		  tmp2 <- data[data[[item2Col]]==j, ]
  		  tmp2[[dirOverlapCol]] <- -1*tmp2[[dirOverlapCol]]
  		  tmp2$chose <- ifelse(tmp2[[yesNoCol]]==yesNoVal[2], 1, 0)
  		  tmp2$probe2 <- tmp2[[item1Col]]
  		  tmp2$probe1 <- tmp2[[item2Col]]
				tmp2$probeOrder <- "reversed"

  		  temp.merge <-rbind(tmp1,tmp2)
				#even though there are duplicates in temp.merge (reverse order duplicates), they do not affect the following call because
				#we are calculating means by probe1 first, so the duplicates are necessary to get all the data recorded.

				table.hold <- as.data.frame(temp.merge %>% group_by_(overlapRoundCol, "probe1", "probe2") %>% summarise (N = length(chose), pctChosen=mean(chose), value.spVp= mean(eval(parse(text=dirOverlapCol))) ) )

  		  table.hold <-droplevels(table.hold)

  			levels(table.hold$probe2)[which(levels(table.hold$probe2)=="an adult with a deadly contagious disease")] <- "an adult w/ DCD*"

				itemOutData <- ch.rbind(itemOutData, temp.merge)

  		 	par(mfrow=c(2,1), omi=c(1,.25,.25,.25))
  		  stripchart(table.hold$pctChosen ~ reorder(table.hold$probe2, table.hold$value.spVp), vertical= T, las=2, pch=16, main= paste0("Probability of Choosing ",ch.capwords(j) , " by Probe"), ylab= paste0("p(", j, ")"), ylim=c(0,1))
  		  stripchart(table.hold$N ~ reorder(table.hold$probe2, table.hold$value.spVp), vertical= T, las=2, pch=16, ylab="Number of Trials")

				if (saveFigures) {
  		 		dev.copy(pdf, file.path(itemDir,paste0(params$dt.set, "Choosing ", ch.capwords(j),".pdf")), width=12, height=9)
  		 		dev.off();
				}
  		}

		return(itemOutData)

}
