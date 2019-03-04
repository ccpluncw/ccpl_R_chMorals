#' A function to graph the response choice pattern per item in the morals experiment
#'
#' This function graphs the response choice pattern per item in the morals experiment.
#' @param data morals dataframe.
#' @param item1Col a string that specifies the name of the column in "data" that contains one of the items in the morals task.
#' @param item2Col a string that specifies the name of the column in "data" that contains the comparison item in the morals task.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param dirOverlapCol a string that specifies the name of the column in "data" that contains the directional Overlap for the item in each trial.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction of the overlap for each trial.
#' @param respChoiceCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param respChoiceVal a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants) value (index 2).
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param saveFigures a boolean to state whether to save the figures
#' @param comparisonItemName a string that states the exact item that you want to compare to all the other items.  If this parameter is used, then only one item will be compared to all the others.  Otherwise, all items will be compared to each other. Default comparisonItemName = NULL.
#' @keywords morals item analysis choice
#' @return the dataset used to create the graphs.  Because this dataset doubles the actual data (all A-B item combinations are duplicated to be B-A combinations as well), do not use it for analysis unless you keep this in mind. The duplication is needed because a graph showing the responses to Item A, needs to have all Item A responses, regardless of order.  It is expected that the graph showing responses to Item B will include some of Item A when there is an Item A-Item B combination.
#' @export
#' @examples ch.moralsItemChoiceAnalysis (analysisReadyData.gp, "Item1", "Item2", "overlapRound", "dirOverlap","keyDef", respChoiceVal = c("Yes", "No"), params, printFigures = T)


ch.moralsItemChoiceAnalysis <- function (data, item1Col, item2Col, overlapRoundCol, dirOverlapCol,respChoiceCol, respChoiceVal = c("Item1", "Item2"), params, saveFigures = T, comparisonItemName = NULL) {

##### Functions ######

		#create new directories
		mainDir <- getwd()

		ch.newDir (mainDir, params$itemSubDir)
		itemDir <- getwd()
		setwd(mainDir)

# DATA PREPERATION
	#if there is no comparisonItemName, then compare all items to one another
	if(is.null(comparisonItemName) == TRUE) {
		items <-unique(unique(data[[item1Col]]), unique(data[[item2Col]]))
	} else {
		#else only compare the one item
		items <-comparisonItemName
	}
	itemOutData <- NULL
	op <- par(mfrow=c(2,1), omi=c(1,.25,.25,.25))
  # plots for each item
  		for (j in items)  {
				tmp1 <- data[data[[item1Col]]==j, ]
				if(length(tmp1[[item1Col]]) > 0) {
					tmp1$probeOrder <- "original"
					tmp1$chose <- ifelse(tmp1[[respChoiceCol]]==respChoiceVal[1], 1, 0)
				}
				# #find all the item_i in probe 2 and put it in probe 1
				# #this way we are finding all instances of item_i
  		  tmp2 <- data[data[[item2Col]]==j, ]
				if(length(tmp2[[item2Col]]) > 0) {
	  		  tmp2[[dirOverlapCol]] <- -1*tmp2[[dirOverlapCol]]
					tmp2$ItemTmp <- tmp2[[item2Col]]
	  		  tmp2[[item2Col]] <- tmp2[[item1Col]]
	  		  tmp2[[item1Col]] <- tmp2$ItemTmp
					tmp2$ItemTmp <- NULL
					tmp2$probeOrder <- "reversed"
					tmp2$chose <- ifelse(tmp2[[respChoiceCol]]==respChoiceVal[2], 1, 0)
				}
				temp.merge <-rbind(tmp1,tmp2)
				#even though there are duplicates in temp.merge (reverse order duplicates), they do not affect the following call because
				#we are calculating means by probe1 first, so the duplicates are necessary to get all the data recorded.

				table.hold <- as.data.frame(temp.merge %>% dplyr::group_by_(overlapRoundCol, item1Col, item2Col) %>% dplyr::summarise(N = length(chose), pctChosen=mean(chose), mDirOverlap= mean(eval(parse(text=dirOverlapCol))) ) )
  		  table.hold <-droplevels(table.hold)

  			levels(table.hold[[item2Col]])[which(levels(table.hold[[item2Col]])=="an adult with a deadly contagious disease")] <- "an adult w/ DCD*"

				itemOutData <- ch.rbind(itemOutData, temp.merge)

				par(mfrow=c(2,1), omi=c(1,.25,.25,.25))
  		  stripchart(table.hold$pctChosen ~ reorder(table.hold[[item2Col]], table.hold$mDirOverlap), vertical= T, las=2, pch=16, main= paste0("Probability of Choosing ",ch.capwords(j) , " by Probe"), ylab= paste0("p(", j, ")"), ylim=c(0,1))
  		  stripchart(table.hold$N ~ reorder(table.hold[[item2Col]], table.hold$mDirOverlap), vertical= T, las=2, pch=16, ylab="Number of Trials")

				if (saveFigures) {
  		 		dev.copy(pdf, file.path(itemDir,paste0(params$dt.set, "Choosing ", ch.capwords(j),".pdf")), width=12, height=9)
  		 		dev.off();
				}
  		}

			par(op)

		return(itemOutData)

}
