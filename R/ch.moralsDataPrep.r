#' A function to prepare the morals data for analysis
#'
#' This function prepares the morals data for analysis by creating overlapRound, correct, and a few other columns.  It also makes sure the trials starts at 1, and computes average p(hit) and RT for each subject.
#' @param data morals dataframe.
#' @param snCol a string that specifies the name of the column in "data" that contains the subject number.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param overlapCol a string that specifies the name of the column in "data" that contains the overlaps for the item in each trial.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction of the overlap for each trial.
#' @param trialCol a string that specifies the name of the column in "data" that contains the trial number.
#' @param yesNoCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param yesNoVal a vector of two values that specifies the yes "take action" value (index 1) and the no "take no action" value (index 2).
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @keywords morals data prep
#' @return a dataframe of prepared data.  It also writes the data to prepDataOutFile (specified in params) which will be used by other functions.
#' @export
#' @examples ch.moralsDataPrep (data=moralsData, "sn", "RT", "overlap", "direction", "trials", "respDef", yesNoVal = c("Yes", "No"), params=parameters)

ch.moralsDataPrep  <- function (data, snCol, RTcol, overlapCol, directionCol, trialCol, yesNoCol, yesNoVal = c("Yes", "No"), params) {

######_____PACKAGES NEEDED FOR CODE______######
	library(plyr)
	library(dplyr)
	library(rowr)
	library(chutils)
  library(chMorals)

	mainDir <- getwd()


# DATA PREPERATION

	######_____	READ IN  RAW DATA and FILES WITH OVERLAP VALUES AND PROBE INFORMATION_____######

	dat.over <-read.table(params$valueOverlapDataFile, sep="\t", header=T)
	drops <- scan(params$dropVarFile, what="a")

	#### remove subjects who had computer troubles or did not comply with the cell phone use instructions
	data <- ch.removeBadSNs(data, snCol, params$removeBadSNFile)

	#### only keep data in which the participant responds yes or no. Get rid of bad button presses
		data <- data[data[[yesNoCol]]==yesNoVal[1] | data[[yesNoCol]]==yesNoVal[2], ]

		#add 1 to trials if they start at 0
		if (min(data[[trialCol]])==0) {
			data[[trialCol]] <- data[[trialCol]] + 1
		}

	######_____CALCULATE MEAN AND STANDARD DEVIATION OF RT BY SUBJECT_____######

		sub.avgRT <- as.data.frame(data %>% group_by_(snCol) %>% summarise (avgRT = mean(eval(parse(text=RTcol))),sdRT=sd(eval(parse(text=RTcol)))))

		#fill the group vectors
		groupA <- seq(params$minGroupAnum, params$maxGroupAnum, 1)
		groupB <- seq(params$minGroupBnum, params$maxGroupBnum, 1)

		#get all combinations that produce the same outcomes
		xCol <- ch.stringSeq ("Item", groupA)
		xColPermA <- matrix(xCol[ch.permute(length(groupA))], ncol=length(groupA))

		xCol <- ch.stringSeq ("Item", groupB)
		xColPermB <- matrix(xCol[ch.permute(length(groupB))], ncol=length(groupB))

		xTot1 <- NULL
		for (i in 1:nrow(xColPermA)) {
			xTmp <- cbind.fill(t(xColPermA[i,]), xColPermB)
			xTot1 <- ch.rbind(xTot1, xTmp)
		}

		#revere order for reversed direction
		xTot2 <- xTot1[,ncol(xTot1):1]

		#now get y vector: it only needs one order because all variations were in xTot1 and 2
		yTot1 <- ch.stringSeq ("IA", seq(1,length(groupA),1))
		yTot2 <- ch.stringSeq ("IB", seq(1,length(groupB),1))
		yTot <- c(yTot1, yTot2)

		dt.merged.d <- NULL

		#may need to convert all strings to lower case (tolower) to match appropriately

		for(j in 1:2) {
			if (j==1) {
				#first run for xTot1
				xCol <- data.frame(lapply(xTot1, as.character), stringsAsFactors=FALSE)
			} else {
				#next run for xTot2 (reverse order of items) and reverse "direction"
				xCol <- data.frame(lapply(xTot2, as.character), stringsAsFactors=FALSE)
			}
			for(i in 1:nrow(xCol)) {
					dt.merged.a <-merge(data, dat.over, by.x=c(unlist(xCol[i,])), by.y=c(unlist(yTot)))
					if(j == 1) {
						dt.merged.a$direct.xVy <- dt.merged.a[[directionCol]]
					} else {
						#	change the direction of choice  because we reversed the order xTot2
						dt.merged.a$direct.xVy <-ifelse(dt.merged.a[[directionCol]]==1,-1,1)
					}
					#round dt.merged$overlap
					dt.merged.a$overlapRound <- round_any(dt.merged.a[[overlapCol]], params$roundThreshold, params$roundDirection)
					#create the Correct Response column. This is necessary to calculate "percentHit", "freq.pred"
					#Here, correct == 1 indicates the person chose the item with the highest value
					dt.merged.a$correct <- ifelse(dt.merged.a$direct.xVy==-1 & dt.merged.a[[yesNoCol]]==yesNoVal[2], 1, ifelse(dt.merged.a$direct.xVy==1 & dt.merged.a[[yesNoCol]]==yesNoVal[1], 1, 0))
					#merge it all into a big dataset and remove some extraneous columns
					dt.merged.d <- ch.rbind(dt.merged.d, dt.merged.a)
			}
		}

		#remove duplicate rows that got inserted when the same item is part of a pair in groupA or groupB
		dt.merged.d <- unique(dt.merged.d)

		#remove extra columns specified in dropVarFile
		dt.merged.d <- dt.merged.d[ , !(names(dt.merged.d) %in% drops)]

		#add average rt data
		dt.merged <- merge(dt.merged.d, sub.avgRT)

		#add a target present column: dt.merged$direct.xVy == 1 indicated that the item with the higher value will be
		#killed if nothing is done.  Therefore, according to SVT, the participant should identify it as a target and should act.
		dt.merged$targetPresent <- ifelse(dt.merged$direct.xVy == 1, TRUE, FALSE)
		#add a directional overlap column.  Here, dirOverlap = 0 if the distributions overlap completely; 1 if the value of Item1 is greater than Item 2; -1 if the value of Item 1 is less than Item 2.
		dt.merged$dirOverlap <- abs(dt.merged$overlap-1)*dt.merged$direct.xVy

		dataOutFile <- file.path(mainDir,params$prepDataOutFile)
		write.table(dt.merged, file=dataOutFile, quote=F, sep="\t", row.names=F)
		return (dt.merged)
}
