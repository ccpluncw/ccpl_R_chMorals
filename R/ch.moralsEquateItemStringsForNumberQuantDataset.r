#' A function to equate the probe 1 and probe 2 strings when quantity varies.
#'
#' This function equates the probe 1 and probe 2 strings when quantity varies.
#' @param data morals dataframe.
#' @param probe1Col a string the specifies the column name in "data" that contains the first probe item.
#' @param probe2Col a string the specifies the column name in "data" that contains the second probe item.
#' @return the dataframe (data) with the four new columns: "p1Quant" and "p1Quant" (the quantities of probe 1 and 2 respectively) & "p1" and "p2" (the singular, equalized names of probe 1 and 2 respoectively).
#' @keywords morals quantity probes equate
#' @export
#' @examples ch.moralsEquateItemStringsForNumberQuantDataset (analysisReadyData.gp, "probe1", "probe2")

ch.moralsEquateItemStringsForNumberQuantDataset <- function (data, probe1Col, probe2Col) {

		# separate the quantity from the probe
		prb1 <- reshape2::colsplit(data[[probe1Col]]," ",c("p1Quant","p1"))
		prb2 <- reshape2::colsplit(data[[probe2Col]]," ",c("p2Quant","p2"))

		# make all the quantities a number
		prb1$p1Quant <- ifelse(prb1$p1Quant == "a" | prb1$p1Quant == "an", 1, prb1$p1Quant)
		prb2$p2Quant <- ifelse(prb2$p2Quant == "a" | prb2$p2Quant == "an", 1, prb2$p2Quant)
		prb1$p1Quant <-as.numeric(prb1$p1Quant)
		prb2$p2Quant <-as.numeric(prb2$p2Quant)
		### Match strings ###

		#trim whitespace
		prb1$p1 <- trimws (prb1$p1)
		prb2$p2 <- trimws (prb2$p2)

		#match singular and plural item names
		allSingular <- unique(prb1[prb1$p1Quant==1,'p1'])
		allMult <- unique(prb1[prb1$p1Quant!=1,'p1'])

		finalItems <- matrix(nrow=length(allSingular),ncol=2)
		for (i in 1:length(allSingular)) {
			bestMatch <- stringdist::stringdist(allSingular[i],allMult[1], method=c("lv"))
			bestIndex <- 1
			for (j in 2:length(allMult)) {
				if (stringdist::stringdist(allSingular[i],allMult[j], method=c("lv")) < bestMatch) {
					bestIndex <- j
					bestMatch <- stringdist::stringdist(allSingular[i],allMult[j], method=c("lv"))
				}
			}
			finalItems[i, 1]<-allSingular[i]
			finalItems[i, 2]<-allMult[bestIndex]
	}

	#recode to the singular form
	for (i in 1:length(finalItems[,1])) {
		prb1$p1[prb1$p1==finalItems[i,2]] <- finalItems[i,1]
		prb2$p2[prb2$p2==finalItems[i,2]] <- finalItems[i,1]
	}

	#merge the dataset
	data <- cbind(data, prb1,prb2)

	data$p1Quant <- as.numeric(data$p1Quant)
	data$p2Quant <- as.numeric(data$p2Quant)

	return(data)

}
