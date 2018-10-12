#' A function to analyze the quantity variation in morals data.
#'
#' This function does the following: (1) ch.moralsQuantsToGrps(); (2) ch.moralsGetQuantityPhit (); (3) plots the linear fits for p(Hit)quantity = abs(quantity difference between Item1 and Item2); (4) ch.moralsModelOvrlpPredPhitQ()
#' @param data a morals dataframe.
#' @param p1QuantCol a string the specifies the column name in "data" that contains the numerical quantity assocaited with Item1.
#' @param p2QuantCol a string the specifies the column name in "data" that contains the numerical quantity assocaited with Item2.
#' @param quantValueCuts a vector of numbers that specifies the cutoff values for each quantity.  The first category is <= to the first cutoff value; each successive category (c1, c2, etc) is c1 < X <= c2; the final category is greater than the last number in the vector.
#' @param respChoiceCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param respChoiceVal a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants) value (index 2).
#' @param targetPresentCol a string that specifies the name of the column in "data" that contains the whether or not the target was presented.
#' @param targetPresentVals a vector of two values that specifies the "target present" value (index 1) and the "target absent" value (index 2). e.g, c("yes", "no")
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @return the input dataframe with 10 new columns: "p1Quant" and "p1Quant" (the quantities of probe 1 and 2 respectively); "p1" and "p2" (the singular, equalized names of probe 1 and 2 respoectively); and "p1GrpSize" and "p2GrpSize" (the group categories based on quantValueCuts for probe 1 and 2 respectively); "pHitQ" which contains a "1" if the participant chose the item with the greatest quantity and a "0" if they did not; and "OvrlpQuantConsistent" which contains a TRUE if the value and quantity of item1 vs item2 make the same prediction and a FALSE if they do not; and "targetPresentQ" contains a TRUE if the  quantity of item1 > item2 so the participant should act and a FALSE if not; and "qDiff" which is the absolute value of the quantity difference between the items.
#' @keywords morals quantity analysis
#' @export
#' @examples ch.moralsQuantAnalyis (moralsData, "Item1", "Item2", c(1,10,40), "keyDef", respChoiceVal = c("Yes", "No"),"targetPresent", c(TRUE, FALSE), "overlapRound", params)

ch.moralsQuantAnalyis <- function (data, probe1Col, probe2Col, quantValueCuts, respChoiceCol, respChoiceVal = c("Item1", "Item2"), targetPresentCol, targetPresentVals, overlapRoundCol, params) {

  #set up item directory, where the graphs will be stored
  mainDir <- getwd()

  ch.newDir (mainDir, params$itemSubDir)
  itemDir <- getwd()
  setwd(mainDir)

  #get the reaction time label
  ReactionTime <- ch.getMoralsRTaxisName(params$keybRTtransform, params$RTresid)

  #get output data filename
  statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))

  #equalize plural and singular strings; get quantitys; group quantites
  data <- ch.moralsQuantsToGrps(data, probe1Col, probe2Col, quantValueCuts)
  #calculate p(Hit)quantity, targetPresentQuantity, quantityDifference
  data.1 <- ch.moralsGetQuantityPhit(data, "p1Quant", "p2Quant", respChoiceCol,respChoiceVal, targetPresentCol, targetPresentVals)

  #plot and get fits for p(Hit)quant = quantityDifference
  filename = file.path(itemDir,paste(params$dt.set,"all pHitQ by Qdiff.pdf",sep=""))
  outList <- ch.plotTwoLinearFits(data.1, "qDiff", "res.RT", "pHitQ", y1Label = ReactionTime, y2Label = "p(Hit)u", xlab="Quantity Difference", minN = params$minOverlapN, ylimMax2 = 1, filename=filename)
  #plot and get fits for p(Hit)quant = a function of (overlap)
  filename = file.path(itemDir,paste(params$dt.set,"Overlap Model predicts pHitQ.pdf",sep=""))
  OtoPmodel.fit <- ch.moralsModelOvrlpPredPhitQ(data.1, "OvrlpQuantConsistent", "qDiff", overlapRoundCol, "pHitQ", minN = params$minOverlapN, filename=filename)

  #output data
  sink(statsOutputFile, append = T)
    cat("\n\n*********************** Quantity Analysis ***********************n\n")
    cat("\n\n**** RT = qDiff ****\n\n")
    print(summary(outList$y1Fit))
    cat("\n\n**** p(Hit)quant = qDiff ****\n\n")
    print(summary(outList$y2Fit))
    cat("\n\n**** p(Hit)quant = f(overlap) : SVT model ****\n\n")
    print(summary(OtoPmodel.fit))

  sink(NULL)

  return(data.1)

}
