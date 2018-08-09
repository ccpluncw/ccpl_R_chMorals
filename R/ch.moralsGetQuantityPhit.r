#' A function to get the predicted p(hit)Quant: that is, p(hit)==TRUE if the participant chooses the item with the greatest quantity.
#'
#' This function calculates the predicted p(hit)Quant: that is, p(hit)==TRUE if the participant chooses the item with the greatest quantity.
#' @param data a morals dataframe.
#' @param p1QuantCol a string the specifies the column name in "data" that contains the numerical quantity assocaited with Item1.
#' @param p2QuantCol a string the specifies the column name in "data" that contains the numerical quantity assocaited with Item2.
#' @param respChoiceCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param respChoiceVal a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants) value (index 2).
#' @param targetPresentCol a string that specifies the name of the column in "data" that contains the whether or not the target was presented.
#' @param targetPresentVals a vector of two values that specifies the "target present" value (index 1) and the "target absent" value (index 2). e.g, c("yes", "no")
#' @return the input dataframe with three new columns: "pHitQ" which contains a "1" if the participant chose the item with the greatest quantity and a "0" if they did not; and "OvrlpQuantConsistent" which contains a TRUE if the value and quantity of item1 vs item2 make the same prediction and a FALSE if they do not; and "targetPresentQ" contains a TRUE if the  quantity of item1 > item2 so the participant should act and a FALSE if not; and "qDiff" which is the absolute value of the quantity difference between the items.
#' @keywords morals quantity p(Hit)q p(Hit)quant
#' @export
#' @examples ch.moralsGetDirOverlapByGrp (overlapData, "probe1", "p1Quant", "dirOverlap")

ch.moralsGetQuantityPhit <- function (data, p1QuantCol, p2QuantCol, respChoiceCol, respChoiceVal = c("Item1", "Item2"), targetPresent, targetPresentVals=c(TRUE,FALSE)) {

      #create the prediction column based on Quantity (this is an alternate hypothesis).
      #This is necessary to calculate "percentHit", "freq.pred"
      #Here, predQ == 1 indicates a hit.  That is, the person chose the item with the highest quantity
      #This only works if the two quantities are different
      #if p1Quant > p2Quant & p1 is valued less & decided wrong (then chose p1 consistent with quantity (but inconsistent with value))
      #etc.

    data$targetPresentQ <- ifelse(data[[p1QuantCol]] > data[[p2QuantCol]], targetPresentVals[1],targetPresentVals[2])
    data$targetPresentQ <- ifelse(data[[p1QuantCol]] == data[[p2QuantCol]], NA, data$targetPresentQ)

    data$pHitQ <- ifelse( ((data[[p1QuantCol]] > data[[p2QuantCol]]) & (data[[respChoiceCol]] == respChoiceVal[1])) | ((data[[p2QuantCol]] > data[[p1QuantCol]]) & (data[[respChoiceCol]] == respChoiceVal[2])), 1,0)
    data$pHitQ <- ifelse(data[[p1QuantCol]] == data[[p2QuantCol]], NA, data$pHitQ)

    data$OvrlpQuantConsistent <- ifelse(data$targetPresentQ == data[[targetPresent]], TRUE,FALSE)
    data$OvrlpQuantConsistent <- ifelse(data[[p1QuantCol]] == data[[p2QuantCol]], NA, data$OvrlpQuantConsistent)

    data$qDiff <- abs(data[[p1QuantCol]]-data[[p2QuantCol]])

    return(data)
}
