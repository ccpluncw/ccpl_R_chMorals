#' A function to fit and plot a model whereby p(hit)quantity (that is, where the correct answer is the Item with the greatest quantity) is predicted by the overlap of value.
#'
#' This function fits and plots a model whereby p(hit)quantity (that is, where the correct answer is the Item with the greatest quantity) is predicted by the overlap of value. This model is used to show that values values can predict p(Hit)quantity when values overlap is considered.  The model is as follows:
#' p(Hit)quantity = a + b1*(-1*sType) + b2*(sType*OverlapRound),
#' where sType ==1 if targetPresent(quantity) is inconsistent with targetPresent(value), and -1 if it is consistent. OverlapRound is the overlap of the values distributions.
#' @param data a morals dataframe.
#' @param OvrlpQuantConsistentCol a string the specifies the column name in "data" that contains the a boolean that specifies whether targetPresent(quantity) is consistent with targetPresent(value) (TRUE) or not (FALSE).
#' @param itemQuantDiffCol a string the specifies the column name in "data" that contains the absolute value of the quantity difference between Item1 and Item2.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlap column.
#' @param pHitQCol a string that specifies the name of the column in "data" that contains a "1" if the participant chose the item with the greatest quantity and a "0" if they did not.
#' @param minN an integer indicating the minimum number of trials in each condition that is valid for analysis.  Below that number, the condition will be removed. DEFAULT = NULL.
#' @param filename the filename (pdf) to save the output of the graph. DEFAULT = NULL (no graph saved)
#' @param lgndPlacement a string that specified the placement of the legend: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and "center". DEFAULT = "topright"
#' @param cex1 the relative font size for the y-axis label. DEFAULT = 1.
#' @param cexLegend the relative font size for the legend. DEFAULT = .75.
#' @param ylim the limits of the y axis. DEFAULT = c(0,1).
#' @param yLab y axis label. DEFAULT = "p(HitQuant)".
#' @param xLab x axis label. DEFAULT = "Overlap".
#' @param parOp the parameter list to enter into par.  DEFAULT = NULL
#' @return the lm model fit.
#' @keywords morals quantity overlap model fit
#' @export
#' @examples ch.moralsQuantAnalyis (analysisReadyData.gp, "probe1", "probe2")


ch.moralsModelOvrlpPredPhitQ <- function (data, OvrlpQuantConsistentCol, itemQuantDiffCol, overlapRoundCol, pHitQCol, minN = NULL, filename = NULL, grpLgndNames = NULL, lgndPlacement = "topright", cex1 = 1,cexLegend = 0.75, ylim=c(0,1), yLab = "p(HitQuant)", xlab = "Overlap", parOp = NULL, ...) {

  #set par if they are not input
  if(is.null(parOp)) {
    op <- par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,8,4,12), las=1, xpd=T)
  }

  #make sure all the data is complete and then get averages
  data <- data[complete.cases(data),]
  df.tmp <- as.data.frame(data %>% dplyr::group_by_(OvrlpQuantConsistentCol, itemQuantDiffCol) %>% dplyr::summarise(
    N = length(eval(parse(text = pHitQCol))),
    percentHit = mean(eval(parse(text = pHitQCol))),
    meanOv =  mean(eval(parse(text = overlapRoundCol))),
  ) )

  #remove conditions with too few points
  if (!is.null(minN)) {
    df.tmp <- df.tmp[df.tmp$N > minN,]
  }
  df.tmp <- droplevels(df.tmp)

  #create variables necessary for the model
  df.tmp$sType <- ifelse(df.tmp[[OvrlpQuantConsistentCol]]==FALSE, 1, -1)
  df.tmp$sTypeOv <- df.tmp$sType*df.tmp$meanOv
  df.tmp$negStype <- -1*df.tmp$sType

  #run the model
  modelFit <- lm(percentHit ~ 1 + negStype + sTypeOv, data=df.tmp)

  ##### plot data and model prediction
  #get the groups and add "model to the groups"
  grps <- unique(df.tmp[[OvrlpQuantConsistentCol]])
  grps <- c(grps, "model")
  nGrps <- length(grps)
  #get colors for plotting
  hsvCols <- ch.getHSVcolors(nGrps)
  #reverse the order so that black will be used for "model"
  hsvCols<-hsvCols[dim(hsvCols)[1]:1,]

  #plot the groups and model
  xLims <- ch.getPlotAxisMinMax(df.tmp$meanOv)
  with(df.tmp[df.tmp[[OvrlpQuantConsistentCol]] == grps[1], ], plot(meanOv, percentHit,  ylab = NA, xlab = expression(paste("", Psi,"(value) Distributional overlap", sep="")), ylim = ylim, xlim=xLims, cex = cex1, pch=16, col=hsv(hsvCols$h[1], hsvCols$s[1], hsvCols$v[1]), ...))
  with(df.tmp[df.tmp[[OvrlpQuantConsistentCol]] == grps[2], ], points(meanOv, percentHit, cex = cex1, pch=16, col=hsv(hsvCols$h[2], hsvCols$s[2], hsvCols$v[2]), ...))
  with(df.tmp, points(meanOv, predict(modelFit), cex = cex1, pch=16, col=hsv(hsvCols$h[3], hsvCols$s[3], hsvCols$v[3]),... ))
  mtext(side=2,expression('p(Hit)'["quant"]), line=3, cex = cex1)

  #create and add the legend
  lNames <- NULL
  if(!is.null(grpLgndNames) & (length(grpLgndNames) == length(grps))) {
    lNames <- grpLgndNames
  } else {
    for(i in 1:(nGrps-1)) {
      lNames[i] <- paste(OvrlpQuantConsistentCol, "=", grps[i])
    }
    lNames[nGrps] <- "Model"
  }
  legend(lgndPlacement, legend=lNames, pch=16, col=hsv(hsvCols$h,hsvCols$s,hsvCols$v), bty="n", cex=cexLegend, inset = c(-.6,0), x.intersp = .35,y.intersp = 1)

  #print and reset the par options
  if(!is.null(filename)) {
    dev.copy(pdf, filename, width=12, height=9)
    dev.off();
  }

  if(is.null(parOp)) {
    par(op)
    par(xpd=F)
  }

  #return the fit
  return(modelFit)

}
