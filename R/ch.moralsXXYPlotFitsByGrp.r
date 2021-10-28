#' A function to plot multiple model fits on one graph
#'
#' This function plots multiple model fits on one graph. The model fits should have one DV, one predictor variable (xCol), and one or two grouping variables (grp1Col & grp2Col)
#' @param df.models a models list containing: a dfIndex, created by ch.subsetDFbyGroups() and a series of model fits output by ch.getLmModel() or ch.getPhitModel();
#' @param grp1Col a string that specifies the name of the column in "dfIndex" that contains one grouping variable.  This is required.
#' @param grp2Col a string that specifies the name of the column in "dfIndex" that contains another grouping variable.  This is optional.  A max of two grouping variables is allowed
#' @param xCol a string that specifies the name of the column in "data" that contains the predictor (x) variables for the fit specified in the models.
#' @param subsetCond an expression that is used to subset df.models$dfIndex to include only the group conditions that you want to graph (e.g., cond1 == "pretest" & sn == "4").
#' @param subsetString an string that mirrors the subsetCond (e.g., "cond1 == pretest & sn == 4"). This is used for titling the graph.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param addLegend a boolean that specifies whether a legend should be added to the graphs. DEFAULT = T
#' @param filenameID a string that will be put on all filenames when the plots are saved. DEFAULT = NULL (no file saved)
#' @keywords morals group model fit plots
#' @return .
#' @export
#' @examples ch.moralsXXYPlotFitsByGrp(df.models, "scenarioType", "agent", "overlapRound", moralsData)

ch.moralsXXYPlotFitsByGrp <- function (df.models, grp1Col, grp2Col = NULL, xCol, data, subsetCond = NULL, subsetString = NULL, addLegend = T, filenameID = NULL) {

  op <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(6,7,5,8), las=1)

  x <- unique(data[[xCol]])
  y <- rep(0, length(x))

  #get x axis limits
  minX <- floor(min(x))
  maxX <- ceiling(max(x))
  xLims <- c(minX, maxX)

  if(!is.null(subsetString)) {
    titlePrefix <- subsetString
  } else {
    titlePrefix <- ""
  }

  #get model names and remove dfIndex from the names
  modelNames <- names(df.models)
  if(modelNames[1]=="dfIndex") {
    modelNames <- modelNames[-1]
  }
  numModels <- length(modelNames)

  #get group names and determine whether there is one or two
  grpNames <- NULL
  if (is.null(grp1Col)) {
    stop("must have at least 1 group")
  } else {
    grpNames[1] <- grp1Col
    if (!is.null(grp2Col)) {
      grpNames[2] <- grp2Col
    }
  }
  numGroups <- length(grpNames)
  #get the unique conditions based on the groups to be plotted
  if(!is.null(subsetCond)) {
    dfIndex <- subset(df.models$dfIndex, eval(subsetCond))
    dfIndex <- dfIndex[ , names(dfIndex) %in% c(grpNames, "indexNum")]
  } else {
    dfIndex <- df.models$dfIndex
  }
  numConds <- nrow(dfIndex)

  #get the line attributes for each condition
  df.legend <- ch.getPlotLegendVals(dfIndex[1:length(dfIndex)-1])

  for(j in 1:numModels) {
    yLab <- df.models[[modelNames[j]]] [[dfIndex$indexNum[1]]]$yLab
    xLab <- df.models[[modelNames[j]]] [[dfIndex$indexNum[1]]]$xLab
    #create a temporary dataframe with the name of the x variable in the formula
    xVar <- df.models[[modelNames[j]]] [[dfIndex$indexNum[1]]]$vars[1]
    df.tmp <- setNames(data.frame(x), xVar)

    if(df.models[[modelNames[j]]] [[dfIndex$indexNum[1]]]$modelType == "lm") {

      #get y-axis bounds
      allYs <- NULL
      for(i in 1:numConds) {
        model <- df.models[[modelNames[j]]][[dfIndex$indexNum[i]]]$model
        #add the y values to the temporary dataframe
        if(!is.na(model)) {
          df.tmp$y <- with(df.tmp, eval(model))
        } else {
          df.tmp$y <- NA
        }
        allYs <- c(allYs,df.tmp$y)
      }
      minMax <- ch.getPlotAxisMinMax(allYs)

      #create empty plot using the x and y above
      plot(y ~ x, main=paste(titlePrefix, modelNames[j]), xlab= xLab, ylab=NA, type="l",  col="white", lwd=.75, ylim = minMax, xlim = xLims, cex.lab = 1)
      mtext(side=2,yLab, line=3, cex = 1)
    }
    if(df.models[[modelNames[j]]] [[dfIndex$indexNum[1]]]$modelType == "pHit") {
      #create empty plot
      plot(y ~ x, main=paste(titlePrefix, modelNames[j]), xlab= xLab, ylab=NA, type="l",  col="white", lwd=.75, ylim = c(0,1), xlim = xLims, cex.lab = 1)
      mtext(side=2,yLab, line=3, cex = 1)
      abline(a=0.5,b=0,col="grey", lwd=2)
    }

    for(i in 1:numConds) {
      if(!is.na(df.models[[modelNames[j]]][[dfIndex$indexNum[i]]]$model)) {
        #create a temporary dataframe with the name of the x variable in the formula
        xVar <- df.models[[modelNames[j]]] [[dfIndex$indexNum[i]]]$vars[1]
        df.tmp <- setNames(data.frame(x), xVar)
        #get y values
        model <- df.models[[modelNames[j]]][[dfIndex$indexNum[i]]]$model
        df.tmp$y <- with(df.tmp, eval(model))

        #get Condition and line attributes
        if(numGroups == 1) {
          tmpLgnd <- df.legend[df.legend[[grp1Col]]==dfIndex[[grp1Col]][i],]
        } else {
          tmpLgnd <- df.legend[df.legend[[grp1Col]]==dfIndex[[grp1Col]][i] & df.legend[[grp2Col]]==dfIndex[[grp2Col]][i],]
        }
        #sort dataframe and plot line
        df.tmp <- df.tmp[order(df.tmp[[xVar]]),]
        lines(df.tmp$y ~ df.tmp[[xVar]], lty =as.character(tmpLgnd$lty),  col=hsv(tmpLgnd$h,tmpLgnd$s, tmpLgnd$v))
      }
    }

    #add legend
    if(addLegend) {
      ch.addLegend(df.legend, c(grp1Col, grp2Col))
    }

    #save file
    if (!is.null(filenameID)) {
      grps <- paste(grp1Col, grp2Col, sep="-")
      filename <-paste(filenameID,modelNames[j],grps,".pdf")
      dev.copy(pdf, filename, width=8, height=8)
      dev.off();
    }

  }
  par(op)

}
