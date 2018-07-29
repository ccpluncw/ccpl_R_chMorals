#' A function to plot multiple model fits on one graph
#'
#' This function plots multiple model fits on one graph.
#' @param df.models a models list containing: a dfIndex, created by ch.subsetDFbyGroups() and a series of model fits output by ch.getLmModel() or ch.getPhitModel();
#' @param xxyCols a vector of 1 or 2 strings that specifies the names of the column in "dfIndex" that contains the grouping variables (up to two).  This is required.
#' @param xCol a string that specifies the name of the column in "data" that contains the predictor (x) variables for the fit specified in the models.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param filenameID a string that will be put on all filenames when the plots are saved. DEFAULT = NULL (no file saved)
#' @keywords morals group model fit plots
#' @return .
#' @export
#' @examples ch.moralsXXYPlotFitsByGrp(df.models, "scenarioType", "agent", "overlapRound", moralsData)


ch.moralsPlotFitsByGrps <- function (df.models, xxyCols, xCol, data, filenameID = NULL) {

  op <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1)

    dfIndex <- df.models$dfIndex
    grpCols <- names(dfIndex)
    #get Number of groups
    if(grpCols[length(grpCols)]=="indexNum") {
      nGrps <- length(grpCols) - 1
    } else {
      nGrps <- length(grpCols)
    }

    #if there are more than 2 groups, the extra groups need to be split up
    if (nGrps > 2) {
      #create a new dfIndex that removes the 2 plotting groups -xxyCols- so that each row contains the combination of the levels of the remaining groups. These are the ones that will be plotted in separate graphs
      dfIndexRemaining <- dfIndex[ , !(names(dfIndex) %in% xxyCols)]
      if(names(dfIndexRemaining[length(dfIndexRemaining)])=="indexNum") {
        dfIndexRemaining <- unique(dfIndexRemaining[-length(dfIndexRemaining)])
      }

      #how many remaining groups are there and what are there names
      nCondsRemaining <- nrow(dfIndexRemaining)
      grpsNamesRemaining <- names(dfIndexRemaining)

      #split up the two grouping variable so they can be put in ch.moralsXXYPlotFitsByGrp()
      xxy1 <- xxyCols[1]
      if(length(xxyCols) == 1) {
        xxy2 <- NULL
      } else {
        xxy2 <- xxyCols[2]
      }

      #for each row in dfIndexRemaining (the graphs that are to be plotted separately)
      #create the subset expression for the specific condition to pass to ch.moralsXXYPlotFitsByGrp()
      #mirror that in a string to be passed to ch.moralsXXYPlotFitsByGrp()
      subsetC <- NULL
      condString <- NULL
      for(i in 1:nCondsRemaining) {
        subsetC <- bquote( .(as.name(grpsNamesRemaining[1])) == .(dfIndexRemaining[[grpsNamesRemaining[1]]][i]))
        condString <- paste(grpsNamesRemaining[1], "=", dfIndexRemaining[[grpsNamesRemaining[1]]][i])
        if(length(grpsNamesRemaining) > 1) {
          for(j in 2:length(grpsNamesRemaining)) {
            subsetC <- bquote( .(subsetC) & .(as.name(grpsNamesRemaining[j])) == .(dfIndexRemaining[[grpsNamesRemaining[j]]][i]))
            condString <- paste(condString, "&", grpsNamesRemaining[j], "==", dfIndexRemaining[[grpsNamesRemaining[j]]][i])
          }
        }

        if(!is.null(filenameID)) {
          #create a filenameID that contains the condition and plot
          filenameID = paste(filenameID, condString)
        }
        ch.moralsXXYPlotFitsByGrp(df.models, grp1Col = xxy1, grp2Col = xxy2, xCol, data, subsetCond = subsetC, subsetString = condString, filenameID = filenameID)
      }

    } else {
      #if there are only two variables, then just plot those.
      xxy1 <- xxyCols[1]
      if(length(xxyCols) == 1) {
        xxy2 <- NULL
      } else {
        xxy2 <- xxyCols[2]
      }
      ch.moralsXXYPlotFitsByGrp(df.models, grp1Col = xxy1, grp2Col = xxy2, xCol, data, filenameID = filenameID)
    }

  par(op)
}
