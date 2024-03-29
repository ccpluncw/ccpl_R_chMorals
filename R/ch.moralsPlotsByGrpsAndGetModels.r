#' A function to analyze the Morals data by grouping variables
#'
#' This function analyzes p(No), p(Hit), RT, d prime, and beta by grouping variable for the morals data.
#' @param data the morals dataframe after running through ch.moralsDataPrep().
#' @param grpCols a vector of strings that specifies the names of the column in "data" that are the grouping variables.
#' @param RTcol a string that specifies the name of the column in "data" that contains the RT for each trial.
#' @param overlapRoundCol a string that specifies the name of the column in "data" that contains the overlapRound column.
#' @param yesNoCol a string the specifies the column name in "data" that contains the variable with the participant's yes/no response.
#' @param yesNoVal a vector of two values that specifies the yes "take action" value (index 1) and the no "take no action" value (index 2). e.g, c("yes", "no")
#' @param correctCol a string that specifies the name of the new column that will contains a "1" if the participant chose the item with the greatest value distribution and a "0" if they did not.
#' @param correctVals a vector of two values that specifies the "correct" value (index 1) and the "incorrect" value (index 2). e.g, c("yes", "no")
#' @param targetPresentCol a string that specifies the name of the column in "data" that contains the whether or not the target was presented.
#' @param targetPresentVals a vector of two values that specifies the "target present" value (index 1) and the "target absent" value (index 2). e.g, c("yes", "no")
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param minNperOverlap an integer that specifies the minimum number of trials necessary to include an overlap bin in the graph. DEFAULT = 0.
#' @param minUniqueOverlaps An integer specifying the minimum number of unique overlap bins necessary for the program to calculate the dPrime function.  DEFAULT = 3.
#' @param useTwoParameterModel A boolean that specifies whether to use a two parameter p(HOV) model.  If this is set to TRUE, then this function will fit a p(HVO) model whereby the rightmost point (overlap = 1.0) is not fixed at p(HVO) = 0.5. DEFAULT = FALSE.
#' @param savePlots a boolean that specifies whether to save plots. DEFAULT = T.
#' @keywords morals data analysis by grouping group variable
#' @return .
#' @export
#' @examples ch.moralsPlotsByGrpsAndGetModels (data=moralsData,c("title", "typeOfScen"), "res.RT", "overlapRound", "keyDef", c("Yes", "No"), "correct", params=parameters)

ch.moralsPlotsByGrpsAndGetModels <- function (data, grpCols, RTCol, overlapRoundCol, yesNoCol, yesNoVal = c("Yes", "No"), correctCol, correctVals = c(TRUE, FALSE), targetPresentCol, targetPresentVals, params, minNperOverlap = 0, minUniqueOverlaps = 3, useTwoParameterModel = useTwoParameterModel, savePlots = T) {

  	mainDir <- getwd()

		ch.newDir (mainDir, params$gpSubDir)
		gpDir <- getwd()
		setwd(mainDir)

    statsOutputFile <- file.path(mainDir,paste(params$dt.set, params$statsOutputFilePrefix))

    for (i in grpCols) {
      #force each grouping variable to a character type
      data[[i]] <- as.character(data[[i]])
    }

    #subset the data
    list.DFbyGroups <- ch.subsetDFbyGroups(data, grpCols)
    #get the index of all the groups that were subset
    dfIndex <- list.DFbyGroups$dfIndex

    append1 <- F
    grpOutModels <- list()
    ### plot individually
    op <-	par(mfrow=c(1,1), bg="white",  bty="n", font=2, family='serif', mar=c(5,6,4,7), las=1, cex=1)

    #for every group subset, plot p(no), p(hit), and RT for each condition on separate graphs
    for (i in 1:nrow(dfIndex)) {
      colNames <- paste(grpCols, collapse="-")
      colValues <- paste(dfIndex[i,1:length(dfIndex)-1], collapse="-")

      tmpDF <- list.DFbyGroups[[dfIndex$indexNum[i]]]

      if(nrow(tmpDF) > 2) {
        title <- paste(colNames,"=",colValues)

        if(savePlots) {
          # filenamePno <-file.path(gpDir,paste(params$dt.set,title,"p(no).pdf"))
          filenameRT <-file.path(gpDir,paste(params$dt.set,title,"p(hit) and RT.pdf"))
          filenameDP <- file.path(gpDir,paste(params$dt.set, title, "d prime.pdf"))
        } else {
          # filenamePno <- NULL
          filenameRT <- NULL
          filenameDP <- NULL
        }

        #plot RT and p(Hit) by overlap round
        rt.outList <- ch.moralsRTpHitFit(tmpDF, overlapRoundCol, RTCol, correctCol, correctVals, minNperOverlap = minNperOverlap, useTwoParameterModel = useTwoParameterModel, printR2 = T, filename = filenameRT, topTitle = title)
        RTModel <- ch.getLmModel(rt.outList$RTfit, yLab="RT")
        pHitModel <- ch.getPhitModel(rt.outList$pHitFit)

    		#plot d prime and beta by overlap round
        dp.outList <- ch.moralsPlotDprimeBetaFits(tmpDF, overlapRoundCol, correctCol, correctVals, targetPresentCol, targetPresentVals, minNperOverlap = minNperOverlap, minUniqueOverlaps= minUniqueOverlaps, printR2 = T, filename = filenameDP, topTitle = title)
        if(all(is.na(dp.outList$dPrimeFit) == FALSE)) {
          dPrimeModel <- ch.getLmModel(dp.outList$dPrimeFit, yLab="d'")
        } else {
          dPrimeModel <- NA
        }
        if(all(is.na(dp.outList$betaFit) == FALSE)) {
          betaModel <- ch.getLmModel(dp.outList$betaFit, yLab="Beta")
        } else {
          betaModel <- NA
        }

        #put everything into a list
        grpOutModels$dfIndex <- ch.rbind(grpOutModels$dfIndex,dfIndex[i,])
        grpOutModels$RTModel[i] <- list(RTModel)
        grpOutModels$pHitModel[i] <- list(pHitModel)
        grpOutModels$dPrimeModel[i] <- list(dPrimeModel)
        grpOutModels$betaModel[i] <- list(betaModel)
        # grpOutModels$pNoModel[i] <- list(pNoModel)

        #output stats
  			sink(statsOutputFile, append=T)
  				cat("\n\n*********************************", "all",colNames,"=", colValues,  "*********************************\n\n")
          cat("\n\n**** Average RT ****\n\n")
    			print(summary(rt.outList$RTfit))
    			cat("\n\n**** p(Hit) ****\n\n")
    			print(summary(rt.outList$pHitFit))
    			cat("r_square: ",rt.outList$pHitR2)
          cat("\n\n**** d Prime ****\n\n")

          if(all(is.na(dp.outList$dPrimeFit) == FALSE)) {
            print(summary(dp.outList$dPrimeFit))
          } else {
            print(paste("Number of Unique OverlapRound Bins Less Than", minUniqueOverlaps))
          }
          cat("\n\n**** beta ****\n\n")
          if(all(is.na(dp.outList$betaFit) == FALSE)) {
    			  print(summary(dp.outList$betaFit))
          } else {
            print(paste("Number of Unique OverlapRound Bins Less Than", minUniqueOverlaps))
          }
  			sink(NULL)
      }
    }
    par(op)
    return(grpOutModels)

}
