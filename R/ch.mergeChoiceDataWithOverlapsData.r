#' A function to prepare the morals data for analysis
#'
#' This function prepares the morals data for analysis by creating overlapRound, correct, and a few other columns.  It also makes sure the trials starts at 1, and computes average p(hit) and RT for each subject.
#' @param data A dataframe with the choice data from a typical PVT experiment.
#' @param overlap.data A dataframe with the overlap data processed from a typical values experiment.
#' @param overlapCol a string that specifies the name of the column in "data" that contains the overlaps for the item in each trial.
#' @param directionCol a string that specifies the name of the column in "data" that contains the direction of the overlap for each trial.
#' @param respChoiceCol a string that specifies the name of the column in "data" that contains the the participant's response to the prompt - yes take action or no take no action.
#' @param respChoiceVal a vector of two values that specifies the choose Item1 option ("yes" take action in many morals experimants, thus saving Item1) value (index 1) and the choose Item1 option ("no" take no action in many morals experimants, thus saving Item2) value (index 2).
#' @param item1cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 1.
#' @param item2cols a vector of strings that specifies the names of the columns in "data" that contains the the probes in Item 2.
#' @param overlapItem1cols a vector of strings that specifies the names of the columns in the overlaps data (overlap.data) that contains the correspoinding probes in Item 1.
#' @param overlapItem2cols a vector of strings that specifies the names of the columns in the overlaps data (overlap.data) that contains the correspoinding probes in Item 2.
#' @param roundThreshold An integer that specifies the nearest interval that the overlaps should be rounded to. DEFAULT = 0.1 (round to the nearest 0.1)
#' @param roundDirection An option that specifies the rounding direction: ceiling (always round up), floor (always round down), or round (round to the nearest value, up or down). DEFAULT = ceiling
#' @param numOverlapBins An integer that specifies the number of bins the overlaps should be binned into. This is only used if roundThreshold = NULL.  DEFAULT = 10 (round overlaps so that there are 10 bins)
#' @param outputFile the filename that you want the output written to. DEFAULT = NULL (no file written)
#' @param params a list of parameters that are read in using "ch.readMoralsDBfile.r."
#' @param overlapDataIsComplete A boolean that specifies whether the overlap data contains all the possible trial stimuli in the exact column order (TRUE) or whether the overlap file contains the trial stimuli in only one order and therefore has to be permuted to be merged with the choice data (FALSE).  DEFAULT = FALSE.
#' @keywords morals merge overlaps choice datafile
#' @return a dataframe of merged data.
#' @export
#' @examples ch.mergeChoiceDataWithOverlapsData (data=moralsData, "sn", "RT", "overlap", "direction", "trials", "respDef", respChoiceVal = c("Yes", "No"))

ch.mergeChoiceDataWithOverlapsData <- function (data, overlap.data, overlapCol, directionCol, respChoiceCol, respChoiceVal = c("Item1", "Item2"), item1cols = c("Item1"), item2cols = c("Item2"), overlapItem1cols = c("IA1"), overlapItem2cols = c("IB1"), roundThreshold = 0.1, roundDirection = ceiling, numOverlapBins = 10, outfile = NULL, overlapDataIsComplete = FALSE) {

  #remove leading whitespace and make all lowercase to make more similar.
  #then get the unique items to ensure they are in both sets
  overItems <- NULL
  for(i in 1:length(overlapItem1cols)) {
    overlap.data[[overlapItem1cols[i]]] <- ifelse(is.na(overlap.data[[overlapItem1cols[i]]]), "NA", overlap.data[[overlapItem1cols[i]]])
    overlap.data[[overlapItem1cols[i]]] <- as.character(overlap.data[[overlapItem1cols[i]]])
    overlap.data[[overlapItem1cols[i]]] <- tolower(trimws(overlap.data[[overlapItem1cols[i]]]))
    overItems <- c(overItems, unique(overlap.data[[overlapItem1cols[i]]]))
  }
  for(i in 1:length(overlapItem2cols)) {
    overlap.data[[overlapItem2cols[i]]] <- ifelse(is.na(overlap.data[[overlapItem2cols[i]]]), "NA", overlap.data[[overlapItem2cols[i]]])
    overlap.data[[overlapItem2cols[i]]] <- as.character(overlap.data[[overlapItem2cols[i]]])
    overlap.data[[overlapItem2cols[i]]] <- tolower(trimws(overlap.data[[overlapItem2cols[i]]]))
    overItems <- c(overItems, unique(overlap.data[[overlapItem2cols[i]]]))
  }
  overItems <- unique(overItems)
  #remove leading whitespace and make all lowercase to make more similar.
  #then get the unique items to ensure they are in both sets
  dataItems <- NULL
  for(i in 1:length(item1cols)) {
    data[[item1cols[i]]] <- ifelse(is.na(data[[item1cols[i]]]), "NA", data[[item1cols[i]]])
    data[[item1cols[i]]] <- as.character(data[[item1cols[i]]])
    data[[item1cols[i]]] <- tolower(trimws(data[[item1cols[i]]]))
    dataItems <- c(dataItems, unique(data[[item1cols[i]]]))
  }
  for(i in 1:length(item2cols)) {
    data[[item2cols[i]]] <- ifelse(is.na(data[[item2cols[i]]]), "NA", data[[item2cols[i]]])
    data[[item2cols[i]]] <- as.character(data[[item2cols[i]]])
    data[[item2cols[i]]] <- tolower(trimws(data[[item2cols[i]]]))
    dataItems <- c(dataItems, unique(data[[item2cols[i]]]))
  }
  dataItems <- unique(dataItems)
  itemsNotInValues <- unique(dataItems[! dataItems %in% overItems])

  if (length(itemsNotInValues) > 0) {
    print("these items are in the choice data, but not in the values data")
    print(itemsNotInValues)
    print("fatalError: All items in the choice data must be in the values data")
    stop()
  }


######_____MERGE MORALS DATA WITH OVERLAP DATA_____######

  if(overlapDataIsComplete == FALSE) {
      ## to do that, we get all possible permutations of the items presented in the experiment
      ## then we use those permutations to merge the morals data and the overlap dataset
      ## We have to do this because the overlap data exists only in one order (e.g., a-b, not b-a)
      ## and the morals data can have any order (e.g, a-b, and b-a)

      #get all combinations that produce the same outcomes
      xColPermA <- matrix(item1cols[ch.permute(length(item1cols))], ncol=length(item1cols))
      xColPermB <- matrix(item2cols[ch.permute(length(item2cols))], ncol=length(item2cols))

      xTot1 <- NULL
      for (i in 1:nrow(xColPermA)) {
        xTmp <- chutils:::cbind.fill(t(xColPermA[i,]), xColPermB)
        xTot1 <- ch.rbind(xTot1, xTmp)
      }

      #revere order for reversed direction
      xTot2 <- xTot1[,ncol(xTot1):1]

      #now get y vector: it only needs one order because all variations were in xTot1 and 2
      yTot <- c(overlapItem1cols, overlapItem2cols)

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
            dt.merged.a <-merge(data, overlap.data, by.x=c(unlist(xCol[i,])), by.y=c(unlist(yTot)))
            if(j == 1) {
              dt.merged.a$direct.xVy <- dt.merged.a[[directionCol]]
            } else {
              #	change the direction of choice  because we reversed the order xTot2
              dt.merged.a$direct.xVy <-ifelse(dt.merged.a[[directionCol]]==1,-1,1)
            }

            #merge it all into a big dataset and remove some extraneous columns
            dt.merged.d <- ch.rbind(dt.merged.d, dt.merged.a)
        }
      }
  } else {
    #if the overlaps data is complete, simply do the following
    nRowsData <- nrow(data)
    dt.merged.d <- merge(data, overlap.data, by.x=c(item1cols, item2cols), by.y=c(overlapItem1cols, overlapItem2cols))
    nRowsMerged <- nrow(dt.merged.d)
    if(nRowsData != nRowsMerged) {
      cat("The number of rows in the original dataset (", nRowsData, ") does not equal the number of rows in the dataset once it is merged with the overlaps (", nRowsMerged, "). You should find out why.  Generally, this is because there are stimuli in the choice dataset that are missing in the values dataset.")
      stop()
    }
    #add direct.xvy column.  This is the same as direction when using a complete overlaps file
    dt.merged.d$direct.xVy <- dt.merged.d$direction
  }

  #remove duplicate rows that got inserted when the same item is part of a pair in groupA or groupB
  dt.merged <- unique(dt.merged.d)

  ############ MAKE SURE YOU HAVE THE SMAE NUMBER OF ROWS IN THE OUTPUT Dataset
  nRowsData <- nrow(data)
  nRowsMerged <- nrow(dt.merged.d)
  if(nRowsData != nRowsMerged) {
    cat("The number of rows in the original dataset (", nRowsData, ") does not equal the number of rows in the dataset once it is merged with the overlaps (", nRowsMerged, "). You should find out why.  If overlapDataIsComplete = TRUE, this is often because there are stimuli in the choice dataset that are missing in the values dataset. If overlapDataIsComplete = FALSE, this is often because the overlap file contains all the possible trial options in the exact order that they are presented to the participant, therefore set overlapDataIsComplete = TRUE and try again. ")
    stop()
  }
  #remove duplicate rows that got inserted when the same item is part of a pair in groupA or groupB


  if(is.null(roundThreshold)) {
    dt.merged$overlapRound <-chutils::ch.binNumbers(dt.merged[[overlapCol]], numOverlapBins)
    #sometimes round
  } else {
    #sometimes round_any will produce negative bins when the overlap == 0.  Here, we correct for that.
    dt.merged[[overlapCol]] <- ifelse(dt.merged[[overlapCol]] == 0, 0.00000001, dt.merged[[overlapCol]])
    #round dt.merged$overlap
    dt.merged$overlapRound <- chutils::ch.round_any(dt.merged[[overlapCol]], roundThreshold, roundDirection)
  }

  #create the Correct Response column. This is necessary to calculate "percentHit", "freq.pred"
  #Here, correct == 1 indicates the person chose the item with the highest value
  dt.merged$correct <- ifelse(dt.merged$direct.xVy==-1 & dt.merged[[respChoiceCol]]==respChoiceVal[2], 1, ifelse(dt.merged$direct.xVy==1 & dt.merged[[respChoiceCol]]==respChoiceVal[1], 1, 0))
  #add a target present column: dt.merged$direct.xVy == 1 indicated that the item with the higher value will be
  #killed if nothing is done.  Therefore, according to SVT, the participant should identify it as a target and should act.
  dt.merged$targetPresent <- ifelse(dt.merged$direct.xVy == 1, TRUE, FALSE)
  #add a directional overlap column.  Here, dirOverlap = 0 if the distributions overlap completely; 1 if the value of Item1 is greater than Item 2; -1 if the value of Item 1 is less than Item 2.
  dt.merged$dirOverlap <- abs(dt.merged$overlap-1)*dt.merged$direct.xVy

  #drop direction column because it can confuse people --- direct.xVy is the accurate one.
  dt.merged[[directionCol]] <- NULL

  if(!is.null(outfile)) {
    write.table(dt.merged, file=outfile, quote=F, sep="\t", row.names=F)
    ### output a list of items by overlap
    itemCols <- c(item1cols, item2cols)

    outList <- ch.getMoralsItemsInAllOverlaps(dt.merged, "overlapRound", itemCols)
    overlapSummaryOutFile <- "Probes in OverlapRound.txt"
    sink(overlapSummaryOutFile, append = F)
      print("**** Items by Overlap ****")
      print(outList)
    sink(NULL)
  }

  return (dt.merged)
}
