#' A function to randomize the relation between the probe names and the overlaps. This is used to test a Null model.
#'
#' @param data A dataframe with the overlap data processed from a typical values experiment.
#' @param probeCols a vector of strings that specifies the names of the columns in "data" that contains the the probes.
#' @param shuffleNum an integer specifying the number of times to "shuffle" the vector when randomizing the order. DEFAULT = 10.
#' @keywords randomize prompts overlap
#' @return the overlaps dataframe with the newly randomized relations between the overlaps and the prompt names. WARNING: Do Not Confuse this file with the orginal file that has the correct relation between prompts and overlaps.
#' @export
#' @examples ch.createRandomizedOverlapsFile (df.myInputData, c("Item1", "Item2"))

ch.createRandomizedOverlapsFile <- function(data, probeCols, shuffleNum = 10) {

  #first get the probe names
  probeItems <- vector()
  for(i in probeCols) {
    probeItems <- append(probeItems, unique(data[,i]))
  }
  probeItems <- unique(probeItems)

  #copy the probe items
  randomizedProbes <- probeItems
  #shuffle the order of the probe items
  for(i in 1:shuffleNum) {
    randomizedProbes <- sample(randomizedProbes)
  }
  #create a recode data frame
  df.recodeProbes <- data.frame(probeItems, randomizedProbes)
  #create new columns with the recoded item names
  data[c(probeCols)] <- lapply(data[c(probeCols)],
        function(x) df.recodeProbes$randomizedProbes[match(x, df.recodeProbes$probeItems)])

  return(data)

}
