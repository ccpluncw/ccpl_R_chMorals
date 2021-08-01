#' A function to get the average directional overlap by group.
#'
#' This function calculates the average directional overlap by group.
#' @param data an overlaps dataframe that contains the directional overlaps for all probes (use ch.moralsGetDirOverlapForAllProbes()) and one column with the group name and one column with the probe name (that is the same for all the groups that it belongs to). So, there may be a probe "book" that is the member of three groups "gp1", "gp2", and "gp3".  Here, the "book" would be in the probe column, and gp# would be in the group column.
#' @param probe1Col a string the specifies the column name in "data" that contains the first probe item. Only one probe column is needed because all the probes and their reversals are present in the data.
#' @param grpCol a string the specifies the column name in "data" that contains the grouping column. If NULL, there is not grouping. DEFAULT = NULL.
#' @param dirOverlapCol a string the specifies the column name in "data" that contains the directional overlap column.
#' @return a dataframe with the probe in one column, it's average directional overlap in another, its N in another, followed by separate columns containing the average directional overlap and N for each group.
#' @export
#' @examples ch.moralsGetDirOverlapByGrp (overlapData, "probe1", "p1Quant", "dirOverlap")

ch.moralsGetDirOverlapByGrp <- function (data, probe1Col, grpCol = NULL, dirOverlapCol) {

  #get the average directional overlap by by probe 1
  probeOrder.table <- data.frame (data %>% dplyr::group_by_(probe1Col) %>%  dplyr::summarise( meanDO = mean(eval(parse(text=dirOverlapCol))), nDO = length(eval(parse(text=dirOverlapCol))) ))

  if(!is.null(grpCol)) {
    #get the average directional overlap by quanty (group) size
    probeOrder.table2 <- data.frame (data %>% dplyr::group_by_(probe1Col, grpCol) %>%  dplyr::summarise( meanDO = mean(eval(parse(text=dirOverlapCol))), nDO = length(eval(parse(text=dirOverlapCol))) ))

    #reshape to wide so that each group size in a different column
    probeOrder.table.wide <- reshape(probeOrder.table2, idvar = probe1Col, timevar = grpCol, direction = "wide")

    #merge the overage and the group data
    probeGrpSizeValues <- merge(probeOrder.table, probeOrder.table.wide, by=probe1Col)
  } else {
    probeGrpSizeValues <- probeOrder.table
  }
  probeGrpSizeValues <- probeGrpSizeValues[with(probeGrpSizeValues, order(meanDO)),]

  return(probeGrpSizeValues)
}
