#' MetierTable lvl 4
#'
#' Example of metier table for assigning fishing activity to Metier level 4.
#' This is not a universal conversion table, but an example of a table made for a particular purpose.
#' Some gear assignments are done with assumptions, and considering the exact metier codes acceptable by data recipients.
#'
#' For example NS9400 has codes for nephrops and shrimp trawls, and does not necessarilly distinguish pelagic shrimp trawls from bottom shrimp trawls.
#' Also the data recipient did not accept the FAO codes for nephrops and shrimp trawls,
#' so all of these gears are assigned to OTB, as this was identified as the most likely category based on knowledge of the fishery in question.
#'
#' There are other examples of pragmatic code-mapping as well. This particular one was explained in order to caution against indiscriminate use.
#' The code example below annotates an activity census with FAO-gearcode declarations and compares the annotated metiers with the codes used for reporting catch.
#'
#' @docType data
#'
#' @usage data(metier4table)
#'
#' @format \code{\link[RstoxFDA]{MetierTable}} with column 'gearcode' identifying gear codes used in Norwegian fisheries data (derived from NS 9400)
#'
#' @keywords datasets
#'
#' @examples
#' data(metier4table)
#' data(activityCensus)
#' annotated <- assignMetier(activityCensus, metier4table, "gearNS", metierColName = "metier4")
#' table(annotated$gearFAO, annotated$metier4)
"metier4table"
