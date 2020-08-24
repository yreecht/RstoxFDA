
#' Tabulate fisheries
#' @description Tabulates fisheries based on custom cell definitions
#' @details
#'  A fishery will be decomposed into cells, within which total weight will be reported.
#' @param data data.frame containg fisheries data
#' @param weightCol character() column in 'data' that contains catch weights
#' @param cellCols charcter() vector of columns in 'data' defining cells
#' @param complete logical() whether all combinations of the columns in cellCols should be tabulated, even if they contain no catch.
#' @return \code{\link[data.table]{data.table}} with the cells specified in cellCols tabulated by decreasing weight and with the columns 'weight', 'frac' and 'cumFrac' containing the weight in each cells and the fraction and cumulative fraction of total weight in that cell.
#' @examples
#'  data(landings)
#'  tabulateFisheries(landings)
#' @export
tabulateFisheries <- function(data, weightCol="LiveWeightKG", cellCols=c("Metier5", "quarter", "Area"), complete=F){

  if (any(c("weight", "cumFrac") %in% cellCols)){
    stop("Column names 'weight' and 'cumFrac' are reserved. Cannot be used as cell column.")
  }
  if (!all(cellCols %in% names(data))){
    stop("Some cell columns are not columns of 'data'")
  }

  aggVars <- list()
  for (v in cellCols){
    stopifnot(is.character(v))
    aggVars[[v]] <- data[[v]]
  }

  tab <- aggregate(list(weight=data[[weightCol]]), by=aggVars, FUN=function(x){sum(x)}, drop=!complete)
  tab$weight[is.na(tab$weight)] <- 0
  tab <- tab[order(tab$weight, decreasing = T),]
  tab$frac <- tab$weight / sum(tab$weight)
  tab$cumFrac <- cumsum(tab$weight) / sum(tab$weight)

  return(data.table::as.data.table(tab))

}
