#' Get temporal categories
#' @description define a categorical variable based for a vector of dates
#' @param date POSIXct() vector of dates
#' @param temporalType character() specify the kind of temporal category to define: "quarter", "week" or "custom"
#' @param seasonal logical() specify whether the temporal category should be seasonal (e.g. week 1 in year x is the same category as week 1 in year y)
#' @param FUN function(day, month) mapping a day (1-based integer()) and month (1-based integer()) to a value (character()) for the categorical variable to be defined.
#' @return character() a vector of values for the categorical variable, corresponding to the dates in 'date'
getTemporalCategory <- function(date, temporalType="quarter", seasonal=T, FUN=NULL){

  if (length(date) == 0){
    return(character())
  }


  if (!is.null(FUN) & temporalType != "custom"){
    stop("Parameter 'FUN' may only be used with temporalType custom")
  }

  output <- NULL

  if (temporalType == "week"){
    output <- paste("W", strftime(date, format="%V"), sep="")
  }
  else if (temporalType == "month"){
    output <- strftime(date, format="%B")
  }
  else if (temporalType == "quarter"){
    output <- paste("Q", (as.integer(strftime(date, format="%m"))-1)%/%3+1L, sep="")
  }
  else if (temporalType == "custom"){
    vf <- Vectorize(FUN, SIMPLIFY=T)
    output <- vf(as.integer(strftime(date, format="%d")), as.integer(strftime(date, format="%m")))
  }
  else{
    stop(paste("Temporal type", temporalType, "not recognized."))
  }

  if (!seasonal){
    year <- strftime(date, format="%Y")
    output <- paste(output, year, sep="-")
  }

  return(output)

}
