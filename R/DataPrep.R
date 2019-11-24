#' read tab separated file
#' @noRd
readTabSepFile <- function(filepath, encoding="ascii", col_types = NULL, col_names = NULL){
  loc <- readr::default_locale()
  loc$encoding <- encoding
  tab <- readr::read_delim(filepath, delim = "\t", locale = loc, col_types = col_types)
  tab <- data.table::as.data.table(tab)

  if (length(col_names)>0){
    missing <- col_names[!(col_names %in% names(tab))]
    if (length(missing)>0){
      stop(paste("Resource file does not have required columns:", paste(missing, collapse=", ")))
    }
  }

  return(tab)
}

#' Get temporal categories
#' @description define a categorical variable based for a vector of dates
#' @param date POSIXct() vector of dates
#' @param temporalType character() specify the kind of temporal category to define: "quarter", "week" or "custom"
#' @param seasonal logical() specify whether the temporal category should be seasonal (e.g. week 1 in year x is the same category as week 1 in year y)
#' @param FUN function(day, month) mapping a day (1-based integer()) and month (1-based integer()) to a value (character()) for the categorical variable to be defined.
#' @examples
#'  #get current quarter
#'  quarter <- categoriseDate(Sys.time())
#'
#'  #get custom non-seasonal category
#'  dates <- as.POSIXct(c(
#'     "2018-01-17 21:37:29 CET",
#'     "2019-02-28 21:37:29 CET",
#'     "2019-12-28 21:37:29 CET"))
#'  inDecember <- function(day, month){if(month==12){return("Yes")};return("No")}
#'  categoriseDate(dates, temporalType = "custom", FUN=inDecember, seasonal = FALSE)
#'
#' @return character() a vector of values for the categorical variable, corresponding to the dates in 'date'
#' @export
categoriseDate <- function(date, temporalType="quarter", seasonal=T, FUN=NULL){

  if (any(is.na(date))){
    stop("NAs in date.")
  }

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

#' Convert codes.
#' @description
#'  Apply conversion table, perform approriate checks and return result.
#' @details
#'  Will stop with error if any codes can not be converted, or if any entries are NA.
#'  Require all codes (original and converted) to be character().
#' @param code character() with original codes
#' @param conversionTable list() mapping code to converted code.
#' @return character() with converted codes
#' @examples
#'  gearConversion <- list()
#'  gearConversion["TBS"] <- "OTB"
#'  gearConversion["TBN"] <- "OTB"
#'  gearConversion["OTB"] <- "OTB"
#'  convertCodes(c("TBS", "TBN", "OTB"), gearConversion)
#' @export
convertCodes <- function(code, conversionTable){

  if (length(code) == 0){
    return(character())
  }

  if (!is.character(code)){
    stop("Codes must be character()")
  }

  if (!is.character(unlist(conversionTable))){
    warning("Coercing converted codes to character")
  }

  if (is.null(names(conversionTable))){
    stop("Conversion table must be indexed by character(). names(conversionTable) is NULL.")
  }

  if (any(is.na(code))){
    stop("NAs in codes")
  }

  if (!all(code %in% names(conversionTable))){
    missing <- code[!(code %in% names(conversionTable))]
    stop(paste("Conversion not defined for all codes. Missing for:", paste(missing, collapse=", ")))
  }

  return(as.character(conversionTable[code]))
}
