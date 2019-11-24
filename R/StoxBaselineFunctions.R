

#' Checks symmetry of Car table
#' @noRd
checkSymmetry <- function(tab){

  getn <- function(value){
    neighbours <- trimws(unlist(strsplit(tab[tab[,1]==value,2], split = ",")))
    return(neighbours)
  }

  for (i in 1:nrow(tab)){
    carvalue <- tab[i,1]
    neighbours <- getn(carvalue)
    for (n in neighbours){
      if (!(n %in% tab[,1]) | !(carvalue %in% getn(n))){
        stop(paste("Neighbour definition not symmetric.", n, "is neighbour of", carvalue, "but not vice versa."))
      }
    }
  }
}

#' Get lookup list for unified categorical definition
#' @description
#'  From a resource file, read a definition for a categorical variable that can unify coding systems
#'  between formats such as NMDlanding and NMDbiotic.
#' @details
#'  When different data formats encode similar information in with different coding systems
#'  a unified value can be defined and set in correspondance with one or more codes in the different formats.
#'  this is done in order to convert coding systems to the unified coding system.
#'
#'  Definitions are stored with in a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: <name not constrained>}{Unified value (key)}
#'  \item{Column 2: <name not constrained>}{The format (key)}
#'  \item{Column 3: <name not constrained>}{A comma separated list of values, any of which will be defined as the unified value in the format.}
#'  }
#' @param tab parsed tab separated file
#' @param formats the formats to extract definitions for, if NULL all formats will be extracted.
#' @return list() with one member for each format, each a list mapping codes in that format to the unified variable.
#' @noRd
makeUnifiedDefinitionLookupList <- function(tab, formats=NULL){

  if (is.null(formats)){
    formats <- unique(unlist(tab[,2]))
  }

  if (!all(formats %in% unlist(tab[,2]))){
    missing <- formats[!(formats %in% unlist(tab[,2]))]
    stop(paste("Not all formats found in resource. Missing:", paste(missing, collapse=", ")))
  }

  mappings <- list()
  for (f in formats){
    mappings[[f]] <- list()
    ftab <- tab[tab[,2] == f,]
    for (i in 1:nrow(ftab)){
      u <- ftab[[i,1]]
      codes <- trimws(unique(unlist(strsplit(ftab[[i,3]], ","))))
      if (any(codes %in% unlist(names(mappings[[f]])))){
        redefined <- codes[codes %in% unlist(names(mappings[[f]]))]
        stop(paste("Codes redefined:", paste(redefined, sep=", ")))
      }
      for (code in codes){
        mappings[[f]][code] <- u
      }
    }
  }

  return(mappings)
}

###
# Functions for defining resources, typically processData
#


#' Define gear
#' @description
#'  StoX function
#'  Define a unified categorical variable 'Gear', and its correspondance to gear codes in
#'  data formats \code{\link[RstoxData]{StoxBioticData}} and \code{\link[RstoxData]{StoxLandingData}}.
#'  Definitions are read from a resource file.
#' @details
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'UnifiedVariable'}{Unified value (key)}
#'  \item{Column 2: 'Source'}{The format for which the unified value is defined(key)}
#'  \item{Column 3: 'Definition'}{A comma separated list of values, any of which will be defined as the unified value in the format.}
#'  }
#' @param processData data.table() as returned from this function
#' @param resourceFilePath path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Unified variable definition, see: \code{\link[RstoxFDA]{UnifiedVariableDefinition}}.
#' @export
DefineGear <- function(processData, resourceFilePath, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  tab <- readTabSepFile(resourceFilePath, col_types = "ccc", col_names = c("UnifiedVariable", "Source", "Definition"), encoding = encoding)

  if (!nrow(unique(tab[,1:2])) == nrow(tab)){
    stop("Malformed resource file. Non-unique keys: repition in first two columns.")
  }

  return(tab)
}

#' Define Temporal Categories
#' @description
#'  Define temporal categories for grouping data based on date.
#' @details
#'  A seasonal definition has the effect of making caegories independent of year,
#'  and of making season cross year boundaries of 1st of January is not a the start of any category.
#' @param processData data.table() as returned from this function
#' @param temporalCategory character(), defaults to 'Quarter', type of temporal category: 'Quarter', 'Month' or 'Custom'
#' @param customPeriods character(), provided if temporalCategory is 'Custom', vector of strings formatted as DD-MM, giving the start date of each temporal category.
#' @param seasonal logical(), defaults to TRUE, whether the categories are defined as seasons or periods, that is, whether e.g. Q1 of 2015 is condiedered the same category as Q1 2016.
#' @param years integer() vector, optional, provide if non-seasonal categories should specify year.
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Temporal Categories, see: \code{\link[RstoxFDA]{TemporalCategories}}.
#' @export
DefineTemporalCategories <- function(processData, temporalCategory=c("Quarter", "Month", "Custom"), customPeriods = NULL, seasonal = T, years = NULL, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }


  temporalCategory <- match.arg(temporalCategory, temporalCategory)

  if (length(customPeriods)>0 & temporalCategory != "Custom"){
    stop(paste("Custom period provided, but temporalCategory is", temporalCategory))
  }

  if (seasonal & length(years)>0){
    stop("Years provided for seasonal definition.")
  }

  if (length(customPeriods)>0){
    if (length(customPeriods) == 1){
      stop("Need to provide at least two periods")
    }
    if (any(duplicated(customPeriods))){
      stop("Need to provide unique periods.")
    }
    customPeriods <- trimws(customPeriods)
    form <- function(x){
      if (nchar(x) != 5){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
      if (substr(x,3,3) != "-"){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
      if (is.na(as.integer(substr(x,1,2))) | is.na(as.integer(substr(x,4,5)))){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
      if (as.integer(substr(x,1,2)) > 31 |
          as.integer(substr(x,1,2)) < 1 |
          as.integer(substr(x,4,5)) < 1 |
          as.integer(substr(x,4,5)) > 12){
        stop("Malformed custom period. All periods must be on the form 'DD-MM' (startday)")
      }
    }

    sapply(customPeriods, form)
  }

  if (temporalCategory == "Month"){
    output <- data.table::data.table(temporalCategory=as.character(
        c("January", "Februrary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
        startDay=as.integer(rep(1,12)),
        startMonth=as.integer(seq(1,12)),
        year=as.integer(rep(NA, 4))
    )
  }
  else if (temporalCategory == "Quarter"){
    output <- data.table::data.table(temporalCategory=as.character(
      c("Q1", "Q2", "Q3", "Q4")),
      startDay=as.integer(rep(1,4)),
      startMonth=as.integer(c(1,4,7,10)),
      year=as.integer(rep(NA, 4))
    )
  }
  else if (temporalCategory == "Custom"){
    days <- as.integer(substr(customPeriods, 1,2))
    months <- as.integer(substr(customPeriods, 4,5))

    ord <- order(months, days)
    months <- months[ord]
    days <- days[ord]

    if (!seasonal){
      if (months[1]!=1 & days[1] != 1){
        days <- c(1, days)
        months <- c(1, months)

        customPeriods <- c("01-01", customPeriods)

      }
      startstr <- customPeriods
      endstr <- c(customPeriods[2:length(customPeriods)], "-----")

    }
    else if (seasonal){
      startstr <- customPeriods
      endstr <- c(customPeriods[2:length(customPeriods)], customPeriods[1])
    }
    else{
      stop()
    }

    output <- data.table::data.table(temporalCategory=as.character(
      paste("[", startstr, ", ", endstr, ">", sep="")),
      startDay=days,
      startMonth=months,
      year=as.integer(rep(NA, length(days)))
    )
  }
  else{
    stop(paste("Temporal category", temporalCategory, "not recognized."))
  }

  if (!seasonal & length(years)>0){
    ncat <- nrow(output)
    out <- output
    out$year <- rep(years[1], ncat)
    out$temporalCategory <- paste(years[1], " ", output$temporalCategory, sep="")
    yearoutput <- out
    for (i in 1:(length(years)-1)){
      y<-i+1
      out <- output
      out$year <- rep(years[y], ncat)
      out$temporalCategory <- paste(years[y], " ", output$temporalCategory, sep="")
      yearoutput <- rbind(yearoutput, out)
    }
    output <- yearoutput
  }

  return(output)
}

#' Define Area Code Positions
#' @description
#'  StoX function
#'  Define positions for areas of a spatial coding system.
#'  Definitions are read from a resource file.
#' @details
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'Area'}{Area code (key)}
#'  \item{Column 2: 'Location'}{optional subdivision of area. If provided, encode the case for missing location should be encoded as well.}
#'  \item{Column 3: 'Latitude'}{WGS84 Latitude, decimal degress}
#'  \item{Column 4: 'Longitude'}{WGS84 Longitude, decimal degress}
#'  }
#' @param processData data.table() as returned from this function
#' @param resourceFilePath path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return \code{\link[RstoxFDA]{AreaCodePosition}}.
#' @export
DefineAreaCodePosition <- function(processData, resourceFilePath, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  tab <- readTabSepFile(resourceFilePath, col_types = "ccdd", col_names = c("Area", "Location",	"Latitude",	"Longitude"), encoding = encoding)

  missingLoc <- tab[is.na(tab[,2]),]

  if (length(unique(missingLoc[,1])) != length(unique(tab[,1]))){
    stop("Malformed resource file. Some Area does not have coordinates defined for the case when location is missing.")
  }

  return(tab)

}

#' Define CAR neighbours
#' @description
#'  StoX function.
#'  Define which spatial strata are to be considered neighbours,
#'  when used as a CAR-variable (Conditional AutoRegressive variable).
#' @details
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'  \item{Column 1: 'CarValue'}{Value for the CAR-variable (key)}
#'  \item{Column 2: 'Neigbhours'}{Comma-separated list of neighbours (each should occur in Column 1)}
#'  }
#'  The neighbour definition must be symmetric.
#'  If a is among the neighbours of b, b must also be among the neighbours of a.
#' @param processData data.table() as returned from this function
#' @param resourceFilePath path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Area Neighbour Definition, see: \code{\link[RstoxFDA]{CarNeighbours}}.
#' @export
DefineCarNeighbours <- function(processData, resourceFilePath, encoding="UTF-8", useProcessData=F){
  if (useProcessData){
    return(processData)
  }

  tab <- readTabSepFile(resourceFilePath, col_types = "cc", col_names = c("CarValue", "Neighbours"), encoding = encoding)

  checkSymmetry(tab)

  if (length(unique(tab[,1])) != nrow(tab)){
    d <- tab[,1][duplicated(tab[,1])]
    stop(paste("Malformed resource file, Non-unique keys: repition in first column:", paste(d, collapse = ",")))
  }

  return(tab)
}

#' Define Age Error Matrix
#' @description
#'  StoX function.
#'  Defines probabilities for misreading ages.
#' @details
#'  Definitions are read from a tab separated file with headers and row names in first column.
#'  All row and column names should be integers.
#'  The matrix encodes the probability of observing an age (rows), given true age (columns).
#'  Columns must sum to 1.
#' @param processData data.table() as returned from this function
#' @param resourceFilePath path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Age Error Matrix, see: \code{\link[RstoxFDA]{AgeErrorMatrix}}.
#' @export
DefineAgeErrorMatrix <- function(processData, resourceFilePath, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  stream <- file(resourceFilePath, open="r")
  matrixNoHeader <- utils::read.delim(stream, sep="\t", header=F, encoding = encoding)
  close(stream)

  stream <- file(resourceFilePath, open="r")
  matrix <- utils::read.delim(stream, sep="\t", header=T, row.names = 1, encoding = encoding)
  close(stream)

  coln <- as.character(matrixNoHeader[1,2:ncol(matrixNoHeader)])
  dt <- data.table::data.table(matrix)

  colnames(dt) <- coln
  dt$ReadAge <- rownames(matrix)

  if (!all(colSums(matrix) == 1)){
    stop("Malformed resource file. Columns must sum to 1.")
  }

  if (any(matrix < 0) | any(matrix > 1)){
    stop("Malformed resource file. All probabilities must be in >=0 and <=1.")
  }

  return(dt)
}

#' Define Classification Error (for stock splitting)
#' @description
#'  StoX function.
#'  Defines probabilities for misclassifying when determining stock membership of a specimen.
#' @details
#'  Definitions are read from a tab separated file with headers. Columns defined as:
#'  \describe{
#'   \item{Column 1 : ptype1.CC}{Probability of classifying a type 1 specimen as type 1.}
#'   \item{Column 2: ptype1.S}{Probability of classifying a type 5 specimen as type 1.}
#'   \item{Column 3: ptype2.CC}{Probability of classifying a type 2 specimen as type 2.}
#'   \item{Column 4: ptype2.S}{Probability of classifying a type 4 specimen as type 2.}
#'   \item{Column 5: ptype4.CC}{Probability of classifying a type 2 specimen as type 4.}
#'   \item{Column 6: ptype4.S}{Probability of classifying a type 4 specimen as type 4.}
#'   \item{Column 7: ptype5.CC}{Probability of classifying a type 1 specimen as type 5.}
#'   \item{Column 8: ptype5.S}{Probability of classifying a type 5 specimen as type 5.}
#'  }
#'  see \code{\link[RstoxFDA]{ClassificationError}} for further explanation on the coding system.
#' @param processData data.table() as returned from this function
#' @param resourceFilePath path to resource file
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Classification Error Matrix, see: \code{\link[RstoxFDA]{ClassificationError}}.
#' @export
DefineClassificationError<- function(processData, resourceFilePath, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  tab <- readTabSepFile(resourceFilePath,
                        col_types = "dddddddd",
                        col_names = c("ptype1.CC", "ptype1.S", "ptype2.CC", "ptype2.S", "ptype4.CC", "ptype4.S", "ptype5.CC", "ptype5.S"),
                        encoding = encoding)

  if (nrow((tab)) != 1){
    stop("Malformed resource file: contains more than one row.")
  }

  return(tab)
}

