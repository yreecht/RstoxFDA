
#' read tab separated file
#' @noRd
readTabSepFile <- function(filepath, encoding="ascii"){
  loc <- readr::default_locale()
  loc$encoding <- encoding
  tab <- readr::read_delim(filepath, delim = "\t", locale = loc, col_types = "ccc")
  return(data.table::as.data.table(tab))
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

  if (!nrow(unique(tab[,1:2])) == nrow(tab)){
    stop("Malformed resource file. Non-unique keys: repition in first two columns.")
  }

  if (is.null(formats)){
    formats <- unique(unlist(tab[,2]))
  }

  if (!all(formats %in% unlist(tab[,2]))){
    missing <- formats[!(formats %in% unlist(tab[,2]))]
    stop(paste("Not all formats found in file. Missing:", paste(missing, collapse=", ")))
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

#' Define gear
#' @description
#'  Define a unified categorical variable 'Gear', and its correspondance to gear codes in
#'  data formats \code{\link[RstoxData]{StoxBioticData}} and \code{\link[RstoxData]{StoxLandingData}}.
#'  Definitions are read from a resource file.
#' @details
#'  Definitions are stored with in a tab separated file with headers. Columns defined as:
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
DefineGear <- function(processData, resourceFilePath, encoding="latin1", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  tab <- readTabSepFile(resourceFilePath)
  return(tab)
}

#' Prepare data for Reca.
#' @description
#'  StoX-function.
#'  Performs data checks and data conversions,
#'  and stores some data-related parameters in preparation for running
#'  \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#'  via \code{\link[RstoxFDA]{RunReca}}.
#'
#' @param StoxBioticData
#'  \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries
#'  and approriate columns appended for identifying corresponding landings.
#' @param StoxLandingData
#'  \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries
#'  and approriate columns appended for identifying corresponding samples
#' @param fixedEffects
#'  character() vector identifying column names that should be treated as fixed effects
#' @param randomEffects
#'  character() vector identifying column names that should be treated as fixed effects
#' @param carEffect
#'  character(), optional, identifying the column name that should be treated as CAR-effect
#'  (conditional autoregressive effect)
#' @param CarNeighbours
#'  \code{\link[RstoxFDA]{CarNeighbours}}, mandatory if 'carEffect' is given.
#'  Identifies which values of the carEffect are to be considered as neighbours.
#' @param AgeErrorMatrix
#'  \code{\link[RstoxFDA]{AgeErrorMatrix}}, optional, specifies the probabilities of misreading ages.
#'  If not provided age errors will not be modelled.
#' @param stockSplitting
#'  logical(), default FALSE, whether to run estimates for separate stocks in the data (coastal cod-analysis)
#' @param ClassificationErrorMatrix
#'  \code{\link[RstoxFDA]{ClassificationErrorMatrix}}, optional,
#'  specifies the probability of misclassifying stock for an individual Used in conjunction with 'stockSplitting'. If not provided classification errors will not be modelled.
#' @param minAge
#'  integer(), optional, must match dimensions of any 'AgeErrorMatrix'.
#'  If not provided it will be derived from data.
#' @param maxAge
#'  integer(), optional, must match dimensions of any 'AgeErrorMatrix'.
#'  If not provided it will be derived from data.
#' @param maxLength
#'  numeric(), optional, maximal fish length in data in cm.
#'  If not provided it will be derived from data.
#' @param lengthResolution
#'  numeric(), optional, resolution for length measurements in cm.
#'  If not provided modal value from data is used.
#' @param temporalResolution
#'  character(), default "Quarter", code for temporal resolution in landings: "Month" or "Quarter".
#'  Regulates temporal resolution for calculating fractional ages of fish.
#'  Not to be confused with any temporal covariate.
#' @param hatchDay
#'  integer(), defaults to 1 representing Jan 1st.
#'  encoding the day of the year when fish is consider to transition from one age to the next.
#' @return \code{\link[RstoxFDA]{RecaData}} Data prepared for running Reca.
#' @export
PrepareReca <- function(StoxBioticData, StoxLandingData, fixedEffects, randomEffects, carEffect=NULL, CarNeighbours=NULL, AgeErrorMatrix=NULL, stockSplitting=F, ClassificationErrorMatrix=NULL, minAge=NULL, maxAge=NULL, maxLength=NULL, lengthResolution=NULL, temporalResolution=c("Quarter", "Month"), hatchDay=1){

  temporalResolution <- match.arg(temporalResolution, temporalResolution)
  if (!(temporalResolution %in% c("Quarter", "Month", "Week"))){
    stop(paste("Temporal resolution", temporalResolution, "not supported"))
  }

  if (stockSplitting){
    stop("Data preparation for stock splitting is not yet implemented.")
  }

  quarter=NULL
  month=NULL

  if (temporalResolution == "Quarter"){

  }
  else if (temporalResolution == "Month"){

  }
  else{
    stop(paste("Temporal resolution", temporalResolution, "not supported"))
  }

  warning("Get nFish for each sample with delprøve.")
  nFish = NULL

  recaObject <- prepRECA(StoxBioticData, StoxLandingData, fixedEffects, randomEffects, carEffect, neighbours=CarNeighbours, nFish=nFish, ageError=AgeErrorMatrix, minAge=minAge, maxAge=maxAge, maxLength=maxLength, lengthResolution=lengthResolution, date=NULL, month=month, quarter=quarter, hatchDay=hatchDay)
  return(recaObject)
}

##
# Stox Data types
##

#' RecaData
#'
#' Data and some data parameters prepared for running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunReca}}.
#'
#' @details
#' \describe{
#'  \item{AgeLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{WeightLength}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{Landings}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{GlobalParameters}{input needed for \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}. see details}
#'  \item{CovariateMaps}{Mapping of values for each covariate in landings and samples (including non-configurable catchId) to integer value used in R-ECA.}
#' }
#'
#' @name RecaData
#'
NULL

#' UnifiedVariableDefinition
#'
#' Table (\code{\link[data.table]{data.table}}) defining a unified variable for different data formats
#'
#' @details
#'  \describe{
#'   \item{Unified variable}{Unified code}
#'   \item{Source}{Format for which the unified variable has corresponding codes}
#'   \item{Definition}{The codes defining the unified code in the 'source'. Comma-separated list of codes.}
#'  }
#'
#' @name UnifiedVariableDefinition
#'
NULL


#' Function specification for inclusion in StoX projects
#' @export
stoxFunctionAttributes <- list(

  DefineGear = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "GearDefinition",
    functionParameterType = list(resourceFilePath = "character"),
    functionParameterFormat = list(resourceFilePath = "filePaths"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  PrepareReca = list(
    functionType = "modelData",
    functionCategory = "Analysis",
    functionOutputDataType = "RecaData",
    functionParameterType = list(StoxBioticData = "character",
                                 StoxLandingData = "character",
                                 fixedEffects = "character",
                                 randomEffects = "character",
                                 carEffect = "character",
                                 CarNeighbours = "character",
                                 AgeErrorMatrix = "character",
                                 stockSplitting = "logical",
                                 ClassificationErrorMatrix = "character",
                                 minAge = "integer",
                                 maxAge = "integer",
                                 maxLength = "numeric",
                                 lengthResolution = "numeric",
                                 temporalResolution = "character",
                                 hatchDay = "integer"),
    functionParameterFormat = list(
                                   fixedEffects = "vector",
                                   randomEffects = "vector"
                                   ),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  )

)
