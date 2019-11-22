
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

#' UnifiedVariableDefinition
#'
#' Table (\code{\link[data.table]{data.table}}) defining a unified variable for different data formats
#'
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
#' @details
#'  Performs data checks and data conversions,
#'  and stores some data-related parameters in preparation for running
#'  \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#'  via \code{\link}[RstoxFDA]{runReca}.
#'  @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data with samples from fisheries and approriate columns appended for identifying corresponding landings.
#'  @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data with landings from fisheries and approriate columns appended for identifying corresponding samples
#'  @param fixedEffects character() vector identifying column names that should be treated as fixed effects
#'  @param randomEffects character() vector identifying column names that should be treated as fixed effects
#'  @param carEffect character() identifying the column name that should be treated as CAR-effect (conditional autoregressive effect)
#'  @param CarNeighbours \code{\link[RstoxFDA]{CarNeighbours}} identifies which values of the carEffect are to be considered as neighbours.
#'  @param AgeErrorMatrix \code{\link[RstoxFDA]{AgeErrorMatrix}} specifies the probabilities of misreading ages.
#'  @param stockSplitting logical() whether to run estimate results for separate stocks in the data (coastal cod-analysis)
#'  @param ClassificationErrorMatrix \code{\link[RstoxFDA]{ClassificationErrorMatrix}} specifies the probability of misclassifying stock for an individual.
#'  @return \code{\link[RstoxFDA]{RecaData}}
#'  @export
PrepareReca <- function(StoxBioticData, StoxLandingData, fixedEffects, randomEffects, carEffect=NULL, CarNeighbours=NULL, AgeErrorMatrix=NULL, stockSplitting=F, ClassificationErrorMatrix=NULL){

  if (stockSplitting){
    stop("Data preparation for stock splitting is not yet implemented.")
  }

}

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
    functionCategory = "Baseline",
    functionOutputDataType = "RecaData",
    functionParameterType = list(StoxBioticData = "character",
                                 StoxLandingData = "character",
                                 fixedEffects = "character",
                                 randomEffects = "character",
                                 carEffect = "character",
                                 CarNeighbours = "character",
                                 AgeErrorMatrix = "character",
                                 stockSplitting = "logical",
                                 ClassificationErrorMatrix = "character"),
    functionParameterFormat = list(
                                   fixedEffects = "vector",
                                   randomEffects = "vector"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  )

)
