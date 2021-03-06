#' @noRd
is.POSIXct <- function(date){
  if (length(date) > 1 & "POSIXct" %in% class(date)){
    return(TRUE)
  }
  if (length(date) == 1 & class(date) == "POSIXct"){
    return(TRUE)
  }

  return(FALSE)
}

#' @noRd
is.Date <- function(date){
  if (length(date) > 1 & "Date" %in% class(date)){
    return(TRUE)
  }
  if (length(date) == 1 & class(date) == "Date"){
    return(TRUE)
  }

  return(FALSE)
}
#' Reca Data (RecaData)
#'
#' Data and some data parameters prepared for running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunRecaEstimate}}.
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

#' Check if argument is RecaData
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaData}}
#' @param RecaData argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{RecaData}}
#' @export
is.RecaData <- function(RecaData){
  if (!is.list(RecaData)){
    return(FALSE)
  }
  if (!all(c("AgeLength", "WeightLength", "Landings", "GlobalParameters", "CovariateMaps") %in% names(RecaData))){
    return(FALSE)
  }
  if (!is.list(RecaData$AgeLength)){
    return(FALSE)
  }
  if (!is.list(RecaData$WeightLength)){
    return(FALSE)
  }
  if (!is.list(RecaData$Landings)){
    return(FALSE)
  }
  if (!is.list(RecaData$GlobalParameters)){
    return(FALSE)
  }
  if (!is.list(RecaData$CovariateMaps)){
    return(FALSE)
  }

  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaData$AgeLength))){
    return(FALSE)
  }
  if (!all(c("DataMatrix", "CovariateMatrix", "info") %in% names(RecaData$WeightLength))){
    return(FALSE)
  }
  if (!all(c("AgeLengthCov", "WeightLengthCov", "LiveWeightKG") %in% names(RecaData$Landings))){
    return(FALSE)
  }

  return(TRUE)
}

#' Reca Results (RecaResult)
#'
#' Results from running
#' \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}
#' via \code{\link[RstoxFDA]{RunRecaEstimate}}.
#'
#' @details
#'
#' \describe{
#'  \item{input}{All input data and parameters provided to \code{\link[Reca]{eca.estimate}} and \code{\link[Reca]{eca.predict}}}
#'  \item{fit}{as returned by \code{\link[Reca]{eca.estimate}}}
#'  \item{prediction}{as returned by \code{\link[Reca]{eca.predict}}}
#'  \item{covariateMaps}{list() mapping from Reca covariate encoding to values fed to \code{\link[RstoxFDA]{PrepareRecaEstimate}}. As in \code{\link[RstoxFDA]{RecaData}}}
#' }
#'
#' @name RecaResult
#'
NULL

#' @noRd
is.RecaPrediction <- function(prediction){
  if (!is.list(prediction)){
    return(FALSE)
  }
  if (!all(c("TotalCount", "MeanLength", "MeanWeight", "AgeCategories", "LengthIntervalsLog") %in% names(prediction))){
    return(FALSE)
  }
  if (!is.array(prediction$TotalCount)){
    return(FALSE)
  }
  if (!is.array(prediction$MeanLength)){
    return(FALSE)
  }
  if (!is.array(prediction$MeanWeight)){
    return(FALSE)
  }
  return(TRUE)
}

#' Check if argument is RecaResult
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{RecaResult}}
#' @param RecaResult argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{RecaResult}}
#' @export
is.RecaResult <- function(RecaResult){
  if (!is.list(RecaResult)){
    return(FALSE)
  }
  if (!all(c("input", "fit", "prediction", "covariateMaps") %in% names(RecaResult))){
    return(FALSE)
  }
  if (!is.RecaData(RecaResult$input)){
    return(FALSE)
  }
  if (!is.RecaPrediction(RecaResult$prediction)){
    return(FALSE)
  }

  return(TRUE)
}

#' Unified Variable Definition (UnifiedVariableDefinition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a unified variable for different data formats
#'
#' @details
#'  \describe{
#'   \item{UnifiedVariable}{Unified code}
#'   \item{Source}{Format for which the unified variable has corresponding codes}
#'   \item{Definition}{The codes defining the unified code in the 'source'. Comma-separated list of codes.}
#'  }
#'
#' @name UnifiedVariableDefinition
#'
NULL

#' Check if argument is UnifiedVariableDefinition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{UnifiedVariableDefinition}}
#' @param UnifiedVariableDefinition argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{UnifiedVariableDefinition}}
#' @export
is.UnifiedVariableDefinition <- function(UnifiedVariableDefinition){
  if (!data.table::is.data.table(UnifiedVariableDefinition)){
    return(FALSE)
  }
  if (!all(c("UnifiedVariable", "Source", "Definition") %in% names(UnifiedVariableDefinition))){
    return(FALSE)
  }

  return(TRUE)
}

#' Temporal Categories (TemporalDefinition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a categorical variable for grouping data based on date.
#'
#' @details
#'  \describe{
#'   \item{temporalCategory}{character() Value of the temporal category}
#'   \item{startDay}{integer() Day of month for first day in the temporal category (1-based)}
#'   \item{startMonth}{integer() Month for first day in the temporal category (1-based)}
#'   \item{year}{integer() Year for which the category is defined, NA for seasonal definitions or for definition for a single unspecified year.}
#'  }
#'
#'  Start and end of year is not implied as category delimitations when not included.
#'  If 1st of January is not definied as the start of a category,
#'  it is taken to be included in the last category of the preceding year.
#'
#' @name TemporalDefinition
#'
NULL

#' Check if argument is TemporalDefinition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{TemporalDefinition}}
#' @param TemporalDefinition argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{TemporalDefinition}}
#' @export
is.TemporalDefinition <- function(TemporalDefinition){
  if (!data.table::is.data.table(TemporalDefinition)){
    return(FALSE)
  }
  if (!all(c("temporalCategory", "startDay", "startMonth") %in% names(TemporalDefinition))){
    return(FALSE)
  }

  return(TRUE)
}

#' Area Code Positions (AreaCodePosition)
#'
#' Table (\code{\link[data.table]{data.table}}) defining a position for area codes.
#'
#' @details
#'  \describe{
#'   \item{Area}{Area code. (key)}
#'   \item{Location}{optional subdivision of 'Area'}
#'   \item{Latitude}{WGS84 Latitude, decimal degrees}
#'   \item{Longitude}{WGS84 Longitude, decimal degrees}
#'  }
#'  If location is provided, the case for missing location is also encoded.
#'
#' @name AreaCodePosition
#'
NULL

#' Check if argument is AreaCodePosition
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{AreaCodePosition}}
#' @param AreaCodePosition argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{AreaCodePosition}}
#' @export
is.AreaCodePosition <- function(AreaCodePosition){
  if (!data.table::is.data.table(AreaCodePosition)){
    return(FALSE)
  }
  if (!all(c("Area", "SubArea", "Latitude", "Longitude") %in% names(AreaCodePosition))){
    return(FALSE)
  }

  return(TRUE)
}

#' Area Neighbour Definition (CarNeighbours)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining neighbours for a CAR-variable (Conditional autoregressive variable).
#'
#' @details
#'  \describe{
#'   \item{CarVariable}{Values for a variable used as CAR-variable}
#'   \item{Neighbours}{Comma-separated list of neighbours}
#'  }
#'
#'  The table is symmetric, so that if b is a neighbour of a. a is also a neighbour of b.
#'
#' @name CarNeighbours
#'
NULL

#' Check if argument is CarNeighbours
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{CarNeighbours}}
#' @param CarNeighbours argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{CarNeighbours}}
#' @export
is.CarNeighbours <- function(CarNeighbours){
  if (!data.table::is.data.table(CarNeighbours)){
    return(FALSE)
  }
  if (!all(c("CarVariable", "Neighbours") %in% names(CarNeighbours))){
    return(FALSE)
  }

  return(TRUE)
}

#' Age Error Matrix (AgeErrorMatrix)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining probabilities of misreading age.
#'
#' @details
#'  \describe{
#'   \item{columns 1..n}{numeric() [0,1]. Probability of reading read age, given that true age is as column name.}
#'   \item{ReadAge}{The read age.}
#'  }
#'
#'  Columns sum to 1.
#'
#' @name AgeErrorMatrix
#'
NULL

#' Check if argument is AgeErrorMatrix
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{AgeErrorMatrix}}
#' @param AgeErrorMatrix argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{AgeErrorMatrix}}
#' @export
is.AgeErrorMatrix <- function(AgeErrorMatrix){
  if (!data.table::is.data.table(AgeErrorMatrix)){
    return(FALSE)
  }
  if (!("ReadAge" %in% names(AgeErrorMatrix))){
    return(FALSE)
  }

  return(TRUE)
}

#' Stock classification error (ClassificationError)
#'
#' Table (\code{\link[data.table]{data.table}})
#' defining probabilities of misclassifying stock membership of a fish (e.g. from otholith).
#'
#' The stock classification system is designed for coastal and atlantic cod as they are determined at IMR.
#' The two stock of interest are classified by code 1, and 5.
#' Code 2 signifies the same as code 1, but indicate less certainty.
#' Code 4 signifies the same as code 5, but indicate less certainty.
#'
#' The classification error specifies the probability of misclassifying between some of these classifications.
#'
#' @details
#'  \describe{
#'   \item{ptype1.CC}{numeric() [0,1]. Probability of classifying a type 1 specimen as type 1.}
#'   \item{ptype1.S}{numeric() [0,1]. Probability of classifying a type 5 specimen as type 1.}
#'   \item{ptype2.CC}{numeric() [0,1]. Probability of classifying a type 2 specimen as type 2.}
#'   \item{ptype2.S}{numeric() [0,1]. Probability of classifying a type 4 specimen as type 2.}
#'   \item{ptype4.CC}{numeric() [0,1]. Probability of classifying a type 2 specimen as type 4.}
#'   \item{ptype4.S}{numeric() [0,1]. Probability of classifying a type 4 specimen as type 4.}
#'   \item{ptype5.CC}{numeric() [0,1]. Probability of classifying a type 1 specimen as type 5.}
#'   \item{ptype5.S}{numeric() [0,1]. Probability of classifying a type 5 specimen as type 5.}
#'  }
#'
#'  The data table contains only one row
#'
#' @name ClassificationError
#'
NULL

#' Check if argument is ClassificationError
#' @description
#'  Checks if argument conforms to specification for \code{\link[RstoxFDA]{ClassificationError}}
#' @param ClassificationError argument to be checked for data conformity
#' @return logical, TRUE if argument conformed to specification for \code{\link[RstoxFDA]{ClassificationError}}
#' @export
is.ClassificationError <- function(ClassificationError){
  if (!data.table::is.data.table(ClassificationError)){
    return(FALSE)
  }
  if (!all(c("ptype1.CC", "ptype1.S", "ptype2.CC", "ptype2.S", "ptype4.CC", "ptype4.S", "ptype5.CC", "ptype5.S") %in% names(ClassificationError))){
    return(FALSE)
  }

  return(TRUE)
}

#' Function specification for inclusion in StoX projects
#' @export
stoxFunctionAttributes <- list(

  DefineGear = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "UnifiedVariableDefinition",
    functionParameterType = list(resourceFilePath = "character"),
    functionParameterFormat = list(resourceFilePath = "filePaths"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  DefineTemporalCategories = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "TemporalDefinition",
    functionParameterType = list(temporalCategory = "character",
                                 customPeriods = "character",
                                 seasonal = "logical",
                                 years = "integer"),
    functionParameterFormat = list(customPeriods = "vector",
                                   years = "vector"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  DefineAreaCodePosition = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "AreaCodePosition",
    functionParameterType = list(resourceFilePath = "character"),
    functionParameterFormat = list(resourceFilePath = "filePaths"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  DefineCarNeighbours = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "CarNeighbours",
    functionParameterType = list(resourceFilePath = "character"),
    functionParameterFormat = list(resourceFilePath = "filePaths"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  DefineAgeErrorMatrix  = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "AgeErrorMatrix",
    functionParameterType = list(resourceFilePath = "character"),
    functionParameterFormat = list(resourceFilePath = "filePaths"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  DefineClassificationError  = list(
    functionType = "processData",
    functionCategory = "Baseline",
    functionOutputDataType = "ClassificationError",
    functionParameterType = list(resourceFilePath = "character"),
    functionParameterFormat = list(resourceFilePath = "filePaths"),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  AppendGearStoxBiotic  = list(
    functionType = "modelData",
    functionCategory = "Baseline",
    functionOutputDataType = "StoxBioticData",
    functionParameterType = list(StoxBioticData = "character",
                                 UnifiedVariableDefinition = "character"),
    functionParameterFormat = list(),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  AppendGearStoxLanding  = list(
    functionType = "modelData",
    functionCategory = "Baseline",
    functionOutputDataType = "StoxLandingData",
    functionParameterType = list(StoxBioticData = "character",
                                 UnifiedVariableDefinition = "character"),
    functionParameterFormat = list(),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  AppendTemporalStoxLanding  = list(
    functionType = "modelData",
    functionCategory = "Baseline",
    functionOutputDataType = "StoxLandingData",
    functionParameterType = list(StoxLandingData = "character",
                                 TemporalDefinition = "character"),
    functionParameterFormat = list(),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  AppendPositionLanding = list(
    functionType = "modelData",
    functionCategory = "Baseline",
    functionOutputDataType = "StoxLandingData",
    functionParameterType = list(StoxLandingData = "character",
                                 AreaCodePosition = "character",
                                 resolution = "character"),
    functionParameterFormat = list(),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  AppendStratumStoxLanding = list(
    functionType = "modelData",
    functionCategory = "Baseline",
    functionOutputDataType = "StoxLandingData",
    functionParameterType = list(StoxLandingData = "character",
                                 StratumPolygon = "character"),
    functionParameterFormat = list(),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  PrepareRecaEstimate = list(
    functionType = "modelData",
    functionCategory = "Analysis",
    functionOutputDataType = "RecaData",
    functionParameterType = list(StoxBioticData = "character",
                                 StoxLandingData = "character",
                                 fixedEffects = "character",
                                 randomEffects = "character",
                                 continousEffects = "character",
                                 carEffect = "character",
                                 CarNeighbours = "character",
                                 AgeErrorMatrix = "character",
                                 stockSplitting = "logical",
                                 ClassificationError = "character",
                                 minAge = "integer",
                                 maxAge = "integer",
                                 maxLength = "numeric",
                                 lengthResolution = "numeric",
                                 temporalResolution = "character",
                                 hatchDay = "integer"),
    functionParameterFormat = list(
      fixedEffects = "vector",
      randomEffects = "vector",
      continousEffects = "vector"
    ),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  ),

  RunRecaEstimate = list(
    functionType = "modelData",
    functionCategory = "Analysis",
    functionOutputDataType = "RecaData",
    functionParameterType = list(RecaData = "character",
                                 nSamples = "integer",
                                 burnin = "integer",
                                 lgamodel = "character",
                                 thin = "integer",
                                 delta.age = "double",
                                 seed = "integer",
                                 caa.burnin = "integer"),
    functionParameterFormat = list(),
    functionArgumentHierarchy = list(),
    functionAlias = list(),
    functionParameterAlias = list(),
    functionParameterValueAilas = list()
  )

)
