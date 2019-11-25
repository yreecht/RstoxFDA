
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

#' Unified Variable Definition (UnifiedVariableDefinition)
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

#' Temporal Categories (TemporalCategories)
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
#' @name TemporalCategories
#'
NULL

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
    functionOutputDataType = "TemporalCategories",
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
    functionOutputDataType = "RstoxBioticData",
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
    functionOutputDataType = "RstoxLandingData",
    functionParameterType = list(StoxBioticData = "character",
                                 UnifiedVariableDefinition = "character"),
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
