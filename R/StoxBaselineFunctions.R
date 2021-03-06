

#' Checks symmetry of Car table
#' @noRd
checkSymmetry <- function(tab){

  #to get data.table notation throguh check
  CarValue <- Neighbours <- NULL

  getn <- function(value){
    neighbours <- trimws(unlist(strsplit(tab[CarValue==value,Neighbours], split = ",")))
    return(neighbours)
  }

  for (i in 1:nrow(tab)){
    carvalue <- tab[i,1]
    neighbours <- getn(carvalue)
    for (n in neighbours){
      if (!(n %in% tab[["CarValue"]]) | !(carvalue %in% getn(n))){
        stop(paste("Neighbour definition not symmetric.", n, "is neighbour of", carvalue, "but not vice versa."))
      }
    }
  }
}

#' Get lookup list for unified categorical definition
#' @description
#'  StoX function
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
    ftab <- tab[tab$Source == f,]
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

ConvertLengthBiotic <- function(BioticData){
  stop("Not implemented")
}

ConvertWeightsBiotic <- function(BioticData){
  stop("Not implemented")
}

RedefinePositionStoxBiotic <- function(StoxBioticData){
  stop("Not implemented")
}

#' Append position to landings data
#' @description
#'  StoX function
#'  Appends a position to landings data, based on Area and SubArea codes.
#' @details
#'  When 'resolution' is specified as 'Area' the midpoint of the Area will be assigned to the landings.
#'  When 'resolution' is specified as 'SubArea' the midpoint of the SubArea will be assigned to the landings.
#' @param StoxLandingData landing data, see \code{\link[RstoxData]{StoxLandingData}}
#' @param AreaCodePosition coordinates for Area and SubArea codes, see \code{\link[RstoxFDA]{AreaCodePosition}}
#' @param resolution character(), defaults to Area, specify what resolution to use: 'Area' or 'SubArea'. See details.
#' @param latColName character(), defaults to Latitude, name of the latitude column that will be appended.
#' @param lonColName character(), defaults to Longitude, name of the longitude column that will be appended.
#' @return \code{\link[RstoxData]{StoxLandingData}} with columns for latitude and longitude appended.
#' @export
AppendPositionLanding <- function(StoxLandingData, AreaCodePosition, resolution = c("Area", "SubArea"), latColName="Latitude", lonColName="Longitude"){

  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.AreaCodePosition(AreaCodePosition))

  if (latColName %in% names(StoxLandingData)){
    stop(paste("Column", latColName, "already exists."))
  }
  if (lonColName %in% names(StoxLandingData)){
    stop(paste("Column", lonColName, "already exists."))
  }

  resolution <- match.arg(resolution, resolution)

  AreaCodePosition[[latColName]] <- AreaCodePosition$Latitude
  AreaCodePosition[[lonColName]] <- AreaCodePosition$Longitude

  if (resolution == "Area"){
    if (!all(StoxLandingData$Area %in% AreaCodePosition$Area)){
      missing <- StoxLandingData$Area[!(StoxLandingData$Area %in% AreaCodePosition$Area)]
      stop(paste("Positions not provided for all Areas. Missing: ", paste(missing, collapse=",")))
    }
    AreaCodePosition <- AreaCodePosition[,c("Area", latColName, lonColName), with=F]
    return(data.table::as.data.table(merge(StoxLandingData, AreaCodePosition, by.x="Area", by.y="Area", all.x=T)))
  }
  else if (resolution == "SubArea"){
    arealocdata <- paste(StoxLandingData$Area, StoxLandingData$SubArea, sep="-")
    arealocresource <- paste(AreaCodePosition$Area, AreaCodePosition$SubArea, sep="-")
    if (!all(arealocdata %in% arealocresource)){
      missing <- arealocdata[!(arealocdata %in% arealocresource)]
      stop(paste("Positions not provided for all Areas and SubArea Missing: ", paste(missing, collapse=",")))
    }
    AreaCodePosition <- AreaCodePosition[,c("Area", "SubArea", latColName, lonColName), with=F]
    return(data.table::as.data.table(merge(StoxLandingData, AreaCodePosition, by.x=c("Area", "SubArea"), by.y=c("Area", "SubArea"), all.x=T)))
  }
  else{
    stop(paste("Resolution", resolution, "not supported"))
  }

}

#'
#' @param dateColumns vector of date-columns to try in order.
#' @noRd
appendTemporal <- function(table, temporalColumn, temporalDefinition, datecolumns){
  stopifnot(is.TemporalDefinition(temporalDefinition))

  if (temporalColumn %in% names(table)){
    stop(paste("Temporal column", temporalColumn, "exists already."))
  }

  if (!(all(is.na(temporalDefinition$year))) & any(is.na(temporalDefinition$year))){
    stop("Year is provided for some, but not all temporal definitions.")
  }
  dateCol <- as.POSIXct(rep(NA, nrow(table)))

  for (d in datecolumns){
    if (!is.POSIXct(table[[d]])){
      stop("Error. Invalid date format. Use POSIXct.")
    }
    filter <- is.na(dateCol) & !is.na(table[[d]])
    dateCol[filter] <- table[[d]][filter]
  }

  if (any(is.na(dateCol))){
    stop("NA for some dates")
  }

  month <- as.integer(strftime(dateCol, format="%m"))
  day <- as.integer(strftime(dateCol, format="%d"))
  year <- as.integer(strftime(dateCol, format="%Y"))

  if (!(all(is.na(temporalDefinition$year))) & !(all(year %in% temporalDefinition$year))){
    stop("Year is provided in temporal definitions, but does not contain definitions for all years in data.")
  }

  temporalDefinition <- temporalDefinition[order(temporalDefinition$year, temporalDefinition$startMonth, temporalDefinition$startDay, decreasing = F),]

  temporalCategory <- rep(NA, nrow(table))

  if (all(is.na(temporalDefinition$year))){
    filt <- (month < temporalDefinition$startMonth[1] | (month == temporalDefinition$startMonth[1] & day < temporalDefinition$startDay[1]))
    temporalCategory[filt] <- temporalDefinition$temporalCategory[nrow(temporalDefinition)]

    for (i in 1:nrow(temporalDefinition)){
      filt <- (month > temporalDefinition$startMonth[i] | (month == temporalDefinition$startMonth[i] & day >= temporalDefinition$startDay[i]))
      temporalCategory[filt] <- temporalDefinition$temporalCategory[i]
    }

  }
  else if (all(!is.na(temporalDefinition$year))){

    if (any(year < temporalDefinition$year[1] |
            year == temporalDefinition$year[1] & month < temporalDefinition$startMonth[1] |
            (year == temporalDefinition$year[1] & month == temporalDefinition$startMonth[1] & day < temporalDefinition$startDay[1]))){
      stop("Some dates preced the first temporal category.")
    }

    for (i in 1:nrow(temporalDefinition)){
      filt <- (year > temporalDefinition$year[i] |
                 (year == temporalDefinition$year[i] & month > temporalDefinition$startMonth[i]) |
                 (year == temporalDefinition$year[i] & month == temporalDefinition$startMonth[i] & day >= temporalDefinition$startDay[i]))
      temporalCategory[filt] <- temporalDefinition$temporalCategory[i]
    }
  }
  else{
    stop()
  }


  table[,temporalColumn] <- temporalCategory

  return(table)
}

#' Append Temporal Categories to StoxLandingData
#' @description
#'  StoX function
#'  Appends a column to StoxLandingData with a temporal category, such as 'quarter',
#'  that are also defined for for other formats, such as StoxBioticData.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param TemporalDefinition \code{\link[RstoxFDA]{TemporalDefinition}} definiton of temporal category.
#' @param columnName character(), defaults to 'TemporalCategory', name of the appended column.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AppendTemporalStoxLanding <- function(StoxLandingData, TemporalDefinition, columnName="TemporalCategory"){
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.TemporalDefinition(TemporalDefinition))
  return(appendTemporal(StoxLandingData, columnName, TemporalDefinition, "CatchDate"))
}


AppendTemporalStoxBiotic <- function(StoxBioticData, TemporalDefinition, columnName="TemporalCategory"){
  stop("Not implemented")
}

#' @param table data.table to be annotated.
#' @param areaPolygons sp::SpatialPolygons with strata names in ID slot
#' @param latName name of WGS84 lat column
#' @param lonName name of WGS84 lon column
#' @param colName name of column to be appended
#' @noRd
appendAreaCode <- function(table, areaPolygons, latName, lonName, colName){
  if (colName %in% names(table)){
    stop(paste("Column name", colName, "already exists."))
  }

  stratanames <- sapply(methods::slot(areaPolygons, "polygons"), function(x) methods::slot(x, "ID"))
  stratanames.df <- data.frame( ID=1:length(areaPolygons), row.names = stratanames)
  areaPolygons <- sp::SpatialPolygonsDataFrame(areaPolygons, stratanames.df)
  pos <- as.data.frame(table[,c(latName, lonName), with=F])
  names(pos) <- c("LAT", "LON")
  sp::coordinates(pos) <- ~ LON + LAT
  sp::proj4string(pos) <- sp::CRS("+proj=longlat +datum=WGS84")

  if (!sp::identicalCRS(pos, areaPolygons)){
    stop(paste("CRS:", sp::proj4string(areaPolygons), "not supported."))
  }

  location_codes <- sp::over(pos, areaPolygons)
  table[[colName]] <- stratanames[location_codes$ID]

  return(table)
}

#' Appends Stratum to StoxBioticData
#' @description
#'  StoX function
#'  Appends a column to StoxBioticData with the spatial strata each row belongs to
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated.
#' @param StratumPolygon definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
#' @param columnName character(), defaults to 'Stratum', name of the appended column
#' @return StoxBioticData with column appended. See \code{\link[RstoxData]{StoxBioticData}}.
AppendStratumStoxBiotic <- function(StoxBioticData, StratumPolygon, columnName="Stratum"){
  if (columnName %in% names(StoxBioticData)){
    stop(paste("Column with name '", columnName, "' already exists.", sep=""))
  }
  stop("Not implemented. Remember to export when implemented")
}

#' Appends Stratum to StoxLandingData
#' @description
#'  StoX function
#'  Appends a column to StoxLandingData with the spatial strata each row belongs to.
#' @details
#'  \code{\link[RstoxData]{StoxLandingData}} does not contain columns for positions,
#'  these need to be appended before calling this function, and identified with the parameters 'latColumn' and 'lonColumn'.
#'  \code{\link[RstoxFDA]{AppendPositionLanding}} may be used to append positions.
#' @seealso \code{\link[RstoxFDA]{AppendPositionLanding}} for appending positions to \code{\link[RstoxData]{StoxLandingData}}.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated. Needs postions appended. See details.
#' @param StratumPolygon definition of spatial strata. See \code{\link[RstoxBase]{StratumPolygon}}
#' @param columnName character(), defaults to 'Stratum', name of the appended column.
#' @param latColumn character(), defaults to 'Latitdue', identifies the column in StoxLandingData with latitudes.
#' @param lonColumn character(), defaults to 'Longitude', identifies the column in StoxLandingData with longitudes.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AppendStratumStoxLanding <- function(StoxLandingData, StratumPolygon, columnName="Stratum", latColumn="Latitude", lonColumn="Longitude"){
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  if (!(all(c(latColumn, lonColumn) %in% names(StoxLandingData)))){
    stop(paste("Could not find appended columns:", latColumn, "and", lonColumn, "on StoxLandingData"))
  }
  if (columnName %in% names(StoxLandingData)){
    stop(paste("Column name", columnName, "already exists."))
  }

  return(appendAreaCode(StoxLandingData, StratumPolygon, latColumn, lonColumn, columnName))
}

#' append gear
#' @noRd
appendGear <- function(table, gearcolumn, gearDefinition, colName){

  if (length(unique(gearDefinition$Source)) > 1){
    stop("Error: filter gearDefinition before call")
  }

  if (colName %in% names(table)){
    stop(paste("Column with name '", colName, "' already exists.", sep=""))
  }

  conversionTable <- makeUnifiedDefinitionLookupList(gearDefinition)[[1]]
  table[,colName] <- convertCodes(unlist(table[,gearcolumn,with=F]), conversionTable)

  return(table)
}

###
# Functions for appending columns to data
#

#' Append Gear to StoxBioticData
#' @description
#'  StoX function
#'  Appends a column to StoxBioticData with a unified gear definition
#'  that are also defined for for other formats, such as StoxLandingData.
#' @param StoxBioticData \code{\link[RstoxData]{StoxBioticData}} data which will be annotated.
#' @param UnifiedVariableDefinition \code{\link[RstoxFDA]{UnifiedVariableDefinition}} unified gear definition.
#' @param columnName character(), defaults to 'UnifiedGear', name of the appended column.
#' @return StoxBioticData with column appended. See \code{\link[RstoxData]{StoxBioticData}}.
#' @export
AppendGearStoxBiotic <- function(StoxBioticData, UnifiedVariableDefinition, columnName="UnifiedGear"){
  stopifnot(is.UnifiedVariableDefinition(UnifiedVariableDefinition))
  geardef <- UnifiedVariableDefinition[UnifiedVariableDefinition$Source == "StoxBioticData",]
  return(appendGear(StoxBioticData, "gear", geardef, columnName))
}

#' Append Gear to StoxLandingData
#' @description
#'  StoX function
#'  Appends a column to StoxLandingData with a unified gear definition
#'  that are also defined for for other formats, such as StoxBioticData.
#' @param StoxLandingData \code{\link[RstoxData]{StoxLandingData}} data which will be annotated.
#' @param UnifiedVariableDefinition \code{\link[RstoxFDA]{UnifiedVariableDefinition}} unified gear definition.
#' @param columnName character(), defaults to 'UnifiedGear', name of the appended column.
#' @return StoxLandingData with column appended. See \code{\link[RstoxData]{StoxLandingData}}.
#' @export
AppendGearStoxLanding <- function(StoxLandingData, UnifiedVariableDefinition, columnName="UnifiedGear"){
  stopifnot(RstoxData::is.StoxLandingData(StoxLandingData))
  stopifnot(is.UnifiedVariableDefinition(UnifiedVariableDefinition))
  geardef <- UnifiedVariableDefinition[UnifiedVariableDefinition$Source == "StoxLandingData",]
  return(appendGear(StoxLandingData, "Gear", geardef, columnName))
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
#'  StoX function
#'  Define temporal categories for grouping data based on date.
#' @details
#'  Not providing years, has the effect of making the defintion seasonal, independent of year,
#'  so that e.g. Q1 in 2015 is considered the same category as Q1 in 2016
#' @param processData data.table() as returned from this function
#' @param temporalCategory character(), defaults to 'Quarter', type of temporal category: 'Quarter', 'Month' or 'Custom'
#' @param customPeriods character(), provided if temporalCategory is 'Custom', vector of strings formatted as DD-MM, giving the start date of each temporal category.
#' @param years integer() vector, optional, provide if defintion should be non-seasonal.
#' @param encoding encoding of resource file
#' @param useProcessData logical() Bypasses execution of function, and simply returns argument 'processData'
#' @return Temporal Categories, see: \code{\link[RstoxFDA]{TemporalDefinition}}.
#' @export
DefineTemporalCategories <- function(processData, temporalCategory=c("Quarter", "Month", "Custom"), customPeriods = NULL,  years = NULL, encoding="UTF-8", useProcessData=F){

  if (useProcessData){
    return(processData)
  }

  temporalCategory <- match.arg(temporalCategory, temporalCategory)

  if (length(customPeriods)>0 & temporalCategory != "Custom"){
    stop(paste("Custom period provided, but temporalCategory is", temporalCategory))
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
        year=as.integer(rep(NA, 12))
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

    ord <- order(months, days, decreasing = F)
    months <- months[ord]
    days <- days[ord]

    startstr <- customPeriods


    if (months[1]==1 & days[1] == 1){
        endstr <- c(customPeriods[2:length(customPeriods)], "-----")
    }
    else {
      endstr <- c(customPeriods[2:length(customPeriods)], customPeriods[1])
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

  if (length(years)>0){
    ncat <- nrow(output)
    out <- output
    out$year <- rep(years[1], ncat)
    out$temporalCategory <- paste(years[1], " ", output$temporalCategory, sep="")
    yearoutput <- out
    output <- yearoutput
  }
  if (length(years)>1){
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

  tab <- readTabSepFile(resourceFilePath, col_types = "ccdd", col_names = c("Area", "SubArea",	"Latitude",	"Longitude"), encoding = encoding)

  missingLoc <- tab[is.na(tab[["SubArea"]]),]

  if (length(unique(missingLoc[["Area"]])) != length(unique(tab[["Area"]]))){
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

  if (length(unique(tab[["CarValue"]])) != nrow(tab)){
    d <- tab[["CarValue"]][duplicated(tab[["CarValue"]])]
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

