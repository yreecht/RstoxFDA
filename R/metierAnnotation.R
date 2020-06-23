#' Metier table
#'
#' Table (\code{\link[data.table]{data.table}}) defining metiers.
#'
#' Fishing activity metiers are approximate decompositions of fleets commonly used in Europe.
#' This table defines an approximate definition of metiers by a custom gear code.
#'
#' Metiers are used in EU-regulations for EU-member states, and are therefore often required by ICES data formats
#' Metiers are formal strings encoding decomposition an idealized fleet or set of trips, where target species and gear are clearly and unambigiously identified.
#' The metier system recognize that these parameters are not always clearly and unamibigiously identified to the desired resolution, and allows for grouping and omission of some parameters, and for coding that information is missing.
#' This makes the metier system very flexible, but also provides poor standardization and ICES databases and data-calls may provide code-lists of allowed metiers, sometimes with different metiers being requested for different areas.
#' In effect metier annotations cannot be completely standardized, but must to be configurable through conversion tables like this. Often the annotation has to be approximate.
#'
#' The metier system is spesified by the EU - Data Collection Framework: \url{https://datacollection.jrc.ec.europa.eu/wordef/fishing-activity-metier},
#' briefly a metier is fully specified by a string: <gear>_<target species>_<gear mesh size>_<selectivity device>_<selectivity device mesh size>_<vessel length class>,
#' with coding systems and grouping conventions defined for each of the variables.
#' Common trunctations of these strings are also used. E.g.:
#' Metier level 6 (no truncation): <gear>_<target species>_<gear mesh size>_<selectivity device>_<selectivity device mesh size>
#' Metier level 5 : <gear>_<target species>
#' Metier level 4 : <gear>
#' The term "metier" is also used for some derived systems, for instance some data-calls requests that a code for intended usage of catch (industrial vs human consumption) be appended to the metiers where gear and target species is missing.
#'
#' For example, in intercatch, the code OTB_DEF_>=120_0_0_all identifies bottom trawl with otter boards (OTB) targeting demershal fish (DEF), with mesh size larger than or equal to 120 mm, no selectivity device (0_0) and all vessel size (all)
#'
#' @details
#'  \describe{
#'   \item{metier}{character() metier-string, e.g.: OTB_DEF_>=120_0_0_all}
#'   \item{gearcode}{character() encoding gear}
#'   \item{target}{character(), optional, target species}
#'   \item{meshedGear}{logical(), optional, whether the gear is a meshed gear}
#'   \item{lowerMeshSize}{integer(), optional, the lower mesh size to include in this metier.}
#'   \item{upperMeshSize}{integer(), optional, the upper mesh size to include in this metier.}
#'   \item{selectivityDevice}{character(), optional, encoding selectivity device.}
#'   \item{meshedSelectivityDevice}{logical(), optional, encoding selectivity device.}
#'   \item{selDevLowerMeshSize}{integer(), optional, the lower mesh size of selectivity device to include in this metier.}
#'   \item{selDevUpperMeshSize}{integer(), optional, the upper mesh size of selectivity device to include in this metier.}
#'   \item{AREA}{character(), optional, encoding any area the metier is restricted to. NA indicates that the metier in defined for all areas.}
#'  }
#'
#'  The metier-defining parameters are written in camelCase, parameters that may be used to distinguish applicability of different metierdefinitions are writter in UPPER case.
#'
#' @name MetierTable
#'
NULL

is.MetierTable <- function(table){

  if (!is.data.table(table)){
    return(FALSE)
  }
  if (!all(c("metier", "gearcode", "target", "meshedGear", "lowerMeshSize", "upperMeshSize", "selectivityDevice", "meshedSelectivityDevice", "selDevLowerMeshSize", "selDevUpperMeshSize", "AREA") %in% names(table))){
    return(FALSE)
  }
  if (is.character(table$metier)){
    return(FALSE)
  }
  if (is.character(table$gearcode)){
    return(FALSE)
  }
  if (is.logical(table$meshedGear)){
    return(FALSE)
  }
  if (is.integer(table$lowerMeshSize)){
    return(FALSE)
  }
  if (is.integer(table$upperMeshSize)){
    return(FALSE)
  }
  if (is.character(table$selectivityDevice)){
    return(FALSE)
  }
  if (is.logical(table$meshedSelectivityDevice)){
    return(FALSE)
  }
  if (is.integer(table$selDevLowerMeshSize)){
    return(FALSE)
  }
  if (is.integer(table$selDevUpperMeshSize)){
    return(FALSE)
  }
  if (is.character(table$AREA)){
    return(FALSE)
  }

  return(TRUE)
}

#' Read metier table
#' @description Reads a table of metier definitions.
#' @details
#'  The file identified by 'filename' must be a tab-separated file, and must provided headers which must match column names in \code{\link[RstoxFDA]{MetierTable}}
#'  Optional columns may be omitted. They will be interpreted as NA.
#'  Comments may be provided on lines with a leading '#'.
#'  Logical values ('meshedGear' and 'meshedSelectivityDevice') should be encoded with 'T' for true and 'F' for false.
#' @param filename character() path to file that contains metier definitions. See details for format.
#' @param encoding The character encoding of the file identified by 'filename'
#' @return \code{\link[RstoxFDA]{MetierTable}} containing metier definitons.
readMetierTable <- function(filename, encoding="UTF8"){
  loc <- readr::locale()
  loc$encoding <- encoding
  suppressMessages(mettab <- readr::read_delim(filename, delim="\t", comment = "#", locale = loc, trim_ws = T, skip_empty_rows = T, na = c("")))

  columns <- c("metier", "gearcode", "target", "meshedGear", "lowerMeshSize", "upperMeshSize", "selectivityDevice", "meshedSelectivityDevice", "selDevLowerMeshSize", "selDevUpperMeshSize", "AREA")
  for (co in columns){
    if (is.null(mettab[[co]])){
      mettab[[co]] <- NA
    }
  }

  mettab_dt <- data.table::data.table(metier=as.character(mettab$metier),
                                      gearcode=as.character(mettab$gearcode),
                                      target=as.character(mettab$target),
                                      meshedGear=as.logical(mettab$meshedGear),
                                      lowerMeshSize=as.integer(mettab$lowerMeshSize),
                                      upperMeshSize=as.integer(mettab$upperMeshSize),
                                      selectivityDevice=as.character(mettab$selectivityDevice),
                                      meshedSelectivityDevice=as.logical(mettab$meshedSelectivityDevice),
                                      selDevLowerMeshSize=as.integer(mettab$selDevLowerMeshSize),
                                      selDevUpperMeshSize=as.integer(mettab$selDevUpperMeshSize),
                                      AREA=as.character(mettab$AREA)
                                      )

  nonmapped <- names(mettab)[!names(mettab) %in% names(mettab_dt)]
  if (length(nonmapped) > 0){
    stop(paste("Some column names are not recognized:", paste(nonmapped, collapse=",")))
  }

  return(mettab_dt)

}

#' checks metier table
#' @description checks that metiers are uniquely defined and that mesh-size ranges dont overlap
#' @param metiertable \code{\link[RstoxFDA]{MetierTable}}
#' @noRd
checkMetierTable <- function(metiertable){
  if (!is.MetierTable(metiertable)){
    stop("The provided metiertable is not correctly formatted (RstoxFDA::MetierTable).")
  }

  metstring <- paste(metiertable$gearcode, metiertable$target, metiertable$lowerMeshSize, metiertable$upperMeshSize, metiertable$selectivityDevice, metiertable$selDevLowerMeshSize, metiertable$selDevUpperMeshSize, sep="_")

  duplicates <- unique(metiertable$metier[duplicated(metstring)])
  if (length(duplicates) > 0){
    stop(paste("Some metiers have duplicate definitions:", paste(duplicates, collapse=",")))
  }

  if (any(is.na(metiertable$meshedGear)) & !all(is.na(metiertable$meshedGear))){
    stop("The parameter 'meshedGear' is only provided for some gears")
  }
  if (any(!is.na(metiertable$selectivityDevice) & is.na(metiertable$meshedSelectivityDevice)) &
      !all(!is.na(metiertable$selectivityDevice) & is.na(metiertable$meshedSelectivityDevice))){
    stop("The parameter 'meshedSelectivityDevice' is only provided for some selectivity devices.")
  }

  meshedGears <- unique(metiertable$gearcode[metiertable$meshedGear])
  nonMeshedGears <- unique(metiertable$gearcode[!metiertable$meshedGear])
  meshedConflict <- intersect(meshedGears, nonMeshedGears)

  if (length(meshedConflict) > 0){
    stop(paste("Some gear codes are listed both as meshed gears and non-meshed gears", paste(meshedConflict, collapse=",")))
  }

  selDevGears <- paste(metiertable$gearcode, metiertable$selectivityDevice, sep="_")
  meshedSelDev <- unique(selDevGears[metiertable$meshedSelectivityDevice])
  nonMeshedSelDev <- unique(selDevGears[!metiertable$meshedSelectivityDevice])
  meshedSelDevConflict <- intersect(meshedSelDev, nonMeshedSelDev)

  if (length(meshedConflict) > 0){
    stop(paste("Some selectivity devices are listed both as meshed gears and non-meshed gears", paste(meshedSelDevConflict, collapse=",")))
  }

  # check that mesh sizes dont overlap for same gear
  meshedGears <- metiertable[metiertable$meshedGear,]
  stop("Implement check for overlapping mesh ranges")

  # check that seldev mesh sizes dont overlap for same gear and selection device
  stop("Implement check for overlapping seldev mesh ranges")
}

#' Check if provided gears are OK to use with provided metiertable
#' @noRd
checkGear <- function(gearVector, metiertable){
  missing <- gearVector[!(gearVector %in% metiertable$gearcode)]
  if (length(missing) > 0){
    stop(paste("Metier is not defined for all gears. Missing: ", paste(missing, collapse=",")))
  }
}

#' Check if provided target are OK to use with provided metiertable
#' @noRd
checkTarget <- function(targetVector, metiertable){
  missing <- targetVector[!(targetVector %in% metiertable$target)]
  if (length(missing) > 0){
    stop(paste("Metier is not defined for all gears. Missing: ", paste(missing, collapse=",")))
  }
}

#' Issue warning if not all metier-areas exist in data
#' @noRd
checkArea <- function(areaVector, metiertable){
  missing <- metiertable$AREA[!(metiertable$AREA %in% areaVector)]
  if (length(missing)>0){
    warning(paste("Area specific metier defintions provided for areas not in data, areas:", paste(missing, collapse=",")))
  }
}

#' Check if provided mesh sizes are OK to use with provided metiertable
#' @noRd
checkMeshSize <- function(gearvector, meshSizeVector, metiertable){

}


#' Annotate metier
#' @description annotates metier to data table
#' @details
#'  The parameter 'areaColumn' may be NULL which specifies that metiers are not dependent on area for the annotation.
#'  If areaColumn is given, NA values in 'metiertable' specifies that metiers apply to all areas, so that area codes need only be provided for exceptions to this rule.
#' @param data \code{\link[data.table]{data.table}} with data to be annotated
#' @param metiertable \code{\link[RstoxFDA]} with metier definitions. Accepts all NAs for columns not used.
#' @param gearColumn character() identifies the column in 'data' that encodes gear. Gear definition must match metiertable$gearcode
#' @param targetColumn character(), optional, identifies the column in 'data' that encodes target species. Definition must match metiertable$target
#' @param meshSizeColumn integer(), optional, identifies the column in 'data' that encodes the mesh size of the gear.
#' @param selectivityDeviceColumn character(), optional, identifies the column in 'data' that encodes selectivitydevices mounted on gear. Definition must mathc metiertable$selectivityDevice
#' @param selectivityDeviceMeshSizeColumn integer(), optional, identifies the column in 'data' that encodes the mesh size of any mounted selectivity device.
#' @param metierColName character() name of the column that should be appended to 'data'
#' @param areaColumn character(), optional, identifies the column in 'data' that encodes areas for which different metierdefinitions are applicable.
#' @param strict logical(), whether strict annotation should be applied (halting on missing definitions).
#' @return \code{\link[data.table]{data.table}} 'data' with the column 'metierColName' appended (character).
#' @export
assignMetier <- function(data, metiertable, gearColumn, targetColumn=NULL, meshSizeColumn=NULL, selectivityDeviceColumn=NULL, selectivityDeviceMeshSizeColumn=NULL, areaColumn=NULL, metierColName="metier"){

  if (is.null(data)){
    stop("The parameter 'data' must be provided.")
  }
  if (!data.table::is.data.table(data)){
    warning("Coercing 'data' to data.table")
    data <- data.table::as.data.table(data)
  }

  if (is.null(metiertable)){
    stop("The parameter 'metiertable' must be provided.")
  }
  checkMetierTable(metiertable)

  if (is.null(gearColumn)){
    stop("The parameter 'gearColumn' must be provided.")
  }


  if (is.null(metierColName)){
    stop("The parameter 'metierColName' must be provided.")
  }
  if (metierColName %in% names(data)){
    stop(paste("The column", metierColName, "already exists in the data table 'data'."))
  }

  #check metier configuration
  checkGear(data[[gearColumn]], metiertable)
  if (!is.null(targetColumn)){
    checkTarget(data[[targetColumn]], metiertable)
  }
  if (!is.null(selectivityDeviceColumn)){
    checkSelectivityDevice(data[[selectivityDeviceColumn]], metiertable)
  }
  if (!is.null(areaColumn)){
    checkArea(data[[areaColumn]], metiertable)
  }
  if (!is.null(meshSizeColumn)){
    checkMeshSize(data[[gearColumn]], data[[meshSizeColumn]], metiertable)
  }
  if (!is.null(selectivityDeviceMeshSizeColumn)){
    checkMeshSizeSelDev(data[[gearColumn]], data[[selectivityDeviceColumn]], data[[selectivityDeviceMeshSizeColumn]], metiertable)
  }

  #annotate
  for (i in 1:nrow(metiertable)){
    selection <- data[[gearColumn]] == metiertable$gearcode[i]
    if (!is.null(targetColumn)){
      selection <- selection & data[[targetColumn]] == metiertable$target[i]
    }
    if (!is.null(selectivityDeviceColumn)){
      selection <- selection & data[[selectivityDeviceColumn]] == metiertable$selectivityDevice[i]
    }
    if (!is.null(areaColumn) & !is.na(metiertable$AREA[i])){
      selection <- selection & (data[[areaColumn]] == metiertable$AREA)
    }
    if (!is.null(meshSizeColumn) & metiertable$meshedGear[i]){
      selection <- selection & (data[[meshSizeColumn]] <= metiertable$upperMeshSize) & (data[[meshSizeColumn]] >= metiertable$lowerMeshSize)
    }
    if (!is.null(selectivityDeviceMeshSizeColumn) & metiertable$meshedSelectivityDevice[i]){
      selection <- selection & (data[[selectivityDeviceMeshSizeColumn]] <= metiertable$selDevUpperMeshSize) & (data[[selectivityDeviceMeshSizeColumn]] >= metiertable$selDevLowerMeshSize)
    }
    data[selection, metierColName] <- metiertable$metier[i]
  }

  #report any missing metier definitions
  missingMetier <- data[is.na(data[metierColName]),]
  if (length(missingMetier) > 0){
    cols <- c(gearColumn, targetColumn, selectivityDeviceColumn, areaColumn, meshSizeColumn, selectivityDeviceMeshSizeColumn)
    usedCols <- cols[cols %in% names(data)]
    missingMetier <- unique(missingMetier[,usedCols])
    for (i in 1:nrow(missingMetier)){
      message(paste("Missing metier definition for:", paste(missingMetier[1,], collapse=" ")))
    }
    stop("Not all rows could be assigned a metier.")
  }

  return(data)
}
