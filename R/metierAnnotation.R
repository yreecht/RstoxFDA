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
#'   \item{meshedGear}{logical(), optional, whether the gear is a meshed gear. Should be provided for all or none gears}
#'   \item{lowerMeshSize}{integer(), optional, the lower mesh size to include in this metier.}
#'   \item{upperMeshSize}{integer(), optional, the upper mesh size to include in this metier.}
#'   \item{selectivityDevice}{character(), optional, encoding selectivity device.}
#'   \item{meshedSelectivityDevice}{logical(), optional, encoding selectivity device. Should be provided for all or none selectivity devices}
#'   \item{selDevLowerMeshSize}{integer(), optional, the lower mesh size of selectivity device to include in this metier.}
#'   \item{selDevUpperMeshSize}{integer(), optional, the upper mesh size of selectivity device to include in this metier.}
#'  }
#'
#'  The metier-defining parameters are written in camelCase, parameters that may be used to distinguish applicability of different metierdefinitions are writter in UPPER case.
#'
#' @name MetierTable
#'
NULL

#' Check if table is correctly formatted metier table
#' @param table
#' @param throwError if set errors are raised, if not, validity will be returned as T/F
#' @return validity
#' @noRd
is.MetierTable <- function(table, throwError=F){

  if (!data.table::is.data.table(table)){
    if (throwError){
      stop("A metiertable must be a data.table")
    }
    return(FALSE)
  }
  if (!all(c("metier", "gearcode", "target", "meshedGear", "lowerMeshSize", "upperMeshSize", "selectivityDevice", "meshedSelectivityDevice", "selDevLowerMeshSize", "selDevUpperMeshSize") %in% names(table))){
    if (throwError){
      stop("Metiertable does not have the required columns")
    }
    return(FALSE)
  }
  if (!is.character(table$metier)){
    if (throwError){
      stop("The column 'metier' must be a character")
    }
    return(FALSE)
  }
  if (!is.character(table$gearcode)){
    if (throwError){
      stop("The column 'gearcode' must be a character")
    }
    return(FALSE)
  }
  if (!is.logical(table$meshedGear)){
    if (throwError){
      stop("The column 'meshedGear' must be a logical")
    }
    return(FALSE)
  }
  if (!is.integer(table$lowerMeshSize)){
    if (throwError){
      stop("The column 'lowerMeshSize' must be an integer")
    }
  }
  if (!is.integer(table$upperMeshSize)){
    if (throwError){
      stop("The column 'upperMeshSize' must be an integer")
    }
    return(FALSE)
  }
  if (!is.character(table$selectivityDevice)){
    if (throwError){
      stop("The column 'selectivityDevice' must be a character")
    }
    return(FALSE)
  }
  if (!is.logical(table$meshedSelectivityDevice)){
    if (throwError){
      stop("The column 'meshedSelectivityDevice' must be a logical")
    }
    return(FALSE)
  }
  if (!is.integer(table$selDevLowerMeshSize)){
    if (throwError){
      stop("The column 'selDevLowerMeshSize' must be an integer")
    }
  }
  if (!is.integer(table$selDevUpperMeshSize)){
    if (throwError){
      stop("The column 'selDevUpperMeshSize' must be an integer")
    }
    return(FALSE)
  }

  #stop("Implement check on meshed gear / selectivitydevice. Given for all non-na gear / selectivitydevices, if given")

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

  columns <- c("metier", "gearcode", "target", "meshedGear", "lowerMeshSize", "upperMeshSize", "selectivityDevice", "meshedSelectivityDevice", "selDevLowerMeshSize", "selDevUpperMeshSize")
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
                                      selDevUpperMeshSize=as.integer(mettab$selDevUpperMeshSize)
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
checkMetierTable <- function(metiertable, target=!is.null(targetColumn), meshSize=!is.null(meshSizeColumn), selDev=!is.null(selectivityDeviceColumn), selDevMeshSize=!is.null(selectivityDeviceMeshSizeColumn)){
  if (!is.MetierTable(metiertable, T)){
    stop("The provided metiertable is not correctly formatted (RstoxFDA::MetierTable).")
  }

  #duplicatecheck
  metstring <- metiertable$gearcode
  if (target){
    metstring <- paste(metstring, metiertable$target, sep="_")
  }
  if (meshSize){
    metstring <- paste(metstring, metiertable$lowerMeshSize, metiertable$upperMeshSize, sep="_")
  }
  if (selDev){
    metstring <- paste(metstring, metiertable$selectivityDevice, sep="_")
  }
  if (selDevMeshSize){
    metstring <- paste(metstring, metiertable$selDevLowerMeshSize, metiertable$selDevUpperMeshSize, sep="_")
  }

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
  meshedGears$range <- paste(meshedGears$lowerMeshSize, meshedGears$upperMeshSize, sep="-")
  for (g in unique(meshedGears$gearcode)){
    ranges <- meshedGears[meshedGears$gearcode == g,]
    ranges <- ranges[!duplicated(ranges$range),]

    lower <- ranges$lowerMeshSize
    upper <- ranges$upperMeshSize

    ord <- order(lower)
    lower <- lower[ord]
    upper <- upper[ord]

    incr <- c()
    for (i in 1:length(lower)){
      incr <- c(incr, lower[i], upper[i])
    }

    if (any(order(incr) != 1:(2*length(lower)))){
      stop(paste("Mesh sizes have overlapping ranges for gear", g))
    }

  }

  # check that seldev mesh sizes dont overlap for same gear and selection device
  meshedSeldev <- metiertable[metiertable$meshedSelectivityDevice,]
  meshedSeldev$range <- paste(meshedSeldev$selDevLowerMeshSize, meshedSeldev$selDevUpperMeshSize, sep="-")
  meshedSeldev$gs <- paste(meshedSeldev$gearcode, meshedSeldev$selectivityDevice, sep="/")
  for (g in unique(meshedSeldev$gs)){
    ranges <- meshedSeldev[meshedSeldev$gs == g,]
    ranges <- ranges[!duplicated(ranges$range),]

    lower <- ranges$selDevLowerMeshSize
    upper <- ranges$selDevUpperMeshSize

    ord <- order(lower)
    lower <- lower[ord]
    upper <- upper[ord]

    incr <- c()
    for (i in 1:length(lower)){
      incr <- c(incr, lower[i], upper[i])
    }

    if (any(order(incr) != 1:(2*length(lower)))){
      stop(paste("Mesh sizes have overlapping ranges for selectivity device on a gear:", g))
    }

  }
}

#' Check if provided gears are OK to use with provided metiertable
#' @noRd
checkGear <- function(gearVector, metiertable){
  missing <- gearVector[!is.na(gearVector) & !(gearVector %in% metiertable$gearcode)]
  if (length(missing) > 0){
    stop(paste("Metier is not defined for all gears. Missing: ", paste(missing, collapse=",")))
  }
}

#' Check if provided target are OK to use with provided metiertable
#' @noRd
checkTarget <- function(targetVector, metiertable){
  missing <- targetVector[!is.na(targetVector) & !(targetVector %in% metiertable$target)]
  if (length(missing) > 0){
    stop(paste("Metier is not defined for all targets. Missing: ", paste(missing, collapse=",")))
  }
}

#' @noRd
checkSelectivityDevice <- function(selectivityDeviceVector, metiertable){
  missing <- selectivityDeviceVector[!is.na(selectivityDeviceVector) & !(selectivityDeviceVector %in% metiertable$selectivityDevice)]
  if (length(missing) > 0){
    stop(paste("Metier is not defined for all selectivity devices Missing: ", paste(missing, collapse=",")))
  }
}

#' Annotate metier
#' @description annotates metier to data table
#' @details
#'  For the metier-defining variables 'gearcode', 'target' and 'selectivityDevice' (see \code{\link[RstoxFDA]{MetierTable}}), missing values are matches with missing values.
#'  That is, a metier that is defined with a missing value for 'target'
#'  will be annotated to otherwise matching data which has a missing value in the 'targetColumn'.
#'
#' @param data \code{\link[data.table]{data.table}} with data to be annotated
#' @param metiertable \code{\link[RstoxFDA]{MetierTable}} with metier definitions. Accepts all NAs for columns not used.
#' @param gearColumn character() identifies the column in 'data' that encodes gear. Gear definition must match metiertable$gearcode
#' @param targetColumn character(), optional, identifies the column in 'data' that encodes target species. Definition must match metiertable$target
#' @param meshSizeColumn integer(), optional, identifies the column in 'data' that encodes the mesh size of the gear.
#' @param selectivityDeviceColumn character(), optional, identifies the column in 'data' that encodes selectivitydevices mounted on gear. Definition must mathc metiertable$selectivityDevice
#' @param selectivityDeviceMeshSizeColumn integer(), optional, identifies the column in 'data' that encodes the mesh size of any mounted selectivity device.
#' @param metierColName character() name of the column that should be appended to 'data'
#' @param strict logical(), whether strict annotation should be applied (halting on missing definitions).
#' @return \code{\link[data.table]{data.table}} 'data' with the column 'metierColName' appended (character).
#' @export
assignMetier <- function(data, metiertable, gearColumn, targetColumn=NULL, meshSizeColumn=NULL, selectivityDeviceColumn=NULL, selectivityDeviceMeshSizeColumn=NULL, metierColName="metier"){

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
  checkMetierTable(metiertable, target=!is.null(targetColumn), meshSize=!is.null(meshSizeColumn), selDev=!is.null(selectivityDeviceColumn), selDevMeshSize=!is.null(selectivityDeviceMeshSizeColumn))

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

  #annotate
  for (i in 1:nrow(metiertable)){

    selection <- !is.na(data[[gearColumn]]) & (data[[gearColumn]] == metiertable$gearcode[i])
    if (!is.null(targetColumn)){
      if (is.na(metiertable$target[i])){
        selection <- selection & is.na(data[[targetColumn]])
      }
      else{
        selection <- selection & !is.na(data[[targetColumn]]) & data[[targetColumn]] == metiertable$target[i]
      }
    }
    if (!is.null(selectivityDeviceColumn)){
      if (is.na(metiertable$selectivityDevice[i])){
        selection <- selection & is.na(data[[selectivityDeviceColumn]])
      }
      else{
        selection <- selection & !is.na(data[[selectivityDeviceColumn]]) & data[[selectivityDeviceColumn]] == metiertable$selectivityDevice[i]
      }
    }
    if (!is.null(meshSizeColumn) & !is.na(metiertable$gearcode[i]) & metiertable$meshedGear[i]){
      selection <- selection & !is.na(data[[meshSizeColumn]]) & (data[[meshSizeColumn]] <= metiertable$upperMeshSize) & (data[[meshSizeColumn]] >= metiertable$lowerMeshSize)
    }
    if (!is.null(selectivityDeviceMeshSizeColumn) & !is.na(metiertable$gearcode[i]) & !is.na(metiertable$selectivityDevice[i]) & metiertable$meshedSelectivityDevice[i]){
      stop("Deal with NA in selectivityDeviceMeshSizeColumn")
      selection <- selection & !is.na(data[[selectivityDeviceMeshSizeColumn]]) & (data[[selectivityDeviceMeshSizeColumn]] <= metiertable$selDevUpperMeshSize) & (data[[selectivityDeviceMeshSizeColumn]] >= metiertable$selDevLowerMeshSize)
    }

    data[selection, metierColName] <- metiertable$metier[i]
  }

  #report any missing metier definitions
  missingMetier <- data[is.na(data[[metierColName]]),]
  if (nrow(missingMetier) > 0){
    cols <- c(gearColumn, targetColumn, selectivityDeviceColumn, meshSizeColumn, selectivityDeviceMeshSizeColumn)
    usedCols <- cols[cols %in% names(data)]
    missingMetier <- unique(missingMetier[,..usedCols])
    for (i in 1:nrow(missingMetier)){
      message(paste("Missing metier definition for:", paste(missingMetier[1,], collapse=" ")))
    }
    stop("Not all rows could be assigned a metier.")
  }

  return(data)
}
