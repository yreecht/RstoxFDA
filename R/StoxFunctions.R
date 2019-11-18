#' Read unified categorical definition
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
#'  \item{Column 1: 'CovariateID' or 'UnifiedValue'}{Unified value (key)}
#'  \item{Column 2: 'Source' or 'Format'}{The format (key)}
#'  \item{Column 3: 'Definition'}{A comma separated list of values, any of which will be defined as the unified value in the format.}
#'  }
#' @param filepath path to resource file
#' @param encoding encoding of resource file
#' @param formats the formats to extract definitions for, if NULL all formats will be extracted.
#' @import readr
#' @return list() with one member for each format, each a list mapping codes in that format to the unified variable.
readUnifiedDefinition <- function(filepath, encoding="ascii", formats=NULL){
  loc <- readr::default_locale()
  loc$encoding <- encoding
  tab <- readr::read_delim(filepath, delim = "\t", locale = loc, col_types = "ccc")

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
