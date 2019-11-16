#' Parses landings (sales notes)
#' @description Parses sales notes data from the Norwegian Directorate of Fisheries on the LSS format
#' @param file file with LSS landings
#' @return data.table with LSS landings
#' @import data.table
#' @import readr
#' @export
parseLSS <- function(file){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- "latin1"
  db <- read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, guess_max = 100000)
  return(db)
}
