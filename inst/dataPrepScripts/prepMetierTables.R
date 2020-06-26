library(RstoxFDA)
library(data.table)
#
# prepares example activity census for metier annotation based on logbooks
#
prepMetier4 <- function(){
  metier4table <- data.table(metier=as.character( c("MIS","LA","PS","GN","GNS","LLD","LLS","LHM","LTL","FPO","MIS","OTB","PTB","OTM","PTM","OTB","OTB","OTT","SSC","MIS","MIS")),
                            gearcode=as.character(c(NA,"10","11","20","22", "31", "32", "33", "34", "42", "50", "51", "52", "53", "54", "55", "57", "58", "61", "70","80")),
                            target=as.character(NA),
                            meshedGear=as.logical(NA),
                            lowerMeshSize=as.numeric(NA),
                            upperMeshSize=as.numeric(NA),
                            selectivityDevice=as.character(NA),
                            meshedSelectivityDevice=as.logical(NA),
                            selDevLowerMeshSize=as.numeric(NA),
                            selDevUpperMeshSize=as.numeric(NA))
  RstoxFDA:::is.MetierTable(metier4table)
  RstoxFDA:::checkMetierTable(metier4table)
  usethis::use_data(metier4table, overwrite=T)
}
prepMetier4()

prepMetier5 <- function(){
  metier5table <- data.table(metier=as.character( c("MIS_MIS","LA_DEF","PS_SPF","GND_DEF","GNS_DEF","LLD_DEF","LLS_DEF","LHM_DEF","LTL_DEF","MIS_MIS","MIS_MIS","OTB_DEF","PTB_DEF","OTM_SPF","PTM_SPF","OTB_CRU","OTB_CRU","OTT_DEF","SSC_DEF","MIS_MIS","MIS_MIS")),
                             gearcode=as.character(c(NA,      "10",    "11",    "20",     "22",     "31",      "32",     "33",    "34",     "42",      "50",     "51",     "52",     "53",    "54",     "55",      "57",    "58",      "61",    "70",    "80")),
                             target=as.character(NA),
                             meshedGear=as.logical(NA),
                             lowerMeshSize=as.numeric(NA),
                             upperMeshSize=as.numeric(NA),
                             selectivityDevice=as.character(NA),
                             meshedSelectivityDevice=as.logical(NA),
                             selDevLowerMeshSize=as.numeric(NA),
                             selDevUpperMeshSize=as.numeric(NA))
  RstoxFDA:::is.MetierTable(metier5table)
  RstoxFDA:::checkMetierTable(metier5table)
  usethis::use_data(metier5table, overwrite=T)
}
prepMetier5()

prepMetier6 <- function(){
  metier6table <- RstoxFDA::readMetierTable(system.file("configFileExamples", "metiertable.txt", package="RstoxFDA"))
  RstoxFDA:::is.MetierTable(metier6table)
  RstoxFDA:::checkMetierTable(metier6table, meshSize = T)
  usethis::use_data(metier6table, overwrite=T)
}
prepMetier6()
