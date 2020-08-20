mainareaFdir2018 <- RstoxBase::DefineStrata(NULL, "ResourceFile", system.file("testresources", "mainarea_fdir_fom2018_strata.txt", package="RstoxFDA"))

if (is.na(sp::proj4string(mainareaFdir2018))){
  sp::proj4string(mainareaFdir2018) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
}

usethis::use_data(mainareaFdir2018, overwrite = T)

