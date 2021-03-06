mainareaFdir2018 <- RstoxBase::DefineStrata(NULL, "ResourceFile", system.file("dataPrepScripts", "mainarea2018.txt", package="RstoxFDA"))

if (is.na(sp::proj4string(mainareaFdir2018))){
  sp::proj4string(mainareaFdir2018) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
}

usethis::use_data(mainareaFdir2018, overwrite = T)

mainareaFdir2017 <- RstoxBase::DefineStrata(NULL, "ResourceFile", system.file("dataPrepScripts", "mainarea2017.txt", package="RstoxFDA"))
if (is.na(sp::proj4string(mainareaFdir2017))){
  sp::proj4string(mainareaFdir2017) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
}
usethis::use_data(mainareaFdir2017, overwrite = T)
