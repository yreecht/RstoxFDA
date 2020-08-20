mainareaFdir2018 <- RstoxBase::DefineStrata(NULL, "ResourceFile", system.file("testresources", "mainarea_fdir_fom2018_strata.txt", package="RstoxFDA"))
usethis::use_data(mainareaFdir2018, overwrite = T)
