
context("test-StoxFunctions: makeUnifiedDefinitionLookupList")
regularfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")

parsedfile <- DefineGear(resourceFilePath = regularfile)

mappings <- makeUnifiedDefinitionLookupList(parsedfile)
expect_true("StoxLandingData" %in% names(mappings))
expect_equal(length(mappings), 2)
expect_equal(length(unique(mappings$StoxLandingData)), 7)
expect_equal(length(unique(mappings$StoxBioticData)), 7)

context("test-StoxFunctions: makeUnifiedDefinitionLookupList one format only")
mappings <- makeUnifiedDefinitionLookupList(parsedfile, formats = c("StoxBioticData"))
expect_equal(length(mappings), 1)
expect_equal(length(unique(mappings$StoxBioticData)), 7)
expect_false("StoxLandingData" %in% names(mappings))

context("test-StoxFunctions: makeUnifiedDefinitionLookupList missing formats")
expect_error(makeUnifiedDefinitionLookupList(parsedfile, formats = c("StoxBioticData", "ICESbiotic")), "Not all formats found in resource. Missing: ICESbiotic")

context("test-StoxFunctions: makeUnifiedDefinitionLookupList redefined codes")
errorfile <- system.file("testresources","gearfactor_error.txt", package="RstoxFDA")
parsedfile <- DefineGear(resourceFilePath = errorfile)
expect_error(makeUnifiedDefinitionLookupList(parsedfile), "Codes redefined: 3714")

context("test-StoxFunctions: makeUnifiedDefinitionLookupList repeated keys")
errorfile <- system.file("testresources","gearfactor_errorkeys.txt", package="RstoxFDA")
expect_error(DefineGear(resourceFilePath = errorfile), "Malformed resource file. Non-unique keys: repition in first two columns.")





context("test-StoxFunctions: DefineGear")
regularfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")
gear <- DefineGear(resourceFilePath = regularfile)
expect_true(data.table::is.data.table(gear))
expect_equal(nrow(gear), 14)
expect_equal(ncol(gear), 3)

context("test-StoxFunctions: DefineGear useProcessData")
nullgear <- DefineGear(NULL, resourceFilePath = regularfile, useProcessData = T)
expect_true(is.null(nullgear))
errorfile <- system.file("testresources","gearfactor_error.txt", package="RstoxFDA")
gg <- DefineGear(gear, resourceFilePath = errorfile, useProcessData = T)
expect_equal(nrow(gear), 14)
expect_equal(ncol(gear), 3)
gg <- DefineGear(gear, resourceFilePath = NULL, useProcessData = T)
expect_equal(nrow(gear), 14)
expect_equal(ncol(gear), 3)




context("test-StoxFunctions: DefineTemporalCategories")
temp <- DefineTemporalCategories(NULL)
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)

context("test-StoxFunctions: DefineTemporalCategories useProcessData")
temp <- DefineTemporalCategories(NULL, useProcessData = T)
expect_true(is.null(temp))

context("test-StoxFunctions: DefineTemporalCategories Month")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Month")
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 12)
expect_equal(ncol(temp), 4)

context("test-StoxFunctions: DefineTemporalCategories non-seasonal")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Month", seasonal = F, years=c(2015,2016))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 24)
expect_equal(ncol(temp), 4)
expect_false(any(is.na(temp$year)))


context("test-StoxFunctions: DefineTemporalCategories unrecognized category")
expect_error(DefineTemporalCategories(NULL, temporalCategory = "Something"), "Temporal category Something not recognized.")

context("test-StoxFunctions: DefineTemporalCategories Custom")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("05-02","15-09"))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 4)

context("test-StoxFunctions: DefineTemporalCategories Custom seasonal")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("05-02","15-09"), seasonal=T)
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 2)
expect_equal(ncol(temp), 4)
expect_error(DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("05-02","15-09"), seasonal=T, years=c(2015, 2016)), "Years provided for seasonal definition.")

context("test-StoxFunctions: DefineTemporalCategories Custom non-seasonal")
expect_error(DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-01","15-09","01-01"), seasonal=F, years=c(2015, 2016)), "Need to provide unique periods.")
temp <- DefineTemporalCategories(NULL, temporalCategory = "Custom", customPeriods = c("01-01","15-09"), seasonal=F, years=c(2015, 2016))
expect_true(data.table::is.data.table(temp))
expect_equal(nrow(temp), 4)
expect_equal(ncol(temp), 4)
expect_false(any(is.na(temp$year)))



context("test-StoxFunctions: DefineAreaCodePosition")
regularfile <- system.file("testresources","mainarea_fdir_from_2018_incl.txt", package="RstoxFDA")
areaPos <- DefineAreaCodePosition(resourceFilePath = regularfile)
expect_true(data.table::is.data.table(areaPos))
expect_equal(nrow(areaPos), 21)
expect_equal(ncol(areaPos), 4)

context("test-StoxFunctions: DefineAreaCodePosition useProcessData")
nullPos <- DefineAreaCodePosition(NULL, resourceFilePath = regularfile, useProcessData = T)
expect_true(is.null(nullPos))

context("test-StoxFunctions: DefineAreaCodePosition malformed")
errorfile <- system.file("testresources","areaPosError.txt", package="RstoxFDA")
expect_error(DefineAreaCodePosition(resourceFilePath = errorfile), "Malformed resource file. Some Area does not have coordinates defined for the case when location is missing.")




context("test-StoxFunctions: DefineCarNeighbours")
carfile <- system.file("testresources","mainarea_neighbour.txt", package="RstoxFDA")
car <- DefineCarNeighbours(resourceFilePath = carfile)
expect_true(data.table::is.data.table(car))
expect_equal(nrow(car), 60)
expect_equal(ncol(car), 2)

context("test-StoxFunctions: DefineCarNeighbours useProcessData")
nullCar <- DefineCarNeighbours(NULL, resourceFilePath = carfile, useProcessData = T)
expect_true(is.null(nullCar))

context("test-StoxFunctions: DefineCarNeighbours non-symmetric")
errorfile <- system.file("testresources","mainarea_error.txt", package="RstoxFDA")
expect_error(DefineCarNeighbours(resourceFilePath = errorfile), "Neighbour definition not symmetric. 1 is neighbour of 0 but not vice versa.")

context("test-StoxFunctions: DefineCarNeighbours repeated key")
errorfile <- system.file("testresources","mainarea_error2.txt", package="RstoxFDA")
expect_error(DefineCarNeighbours(resourceFilePath = errorfile), "Malformed resource file, Non-unique keys: repition in first column: 1")




context("test-StoxFunctions: DefineAgeErrorMatrix")
ageerorfile <- system.file("testresources","AgeErrorHirstEtAl2012.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(resourceFilePath = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 15)
expect_equal(ncol(ageerror), 16)

context("test-StoxFunctions: DefineAgeErrorMatrix useProcessData")
nullAE <- DefineAgeErrorMatrix(NULL, resourceFilePath = ageerorfile, useProcessData = T)
expect_true(is.null(nullAE))

context("test-StoxFunctions: DefineAgeErrorMatrix non-symmetric")
ageerorfile <- system.file("testresources","AgeNonSym.txt", package="RstoxFDA")
ageerror <- DefineAgeErrorMatrix(resourceFilePath = ageerorfile)
expect_true(data.table::is.data.table(ageerror))
expect_equal(nrow(ageerror), 14)
expect_equal(ncol(ageerror), 16)

context("test-StoxFunctions: DefineAgeErrorMatrix malformed")
ageerorfile <- system.file("testresources","AgeMalformed.txt", package="RstoxFDA")
expect_error(DefineAgeErrorMatrix(resourceFilePath = ageerorfile),"Malformed resource file. All probabilities must be in >=0 and <=1.")

ageerorfile <- system.file("testresources","AgeMalformed2.txt", package="RstoxFDA")
expect_error(DefineAgeErrorMatrix(resourceFilePath = ageerorfile),"Malformed resource file. Columns must sum to 1.")




context("test-StoxFunctions: DefineClassificationError")
classerorfile <- system.file("testresources","classificationError.txt", package="RstoxFDA")
classerror <- DefineClassificationError(resourceFilePath = classerorfile)
expect_true(data.table::is.data.table(classerror))
expect_equal(nrow(classerror), 1)
expect_equal(ncol(classerror), 8)

context("test-StoxFunctions: DefineClassificationError useProcessdata")
classNULL <- DefineClassificationError(NULL, resourceFilePath = classerorfile, useProcessData = T)
expect_true(is.null(classNULL))

