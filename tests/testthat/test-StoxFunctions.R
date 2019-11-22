
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



context("test-StoxFunctions: PrepReca")
fail("Need data formats StoxBioticData and StoxLandingData in order to test.")

