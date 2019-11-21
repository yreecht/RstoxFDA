context("test-StoxFunctions: makeUnifiedDefinitionLookupList")

regularfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")

parsedfile <- readTabSepFile(regularfile)

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
expect_error(makeUnifiedDefinitionLookupList(parsedfile, formats = c("StoxBioticData", "ICESbiotic")), "Not all formats found in file. Missing: ICESbiotic")

context("test-StoxFunctions: makeUnifiedDefinitionLookupList redefined codes")
errorfile <- system.file("testresources","gearfactor_error.txt", package="RstoxFDA")
parsedfile <- readTabSepFile(errorfile)
expect_error(makeUnifiedDefinitionLookupList(parsedfile), "Codes redefined: 3714")

errorfile <- system.file("testresources","gearfactor_errorkeys.txt", package="RstoxFDA")
parsedfile <- readTabSepFile(errorfile)
expect_error(makeUnifiedDefinitionLookupList(parsedfile), "Malformed resource file. Non-unique keys: repition in first two columns.")
