context("test-StoxFunctions: readUnifiedDefinition")

regularfile <- system.file("testresources","gearfactor.txt", package="RstoxFDA")

mappings <- readUnifiedDefinition(regularfile)
expect_true("Landing" %in% names(mappings))
expect_equal(length(mappings), 2)
expect_equal(length(unique(mappings$Landing)), 7)
expect_equal(length(unique(mappings$Biotic)), 7)

context("test-StoxFunctions: readUnifiedDefinition one format only")
mappings <- readUnifiedDefinition(regularfile, formats = c("Biotic"))
expect_equal(length(mappings), 1)
expect_equal(length(unique(mappings$Biotic)), 7)
expect_false("Landing" %in% names(mappings))

context("test-StoxFunctions: readUnifiedDefinition missing formats")
expect_error(readUnifiedDefinition(regularfile, formats = c("Biotic", "ICESbiotic")), "Not all formats found in file. Missing: ICESbiotic")

context("test-StoxFunctions: readUnifiedDefinition redefined codes")
errorfile <- system.file("testresources","gearfactor_error.txt", package="RstoxFDA")
expect_error(readUnifiedDefinition(errorfile), "Codes redefined: 3714")

errorfile <- system.file("testresources","gearfactor_errorkeys.txt", package="RstoxFDA")
expect_error(readUnifiedDefinition(errorfile), "Malformed resource file. Non-unique keys: repition in first two columns.")
