#
# Parsing LSS
#
context("parseLSS")
data <- parseLSS(system.file("testresources","landings_trimmed_2018.lss", package="prepRECA"))
expect_true("Landingsmåte" %in% names(data))
expect_true(all(!is.na(data$`Art - FDIR`)))
expect_equal(nrow(data),9)
