context("test-StoxAnalysisFunctions: RunRecaEstimate")
data(recaDataExample)
result <- RunRecaEstimate(recaDataExample, 100, 100, thin=1)
expect_true(all(c("input", "fit", "prediction", "covariateMaps") %in% names(result)))
expect_equal(dim(result$prediction$TotalCount)[3], 100)

