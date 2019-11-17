context("test-DataPrep: default")

date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
quarters <- getTemporalCategory(date)
expect_equal(quarters, c("Q1", "Q1", "Q1", "Q2", "Q2", "Q3", "Q4"))

context("test-DataPrep: month")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
quarters <- getTemporalCategory(date, temporalType = "month")
expect_equal(quarters, strftime(date, format="%B"))

context("test-DataPrep: week")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
weeks <- getTemporalCategory(date, temporalType = "week")
expect_equal(weeks, c("W03", "W09", "W09", "W14", "W22", "W27", "W50"))

context("test-DataPrep: custom")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
custom <- getTemporalCategory(date, temporalType = "custom", FUN=function(day,month){if(month<4){return("l")};if(month==4 & day<2){return("g")};return("s")})
expect_equal(custom, c("l", "l", "l", "g", "s", "s", "s"))

context("test-DataPrep: non-seasonal")
date <- as.POSIXct(c("2018-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
custom <- getTemporalCategory(date, temporalType = "custom", seasonal = F, FUN=function(day,month){if(month<4){return("l")};if(month==4 & day<2){return("g")};return("s")})
expect_equal(custom, c("l-2018", "l-2019", "l-2019", "g-2019", "s-2019", "s-2019", "s-2019"))

context("test-DataPrep: undefined")
date <- as.POSIXct(c("2019-01-17 21:37:29 CET", "2019-02-28 21:37:29 CET", "2019-03-01 21:37:29 CET", "2019-04-1 21:37:29 CET", "2019-06-1 21:37:29 CET", "2019-07-1 21:37:29 CET", "2019-12-12 21:37:29 CET"))
expect_error(getTemporalCategory(date, temporalType = "QRT"), "Temporal type QRT not recognized.")
