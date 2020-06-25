context("metierannotation parse table")
regularfile <- system.file("testresources","metiermapping_example.txt", package="RstoxFDA")
metiertable <- readMetierTable(regularfile)
expect_true(!any(is.na(metiertable$metier)))
expect_equal(sum(is.na(metiertable$gearcode)), 1)
expect_equal(sum(is.na(metiertable[1,])), 3)
expect_equal(sum(is.na(metiertable[6,])), 6)
expect_equal(metiertable[7,"gearcode"][[1]], "01")

simplefile <- system.file("testresources","metiermapping_example_simple.txt", package="RstoxFDA")
metiertable <- readMetierTable(simplefile)
expect_true(!any(is.na(metiertable$metier)))
expect_true(!any(is.na(metiertable$gearcode)))
expect_equal(sum(is.na(metiertable[1,])), 8)

errorfile <- system.file("testresources","metiermapping_example_error.txt", package="RstoxFDA")
expect_error(readMetierTable(errorfile), "Some column names are not recognized: area")

#test annotation
testdata <- data.table::data.table(id=seq(1,6), gear=as.character(c(51,51,22,22,35,35)),
                                   meshSize=c(90,90,90,120,NA,NA),
                                   selDev=c("G","G",NA,NA,NA,NA),
                                   selDevMS=c(110,90,NA,NA,NA,NA),
                                   area=as.character(c("08","08","09","09","09","10")))

regularfile <- system.file("testresources","metiermapping_example.txt", package="RstoxFDA")
metiertable <- readMetierTable(regularfile)

assignMetier(testdata, metiertable, "gear", meshSizeColumn = "meshSize", selectivityDeviceColumn = "selDev", selectivityDeviceMeshSizeColumn = "selDevMS")

context("Annotation error checks")
expect_error(assignMetier(testdata, metiertable, "gear"))

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedGear[2] <- NA
expect_error(is.MetierTable(metiertableMissingMS, T), "The column 'meshedGear' has a value for some gears, but not all")
expect_false(is.MetierTable(metiertableMissingMS))
metiertableMissingMS$meshedGear <- NA
expect_error(is.MetierTable(metiertableMissingMS, T))
expect_false(is.MetierTable(metiertableMissingMS))
metiertableMissingMS$lowerMeshSize <- NA
metiertableMissingMS$upperMeshSize <- NA
expect_true(is.MetierTable(metiertableMissingMS))

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedGear[2] <- F
expect_error(is.MetierTable(metiertableMissingMS, T))

metiertableMissingMS <- metiertable
metiertableMissingMS$meshedSelectivityDevice[2] <- NA
expect_error(is.MetierTable(metiertableMissingMS, T), "The column 'meshedSelectivityDevice' has a value for some gears, but not all")
metiertableMissingMS$meshedSelectivityDevice <- NA
expect_error(is.MetierTable(metiertableMissingMS, T))
metiertableMissingMS$selDevLowerMeshSize <- NA
metiertableMissingMS$selDevUpperMeshSize <- NA
expect_true(is.MetierTable(metiertableMissingMS))

metiertableMissingMS <- metiertable
metiertableMissingMS$lowerMeshSize <- NA
expect_error(is.MetierTable(metiertableMissingMS, T))
metiertableMissingMS <- metiertable
metiertableMissingMS$selDevLowerMeshSize <- NA
expect_error(is.MetierTable(metiertableMissingMS, T))

metiertableMissingMS <- metiertable
metiertableMissingMS$selectivityDevice[2] <- NA
expect_error(is.MetierTable(metiertableMissingMS, T), "Mesh sizes provided for selectivity devices where 'selectivityDevice' is not given")

metiertableMissingMS <- metiertable
metiertableMissingMS$gearcode[2] <- NA
expect_error(is.MetierTable(metiertableMissingMS, T), "Mesh sizes provided for gear where 'gear' is not given")


metiertableMissingMS <- metiertable
metiertableMissingMS$selDevUpperMeshSize[2] <- 101
expect_error(checkMetierTable(metiertableMissingMS, meshSize = T, selDevMeshSize = T), "Mesh sizes have overlapping ranges for gear 51/G")

metiertableMissingMS <- metiertable
metiertableMissingMS$upperMeshSize[4] <- 101
expect_error(checkMetierTable(metiertableMissingMS, meshSize = T, selDevMeshSize = T), "Mesh sizes have overlapping ranges for gear 22")


#
# tests
#


# test annotation with NAs in data

