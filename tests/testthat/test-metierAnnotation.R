context("metierannotation parse table")
regularfile <- system.file("testresources","metiermapping_example.txt", package="RstoxFDA")
metiertable <- readMetierTable(regularfile)
expect_true(!any(is.na(metiertable$metier)))
expect_true(!any(is.na(metiertable$gearcode)))
expect_equal(sum(is.na(metiertable[1,])), 3)
expect_equal(sum(is.na(metiertable[6,])), 6)

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

#
# tests
#

# check parsing of integers in char field

# implement test for seldev mesh size given, when no selection device is given
# implement test for seldev mesh size given, selection device is not meshed (logical)

# implement test for mesh size given, when gear is not meshed (logical)
# implement test for one range of mesh sizes is missing gear
# implement test for one range of mesh sizes is missing seldev

# implement test for partially specified meshedGear and meshed seldev

