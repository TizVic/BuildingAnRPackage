library(testthat)
library(farsPackage)

context("farsPackage Tests")

setwd(system.file("extdata", package = "farsPackage"))

test_that("fars_read_years returns a list", {
        myData <- farsPackage::fars_read_years(c(2013, 2015))
        expect_that(myData, is_a("list"))
})

test_that("fars_read_years generates a warning for invalid year", {
        expect_warning(farsPackage::fars_read_years(2017), "invalid year: 2017")
})

test_that("fars_summarize_years returns a data frame", {
        myData <- farsPackage::fars_summarize_years(c(2013,2014))
        expect_that(myData, is_a("data.frame"))
})