# rm(list = ls())
context("test-ForestData:::check_rename_variable_col")
library(testthat)
library(ForestData)
data = data.frame("variable" = rep(1,20),"variable2"= rep(2,20))
arg <- "variable"
name <- "new_name"

test_that("ForestData:::check_rename_variable_col throws an error when data is not a data.frame",
          {
            expect_error(ForestData:::check_rename_variable_col(arg,name, list()))
          })

test_that("ForestData:::check_rename_variable_col throws an error when arg is not in the column names",
          {
            expect_error(ForestData:::check_rename_variable_col("unexisting_colname",name, data))
          })

test_that("ForestData:::check_rename_variable_col throws an error when non-character objects are inputted as arguments arg or name",
          {
            expect_error(ForestData:::check_rename_variable_col(3, data))
            expect_error(ForestData:::check_rename_variable_col(arg,3, data))
          })

test_that("ForestData:::check_rename_variable_col throws an error when non length 1 objects are inputted as arguments arg or name",
          {
            expect_error(ForestData:::check_rename_variable_col(c("variable","variable2"),name, data))
            expect_error(ForestData:::check_rename_variable_col(arg,c("new","name"), data))
          })
test_that("ForestData:::check_rename_variable_col does well",
          {
            expect_equal(names(ForestData:::check_rename_variable_col(arg,name, data)),c("new_name","variable2"))
          })

