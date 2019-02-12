rm(list = ls())
library(testthat)
data = data.frame("variable" = rep(1,20),"variable2"= rep(2,20))
context("check_rename_variable_col")
arg <- "variable"
name <- "new_name"

test_that("check_rename_variable_col throws an error when data is not a data.frame",
          {
            expect_error(check_rename_variable_col(arg,name, list()))
          })

test_that("check_rename_variable_col throws an error when arg is not in the column names",
          {
            expect_error(check_rename_variable_col("unexisting_colname",name, data))
          })

test_that("check_rename_variable_col throws an error when non-character objects are inputted as arguments arg or name",
          {
            expect_error(check_rename_variable_col(3, data))
            expect_error(check_rename_variable_col(arg,3, data))
          })

test_that("check_rename_variable_col throws an error when non length 1 objects are inputted as arguments arg or name",
          {
            expect_error(check_rename_variable_col(c("variable","variable2"),name, data))
            expect_error(check_rename_variable_col(arg,c("new","name"), data))
          })
