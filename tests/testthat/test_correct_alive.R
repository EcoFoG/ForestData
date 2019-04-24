
# Unit tests for correct_alive function -----------------------------------

#The tests are run from the internalmost function to the whole function level

####################### Allow NAs to exist in the initial dataset ##################
############### i.e. unseen trees are already "spotted" and with status NA #########
# Individual tree level ---------------------------------------------------


test_that("correct_status_tree works on paracou config", {

  source("R/correct_alive.R")
  data("example_alive")
  data("expected_2_unseen")

  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]

  ids <- unique(example_alive$id)
  for(i in ids){

    tree <- example_alive[which(example_alive$id == i),]
    pl <- unique(tree$plot)
    cens <- unique(example_alive[which(example_alive$plot == pl),"time"])
    corrected_tree <-  expected_2_unseen[which(expected_2_unseen$id == i),]

    res_tree <- .correct_alive_tree(tree_temp = tree,
                                    censuses = cens,
                                    dead_confirmation_censuses =2,
                                    i = i)



    row.names(res_tree) <- 1:nrow(res_tree)
    row.names(corrected_tree) <- 1:nrow(corrected_tree)
    # print(res_tree)
    # # print(tree)
    # print(corrected_tree)
    expect_equal(res_tree,corrected_tree)
    # print(i)
  }
})

test_that("correct_status_tree works on death_confirmation_censuses = 3", {

  source("R/correct_alive.R")
  data("example_alive")
  data("expected_3_unseen")
  names(expected_3_unseen)[which(names(expected_3_unseen) == "status_altern")] <- "status_corr"

  example_alive <- example_alive[,which(names(example_alive) != "status_corr")]
  names(example_alive)[which(names(example_alive)== "status_altern")] <- "status_corr"
  # names(example_alive[,which(names(example_alive)== "status_altern")]) <- "status_corr"
  names(example_alive)
  ids <- unique(example_alive$id)
  for(i in ids){
    tree <- example_alive[which(example_alive$id == i),]
    pl <- unique(tree$plot)
    cens <- unique(example_alive[which(example_alive$plot == pl),"time"])
    corrected_tree <-  expected_3_unseen[which(expected_3_unseen$id == i),]
    # print(i)
    # i <- "b"
    tree
    res_tree <- .correct_alive_tree(tree_temp = tree,
                                    censuses = cens,
                                    dead_confirmation_censuses =3,
                                    i)

    # print(i)
    row.names(res_tree) <- 1:nrow(res_tree)
    row.names(corrected_tree) <- 1:nrow(corrected_tree)
    res_tree <- res_tree[,names(corrected_tree)]
    # print(res_tree)
    # # print(tree)C
    # print(corrected_tree)

    expect_equal(res_tree,corrected_tree)
  }
})


# Plot level --------------------------------------------------------------

test_that("correct_status_plotlevel works on paracou config", {

  source("R/correct_alive.R")
  data("example_alive")
  data("expected_2_unseen")

  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]


  plot1 <- example_alive[which(example_alive$plot == 1),]
  plot2 <- example_alive[which(example_alive$plot == 2),]

  corrected1 <- expected_2_unseen[which(expected_2_unseen$plot == 1),]
  corrected2 <- expected_2_unseen[which(expected_2_unseen$plot == 2),]

  plot1 <- plot1[,-which(names(example_alive) == "status_corr")]
  plot2 <- plot2[,-which(names(example_alive) == "status_corr")]

  plot1$status_corr <- plot1$status
  plot2$status_corr <- plot2$status


  res1 <- .correct_status_plotlevel(data_plot = plot1,
                                    dead_confirmation_censuses = 2,
                                    use_size = FALSE)

  # print("res1")
  # print(res1)
  # print("corrected1")
  # print(corrected1)


  row.names(res1) <- 1:nrow(res1)
  row.names(corrected1) <- 1:nrow(corrected1)
  res1 <- res1[,names(corrected1)]
  expect_equal(res1,corrected1)


  res2 <- .correct_status_plotlevel(data_plot = plot2,
                                    dead_confirmation_censuses = 2,
                                    use_size = FALSE)

  row.names(res2) <- 1:nrow(res2)
  row.names(corrected2) <- 1:nrow(corrected2)
  res2 <- res2[,names(corrected2)]
  expect_equal(res2,corrected2)
})

test_that("correct_status_plotlevel works changing dead_confirmation_census to 3", {

  source("R/correct_alive.R")
  data("example_alive")
  data("expected_3_unseen")

  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]
  names(expected_3_unseen)[which(names(expected_3_unseen)== "status_altern")] <- "status_corr"

  plot1 <- example_alive[which(example_alive$plot == 1),]
  plot2 <- example_alive[which(example_alive$plot == 2),]

  corrected1 <- expected_3_unseen[which(expected_3_unseen$plot == 1),]
  corrected2 <- expected_3_unseen[which(expected_3_unseen$plot == 2),]

  plot1 <- plot1[,-which(names(example_alive) == "status_corr")]
  plot2 <- plot2[,-which(names(example_alive) == "status_corr")]

  plot1$status_corr <- plot1$status
  plot2$status_corr <- plot2$status

  res1 <- .correct_status_plotlevel(data_plot = plot1,
                                    dead_confirmation_censuses = 3,
                                    use_size = FALSE)
  row.names(res1) <- 1:nrow(res1)
  row.names(corrected1) <- 1:nrow(corrected1)
  res1 <- res1[,names(corrected1)]

  # print(res1)
  # print(corrected1)
  expect_equal(res1,corrected1)

  res2 <- .correct_status_plotlevel(data_plot = plot2,
                                    dead_confirmation_censuses = 3,
                                    use_size = FALSE)
  row.names(res2) <- 1:nrow(res2)
  row.names(corrected2) <- 1:nrow(corrected2)
  res2 <- res2[,names(corrected2)]
  expect_equal(res2,corrected2)
})


# Overall function --------------------------------------------------------

test_that("correct_alive works", {

  source("R/correct_alive.R")
  data("example_alive")
  data("expected_2_unseen")

  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]
  example_alive <- example_alive[,which(names(example_alive) != "status_corr")]

  names(expected_2_unseen)[which(names(expected_2_unseen)== "status_altern")] <- "status_corr"

  res <- correct_alive(data = example_alive,
                       id_col = "id",
                       time_col = "time",
                       alive_col = "status",
                       plot_col = "plot",
                       byplot = TRUE,
                       dead_confirmation_censuses = 2,
                       use_size = FALSE)

  row.names(res) <- 1:nrow(res)
  row.names(expected_2_unseen) <- 1:nrow(expected_2_unseen)
  res <- res[,names(expected_2_unseen)]
  # print(res)
  # print(example_alive)
  expect_equal(res,expected_2_unseen)

})

test_that("correct_alive works", {

  data("example_alive")
  data("expected_3_unseen")

  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]
  example_alive <- example_alive[,-which(names(example_alive) == "status_corr")]
  names(expected_3_unseen)[which(names(expected_3_unseen)== "status_altern")] <- "status_corr"



  res <- correct_alive(data = example_alive,
                       id_col = "id",
                       time_col = "time",
                       alive_col = "status",
                       plot_col = "plot",
                       byplot = TRUE,
                       dead_confirmation_censuses = 3,
                       use_size = FALSE)

  row.names(res) <- 1:nrow(res)
  row.names(expected_3_unseen) <- 1:nrow(expected_3_unseen)
  res <- res[,names(expected_3_unseen)]
  # print(res)
  # print(example_alive)
  expect_equal(res,expected_3_unseen)


})

####################### Suppress NA lines as in real inventories #################


# Individual tree level ---------------------------------------------------


test_that("correct_status_tree works on paracou config", {

  source("R/correct_alive.R")
  data("example_alive_mini")
  data("expected_2_unseen_mini")


  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]


  ids <- unique(example_alive_mini$id)
  for(i in ids){
    # print(i)
    # i = "f"
    # tree <- example_alive_mini[which(example_alive_mini$id == i),which(names(example_alive_mini) != "status_corr")]
    tree <- example_alive_mini[which(example_alive_mini$id == i),]
    pl <- unique(tree$plot)
    cens <- unique(example_alive_mini[which(example_alive_mini$plot == pl),"time"])
    corrected_tree <-  expected_2_unseen_mini[which(expected_2_unseen_mini$id == i),]

    res_tree <- .correct_alive_tree(tree_temp = tree,
                                    censuses = cens,
                                    dead_confirmation_censuses =2,
                                    i = i)



    row.names(res_tree) <- 1:nrow(res_tree)
    row.names(corrected_tree) <- 1:nrow(corrected_tree)
    res_tree <- res_tree[,names(corrected_tree)]
    # print(res_tree)
    # # print(tree)
    # print(corrected_tree)
    expect_equal(res_tree,corrected_tree)
    # print(i)
  }
})

test_that("correct_status_tree works on death_confirmation_censuses = 3", {

  source("R/correct_alive.R")
  data("example_alive_mini")
  data("expected_3_unseen_mini")
  names(expected_3_unseen_mini)[which(names(expected_3_unseen_mini) == "status_altern")] <- "status_corr"

  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_corr")]
  names(example_alive_mini)[which(names(example_alive_mini)== "status_altern")] <- "status_corr"
  # names(example_alive_mini[,which(names(example_alive_mini)== "status_altern")]) <- "status_corr"
  names(example_alive_mini)
  ids <- unique(example_alive_mini$id)
  for(i in ids){
    # i = "f"
    tree <- example_alive_mini[which(example_alive_mini$id == i),]
    pl <- unique(tree$plot)
    cens <- unique(example_alive_mini[which(example_alive_mini$plot == pl),"time"])
    corrected_tree <-  expected_3_unseen_mini[which(expected_3_unseen_mini$id == i),]
    # print(i)
    tree
    res_tree <- .correct_alive_tree(tree_temp = tree,
                                    censuses = cens,
                                    dead_confirmation_censuses =3,
                                    i)

    # print(i)
    row.names(res_tree) <- 1:nrow(res_tree)
    row.names(corrected_tree) <- 1:nrow(corrected_tree)
    res_tree <- res_tree[,names(corrected_tree)]
    # print(res_tree)
    # # print(tree)C
    # print(corrected_tree)

    expect_equal(res_tree,corrected_tree)
  }
})


# Plot level --------------------------------------------------------------

test_that("correct_status_plotlevel works on paracou config", {

  source("R/correct_alive.R")
  data("example_alive_mini")
  data("expected_2_unseen_mini")

  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]


  plot1 <- example_alive_mini[which(example_alive_mini$plot == 1),]
  plot2 <- example_alive_mini[which(example_alive_mini$plot == 2),]

  corrected1 <- expected_2_unseen_mini[which(expected_2_unseen_mini$plot == 1),]
  corrected2 <- expected_2_unseen_mini[which(expected_2_unseen_mini$plot == 2),]

  plot1 <- plot1[,-which(names(example_alive_mini) == "status_corr")]
  plot2 <- plot2[,-which(names(example_alive_mini) == "status_corr")]

  plot1$status_corr <- plot1$status
  plot2$status_corr <- plot2$status


  res1 <- .correct_status_plotlevel(data_plot = plot1,
                                    dead_confirmation_censuses = 2,
                                    use_size = FALSE)

  # print("res1")
  # print(res1)
  # print("corrected1")
  # print(corrected1)


  row.names(res1) <- 1:nrow(res1)
  row.names(corrected1) <- 1:nrow(corrected1)
  res1 <- res1[,names(corrected1)]
  expect_equal(res1,corrected1)


  res2 <- .correct_status_plotlevel(data_plot = plot2,
                                    dead_confirmation_censuses = 2,
                                    use_size = FALSE)

  row.names(res2) <- 1:nrow(res2)
  row.names(corrected2) <- 1:nrow(corrected2)
  res2 <- res2[,names(corrected2)]
  expect_equal(res2,corrected2)
})

test_that("correct_status_plotlevel works changing dead_confirmation_census to 3", {

  source("R/correct_alive.R")
  data("example_alive_mini")
  data("expected_3_unseen_mini")

  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]
  names(expected_3_unseen_mini)[which(names(expected_3_unseen_mini)== "status_altern")] <- "status_corr"

  plot1 <- example_alive_mini[which(example_alive_mini$plot == 1),]
  plot2 <- example_alive_mini[which(example_alive_mini$plot == 2),]

  corrected1 <- expected_3_unseen_mini[which(expected_3_unseen_mini$plot == 1),]
  corrected2 <- expected_3_unseen_mini[which(expected_3_unseen_mini$plot == 2),]

  plot1 <- plot1[,-which(names(example_alive_mini) == "status_corr")]
  plot2 <- plot2[,-which(names(example_alive_mini) == "status_corr")]

  plot1$status_corr <- plot1$status
  plot2$status_corr <- plot2$status

  res1 <- .correct_status_plotlevel(data_plot = plot1,
                                    dead_confirmation_censuses = 3,
                                    use_size = FALSE)
  row.names(res1) <- 1:nrow(res1)
  row.names(corrected1) <- 1:nrow(corrected1)
  res1 <- res1[,names(corrected1)]

  # print(res1)
  # print(corrected1)
  expect_equal(res1,corrected1)

  res2 <- .correct_status_plotlevel(data_plot = plot2,
                                    dead_confirmation_censuses = 3,
                                    use_size = FALSE)
  row.names(res2) <- 1:nrow(res2)
  row.names(corrected2) <- 1:nrow(corrected2)
  res2 <- res2[,names(corrected2)]
  expect_equal(res2,corrected2)
})


# Overall function --------------------------------------------------------

test_that("correct_alive works", {

  source("R/correct_alive.R")
  data("example_alive_mini")
  data("expected_2_unseen_mini")

  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]
  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_corr")]

  names(expected_2_unseen_mini)[which(names(expected_2_unseen_mini)== "status_altern")] <- "status_corr"

  res <- correct_alive(data = example_alive_mini,
                       id_col = "id",
                       time_col = "time",
                       alive_col = "status",
                       plot_col = "plot",
                       byplot = TRUE,
                       dead_confirmation_censuses = 2,
                       use_size = FALSE)

  row.names(res) <- 1:nrow(res)
  row.names(expected_2_unseen_mini) <- 1:nrow(expected_2_unseen_mini)
  res <- res[,names(expected_2_unseen_mini)]
  expect_equal(res,expected_2_unseen_mini)

})

test_that("correct_alive works", {

  data("example_alive_mini")
  data("expected_3_unseen_mini")

  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]
  example_alive_mini <- example_alive_mini[,-which(names(example_alive_mini) == "status_corr")]
  names(expected_3_unseen_mini)[which(names(expected_3_unseen_mini)== "status_altern")] <- "status_corr"



  res <- correct_alive(data = example_alive_mini,
                       id_col = "id",
                       time_col = "time",
                       alive_col = "status",
                       plot_col = "plot",
                       byplot = TRUE,
                       dead_confirmation_censuses = 3,
                       use_size = FALSE)

  row.names(res) <- 1:nrow(res)
  row.names(expected_3_unseen_mini) <- 1:nrow(expected_3_unseen_mini)
  res <- res[,names(expected_3_unseen_mini)]

  #Finally, compare result and expected
  expect_equal(res,expected_3_unseen_mini)
})



# Test error messages -----------------------------------------------------


# Operations I did to modify or create test datasets ----------------------

# convert xls to rda
#
# fil <- list.files(file.path(getwd(),"data"))
# nams <- c("example_alive","example_alive_mini","expected_2_unseen","expected_3_unseen")
#
# for(i in 1:4){
#   assign(nams[i], read.csv2(file.path(getwd(),"data",fil[i])))
#   print(i)
#   # save(get(nams[i]), file = paste0(nams[i],'.rda'))
#   do.call(save, list(nams[i], file=file.path(getwd(),"data",paste(nams[i], "rda", sep = "."))))
# }
#
# save(get("example_alive"), file = "example_alive.rda")
# file.path(getwd(),"data",paste(nams[i], "rda", sep = "."))

# data("expected_3_unseen")
# expected_3_unseen[which(is.na(expected_3_unseen$status)),c("col1","col2")] <- NA
# expected_3_unseen_mini <- expected_3_unseen
# save(expected_3_unseen_mini, file = "data/expected_3_unseen_mini.rda")
