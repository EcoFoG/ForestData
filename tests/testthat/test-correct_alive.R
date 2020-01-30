
# Unit tests for correct_alive function -----------------------------------

#The tests are run from the internalmost function to the whole function level

################## Test correction outputs with hand-crafted sets ##################
######## i.e. control and pathological cases, to check the func's behavior #########

# Individual tree level ---------------------------------------------------
# rm(list = ls())
# getwd()
context("test-correct_alive")
library(testthat)
# load datasets
# cat(getwd())
# print(getwd())
# cat(identical(Sys.getenv("TESTTHAT"), "true"))
# print(identical(Sys.getenv("TESTTHAT"), "true"))
load("./test_data/example_alive.rda")
load("./test_data/example_alive_mini.rda")
# load("./tests/testthat/test_data/example_alive_mini.rda")
load("./test_data/expected_2_unseen.rda")
load("./test_data/expected_3_unseen_mini.rda")
load("./test_data/expected_3_unseen.rda")
load("./test_data/expected_2_unseen_mini.rda")

# Test error messages -----------------------------------------------------


expect_error(correct_alive(data = as.list(example_alive),
                           id_col = "id",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = 3,
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "TOTO",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = 3,
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "TOTO",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = 3,
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "time",
                           status_col = "TOTO",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = 3,
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "TOTO",
                           byplot = TRUE,
                           dead_confirmation_censuses = 3,
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = list(),
                           dead_confirmation_censuses = 3,
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = c(1,2),
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = "2",
                           use_size = FALSE))

expect_error(correct_alive(data = example_alive,
                           id_col = "id",
                           time_col = "time",
                           status_col = "status",
                           plot_col = "plot",
                           byplot = TRUE,
                           dead_confirmation_censuses = 2,
                           use_size = list()))



# Test outputs ------------------------------------------------------------


test_that("correct_status_tree gives accurate results for control datasets", {

  ## We are going to test two datasets, for 2 modalities of death_confirmation_censuses, which
  ## represents the number of censuses needed to declare dead a tree if unsighted for such a duration.
  # Load dataset 1: When trees are unsighted, a corresponding line appears in the table with status NA
  # data("example_alive")
  # Load dataset 2, where unsighted trees have no corresponding line in the table (realistic case)
  # data("example_alive_mini")
  # Corresponding hand-corrected datasets for death_confirmation_censuses = 3
  # data("expected_2_unseen")
  # data("expected_2_unseen_mini")
  # Corresponding hand-corrected datasets for death_confirmation_censuses = 3
  # data("expected_3_unseen")
  # data("expected_3_unseen_mini")


  # Remove one of the columns of the initial dataset. Unimportant step.
  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]
  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]

  # Change one column name of the initial dataset. Unimportant step.
  names(expected_3_unseen)[which(names(expected_3_unseen) == "status_altern")] <- "status_corr"
  # names(expected_3_unseen_mini)[which(names(expected_3_unseen_mini) == "status_altern")] <- "status_corr"

  # Extract ids and loop on them - since we are testing individual tree level function
  ids <- unique(example_alive_mini$id)
  for(i in ids){
    #Prepare required arguments
    ## Tree's censuses
    tree <- example_alive[which(example_alive$id == i),]
    tree_mini <- example_alive_mini[which(example_alive_mini$id == i),]
    ## A vector containing all the census years for the plot where the tree is located
    pl <- unique(tree$plot)
    pl_mini <- unique(tree$plot)
    cens_mini <- unique(example_alive_mini[which(example_alive_mini$plot == pl_mini),"time"])
    cens <- unique(example_alive[which(example_alive$plot == pl),"time"])

    ## Extract the expected corrected lines in the result dataset
    corrected_tree <-  expected_2_unseen[which(expected_2_unseen$id == i),]
    corrected_tree_mini <-  expected_2_unseen_mini[which(expected_2_unseen_mini$id == i),]
    corrected_tree_3 <-  expected_3_unseen[which(expected_3_unseen$id == i),]
    corrected_tree_mini_3 <-  expected_3_unseen_mini[which(expected_3_unseen_mini$id == i),]

    # Correct using the function
    res_tree <- .correct_alive_tree(tree_temp = tree,
                                    censuses = cens,
                                    dead_confirmation_censuses =2,
                                    i = i,
                                    invariant_columns = NULL)
    res_tree_mini <- .correct_alive_tree(tree_temp = tree_mini,
                                          censuses = cens_mini,
                                          dead_confirmation_censuses =2,
                                          i = i,
                                         invariant_columns = NULL)
    res_tree_3 <- .correct_alive_tree(tree_temp = tree,
                                      censuses = cens,
                                      dead_confirmation_censuses =3,
                                      i,
                                      invariant_columns = NULL)
    res_tree_mini_3 <- .correct_alive_tree(tree_temp = tree_mini,
                                           censuses = cens_mini,
                                           dead_confirmation_censuses =3,
                                           i,
                                           invariant_columns = NULL)


    # Rearrange row.names because .correct_alive_tree adds missing and erases useless lines.
    row.names(res_tree) <- 1:nrow(res_tree)
    row.names(res_tree_mini) <- 1:nrow(res_tree_mini)
    row.names(res_tree_3) <- 1:nrow(res_tree_3)
    row.names(res_tree_mini_3) <- 1:nrow(res_tree_mini_3)

    row.names(corrected_tree) <- 1:nrow(corrected_tree)
    row.names(corrected_tree_mini) <- 1:nrow(corrected_tree_mini)
    row.names(corrected_tree_3) <- 1:nrow(corrected_tree_3)
    row.names(corrected_tree_mini_3) <- 1:nrow(corrected_tree_mini_3)


    #Reorder columns
    res_tree <- res_tree[,names(corrected_tree)]
    res_tree_mini <- res_tree_mini[,names(corrected_tree_mini)]
    res_tree_3 <- res_tree_3[,names(corrected_tree_3)]
    res_tree_mini_3 <- res_tree_mini_3[,names(corrected_tree_mini_3)]

    # Expect results to be similar to hand-corrected datasets
    expect_equal(res_tree,corrected_tree)
    expect_equal(res_tree_mini,corrected_tree_mini)
    expect_equal(res_tree_3,corrected_tree_3)
    expect_equal(res_tree_mini_3,corrected_tree_mini_3)
  }
})




# Plot level --------------------------------------------------------------

test_that("correct_status_plotlevel gives accurate results for control datasets", {

  # # Load dataset 1: When trees are unsighted, a corresponding line appears in the table with status NA
  # load("test_data/example_alive.rda")
  # # Load dataset 2, where unsighted trees have no corresponding line in the table (realistic case)
  # load("test_data/example_alive_mini.rda")
  # # Corresponding hand-corrected datasets
  # load("test_data/expected_2_unseen.rda")
  # load("test_data/expected_2_unseen_mini.rda")
  # load("test_data/expected_3_unseen.rda")
  # load("test_data/expected_3_unseen_mini.rda")

  ## We are going to test two datasets, for 2 modalities of death_confirmation_censuses, which
  ## represents the number of censuses needed to declare dead a tree if unsighted for such a duration.
  # Load dataset 1: When trees are unsighted, a corresponding line appears in the table with status NA
  # data("example_alive")
  # Load dataset 2, where unsighted trees have no corresponding line in the table (realistic case)
  # data("example_alive_mini")
  # Corresponding hand-corrected datasets for death_confirmation_censuses = 3
  # data("expected_2_unseen")
  # data("expected_2_unseen_mini")
  # Corresponding hand-corrected datasets for death_confirmation_censuses = 3
  # data("expected_3_unseen")
  # data("expected_3_unseen_mini")

  names(expected_3_unseen)[which(names(expected_3_unseen)== "status_altern")] <- "status_corr"
  names(expected_3_unseen_mini)[which(names(expected_3_unseen_mini)== "status_altern")] <- "status_corr"

  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]
  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]


  plot1 <- example_alive[which(example_alive$plot == 1),]
  plot2 <- example_alive[which(example_alive$plot == 2),]
  plot1_mini <- example_alive_mini[which(example_alive_mini$plot == 1),]
  plot2_mini <- example_alive_mini[which(example_alive_mini$plot == 2),]

  corrected1 <- expected_2_unseen[which(expected_2_unseen$plot == 1),]
  corrected2 <- expected_2_unseen[which(expected_2_unseen$plot == 2),]
  corrected1_mini <- expected_2_unseen_mini[which(expected_2_unseen_mini$plot == 1),]
  corrected2_mini <- expected_2_unseen_mini[which(expected_2_unseen_mini$plot == 2),]

  corrected1_3 <- expected_3_unseen[which(expected_3_unseen$plot == 1),]
  corrected2_3 <- expected_3_unseen[which(expected_3_unseen$plot == 2),]
  corrected1_mini_3 <- expected_3_unseen_mini[which(expected_3_unseen_mini$plot == 1),]
  corrected2_mini_3 <- expected_3_unseen_mini[which(expected_3_unseen_mini$plot == 2),]

  plot1 <- plot1[,-which(names(example_alive) == "status_corr")]
  plot2 <- plot2[,-which(names(example_alive) == "status_corr")]
  plot1_mini <- plot1_mini[,-which(names(example_alive_mini) == "status_corr")]
  plot2_mini <- plot2_mini[,-which(names(example_alive_mini) == "status_corr")]



  plot1$status_corr <- plot1$status
  plot2$status_corr <- plot2$status
  plot1_mini$status_corr <- plot1_mini$status
  plot2_mini$status_corr <- plot2_mini$status




  res1 <- .correct_status_plotlevel(data_plot = plot1,
                                    dead_confirmation_censuses = 2,
                                    use_size = FALSE,
                                    invariant_columns = NULL,
                                    plots = unique(example_alive$plot),
                                    p = 1)
  res1_3 <- .correct_status_plotlevel(data_plot = plot1,
                                      dead_confirmation_censuses = 3,
                                      use_size = FALSE,
                                      invariant_columns = NULL,
                                      plots = unique(example_alive$plot),
                                      p = 1)

  res1_mini <- .correct_status_plotlevel(data_plot = plot1_mini,
                                         dead_confirmation_censuses = 2,
                                         use_size = FALSE,
                                         invariant_columns = NULL,
                                         plots = unique(example_alive$plot),
                                         p = 1)

  res1_mini_3 <- .correct_status_plotlevel(data_plot = plot1_mini,
                                           dead_confirmation_censuses = 3,
                                           use_size = FALSE,
                                           invariant_columns = NULL,
                                           plots = unique(example_alive$plot),
                                           p = 1)

  row.names(res1) <- 1:nrow(res1)
  row.names(res1_mini) <- 1:nrow(res1_mini)
  row.names(res1_3) <- 1:nrow(res1_3)
  row.names(res1_mini_3) <- 1:nrow(res1_mini_3)


  row.names(corrected1) <- 1:nrow(corrected1)
  row.names(corrected1_mini) <- 1:nrow(corrected1_mini)
  row.names(corrected1_3) <- 1:nrow(corrected1_3)
  row.names(corrected1_mini_3) <- 1:nrow(corrected1_mini_3)

  res1 <- res1[,names(corrected1)]
  res1_mini <- res1_mini[,names(corrected1_mini)]
  res1_3 <- res1_3[,names(corrected1_3)]
  res1_mini_3 <- res1_mini_3[,names(corrected1_mini_3)]

  expect_equal(res1,corrected1)
  expect_equal(res1_mini,corrected1_mini)
  expect_equal(res1_3,corrected1_3)
  expect_equal(res1_mini_3,corrected1_mini_3)

  res2 <- .correct_status_plotlevel(data_plot = plot2,
                                    dead_confirmation_censuses = 2,
                                    use_size = FALSE,
                                    invariant_columns = NULL,
                                    plots = unique(example_alive$plot),
                                    p = 2)
  res2_mini <- .correct_status_plotlevel(data_plot = plot2_mini,
                                         dead_confirmation_censuses = 2,
                                         use_size = FALSE,
                                         invariant_columns = NULL,
                                         plots = unique(example_alive$plot),
                                         p = 2)
  res2_3 <- .correct_status_plotlevel(data_plot = plot2,
                                      dead_confirmation_censuses = 3,
                                      use_size = FALSE,
                                      invariant_columns = NULL,
                                      plots = unique(example_alive$plot),
                                      p = 2)
  res2_mini_3 <- .correct_status_plotlevel(data_plot = plot2_mini,
                                           dead_confirmation_censuses = 3,
                                           use_size = FALSE,
                                           invariant_columns = NULL,
                                           plots = unique(example_alive$plot),
                                           p = 2)

  row.names(res2) <- 1:nrow(res2)
  row.names(res2_mini) <- 1:nrow(res2_mini)
  row.names(res2_3) <- 1:nrow(res2_3)
  row.names(res2_mini_3) <- 1:nrow(res2_mini_3)

  row.names(corrected2) <- 1:nrow(corrected2)
  row.names(corrected2_mini) <- 1:nrow(corrected2_mini)
  row.names(corrected2_3) <- 1:nrow(corrected2_3)
  row.names(corrected2_mini_3) <- 1:nrow(corrected2_mini_3)

  res2 <- res2[,names(corrected2)]
  res2_mini <- res2_mini[,names(corrected2_mini)]
  res2_3 <- res2_3[,names(corrected2_3)]
  res2_mini_3 <- res2_mini_3[,names(corrected2_mini_3)]

  expect_equal(res2,corrected2)
  expect_equal(res2_mini,corrected2_mini)
  expect_equal(res2_3,corrected2_3)
  expect_equal(res2_mini_3,corrected2_mini_3)
})


# Overall function --------------------------------------------------------

test_that("correct_alive gives accurate results for control datasets", {

  # Load dataset 1: When trees are unsighted, a corresponding line appears in the table with status NA
  # data("example_alive")
  # Load dataset 2, where unsighted trees have no corresponding line in the table (realistic case)
  # data("example_alive_mini")
  # Corresponding hand-corrected datasets
  # data("expected_2_unseen")
  # data("expected_2_unseen_mini")
  # data("expected_3_unseen")
  # data("expected_3_unseen_mini")


  example_alive <- example_alive[,which(names(example_alive) != "status_altern")]
  example_alive <- example_alive[,which(names(example_alive) != "status_corr")]
  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_altern")]
  example_alive_mini <- example_alive_mini[,which(names(example_alive_mini) != "status_corr")]

  names(expected_2_unseen)[which(names(expected_2_unseen)== "status_altern")] <- "status_corr"
  names(expected_2_unseen_mini)[which(names(expected_2_unseen_mini)== "status_altern")] <- "status_corr"
  # Change one column name of the initial dataset. Unimportant step.
  names(expected_3_unseen)[which(names(expected_3_unseen)== "status_altern")] <- "status_corr"


  res <- correct_alive(data = example_alive,
                       id_col = "id",
                       time_col = "time",
                       status_col = "status",
                       plot_col = "plot",
                       byplot = TRUE,
                       dead_confirmation_censuses = 2,
                       use_size = FALSE,
                       invariant_columns = NULL)
  res_mini <- correct_alive(data = example_alive_mini,
                            id_col = "id",
                            time_col = "time",
                            status_col = "status",
                            plot_col = "plot",
                            byplot = TRUE,
                            dead_confirmation_censuses = 2,
                            use_size = FALSE,
                            invariant_columns = NULL)
  res_3 <- correct_alive(data = example_alive,
                         id_col = "id",
                         time_col = "time",
                         status_col = "status",
                         plot_col = "plot",
                         byplot = TRUE,
                         dead_confirmation_censuses = 3,
                         use_size = FALSE,
                         invariant_columns = NULL)
  res_mini_3 <- correct_alive(data = example_alive_mini,
                              id_col = "id",
                              time_col = "time",
                              status_col = "status",
                              plot_col = "plot",
                              byplot = TRUE,
                              dead_confirmation_censuses = 3,
                              use_size = FALSE,
                              invariant_columns = NULL)



  row.names(res) <- 1:nrow(res)
  row.names(res_mini) <- 1:nrow(res_mini)
  row.names(res_3) <- 1:nrow(res_3)
  row.names(res_mini_3) <- 1:nrow(res_mini_3)

  row.names(expected_2_unseen) <- 1:nrow(expected_2_unseen)
  row.names(expected_2_unseen_mini) <- 1:nrow(expected_2_unseen_mini)
  row.names(expected_3_unseen) <- 1:nrow(expected_3_unseen)
  row.names(expected_3_unseen_mini) <- 1:nrow(expected_3_unseen_mini)

  res <- res[,names(expected_2_unseen)]
  res_mini <- res_mini[,names(expected_2_unseen_mini)]
  res_3 <- res_3[,names(expected_3_unseen)]
  res_mini_3 <- res_mini_3[,names(expected_3_unseen_mini)]

  expect_equal(res,expected_2_unseen)
  expect_equal(res_mini,expected_2_unseen_mini)
  expect_equal(res_mini_3,expected_3_unseen_mini)
  expect_equal(res_3,expected_3_unseen)

})


# Operations I did to modify or create test datasets ----------------------

# convert xls to rda
#
# fil <- list.files(file.path(getwd(),"data"))
# nams <- c("test_data/example_alive.rda","test_data/example_alive_mini.rda","test_data/expected_2_unseen.rda","test_data/expected_3_unseen.rda")
#
# for(i in 1:4){
#   assign(nams[i], read.csv2(file.path(getwd(),"data",fil[i])))
#   print(i)
#   # save(get(nams[i]), file = paste0(nams[i],'.rda'))
#   do.call(save, list(nams[i], file=file.path(getwd(),"data",paste(nams[i], "rda", sep = "."))))
# }
#
# save(get("test_data/example_alive.rda"), file = "test_data/example_alive.rda")
# file.path(getwd(),"data",paste(nams[i], "rda", sep = "."))

# load("test_data/expected_3_unseen.rda")
# expected_3_unseen[which(is.na(expected_3_unseen$status)),c("col1","col2")] <- NA
# expected_3_unseen_mini <- expected_3_unseen
# save(expected_3_unseen_mini, file = "data/expected_3_unseen_mini.rda")


# identical(expected_2_unseen,expected_2_unseen_mini)
# View(expected_2_unseen_mini)
# View(expected_2_unseen)
# nrow(expected_2_unseen_mini)
# for(i in 1:142){
#   line = paste0(expected_2_unseen[i,], collapse = "_")
#   if(line != paste0(expected_2_unseen_mini[i,], collapse="_")){
#     print(i)
#   }
# }
#
