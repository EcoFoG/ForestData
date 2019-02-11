#' Correct Overgrown Recruits in Forest Inventories
#'
#'Spot individuals that have first been unseen during previous censuses but were already over the minimum counting dbh, and provide a correction that is useful to compute biomass estimates.
#'
#' @param data A data.frame containing a forest inventory in which each line corresponds to an individual's measurement for one census
#' @param dbh_min A scalar, integer or numeric, indicating the minimum DIAMETER at breast height at which trees are recorded, in centimeters. Defaults to 10 cm.
#' @param time_col A character indicating the name of the column containing census year information.
#' @param id_col A character indicating the name of the column containing individual tree's IDs
#' @param plot_col A character indicating the name of the column containing plot indices or names.
#' @param size_col A character indicating the name of the column containing tree size measurements -diameters or circumferences-.
#' @param status_col A character indicating the name of the column containing tree vital status - 0 for dead, 1 for alive, NA for unseen. No tree must appear with NA status before their first measurement.
#' @param measure_type A character indicating the size measurement type : "C" for circumference, "D" for diameter. Defaults to "C".
#' @param byplot A logical indicating whether there are several plots in the dataset. Defaults to TRUE
#' @param correct_status A logical indicating whether the dataset was corrected for tree vital status errors beforehand. If FALSE, the correct_mortality function is called first with default parameters - see the function info section.
#' @param diameter_growth_limit A scalar, integer or numeric, indicating the maximum tolerated annual DIAMETER growth rate -in centimeters- over which a tree's growh is considered abnormal. Defaults to 5 cm, as set for Paracou inventories.
#'
#' @return A corrected data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' data("Paracou6")
#' correct_recruits(Paracou6,
#' dbh_min = 10,
#' diameter_growth_limit = 5,
#' time_col = "CensusYear",
#' id_col = "id",
#' plot_col = "Plot",
#' size_col = "Circ",
#' status_col = "AliveCode",
#' measure_type = "circumference",
#' byplot = TRUE,
#' correct_status = FALSE)
#' }
correct_recruits <- function(data,
                             dbh_min = 10,
                             diameter_growth_limit = 5,
                             time_col = "CensusYear",
                             id_col = "id",
                             plot_col = "Plot",
                             size_col = "Circ",
                             status_col = "AliveCode",
                             measure_type = "circumference",
                             byplot = TRUE,
                             correct_status = FALSE){


# Argument checks ---------------------------------------------------------


  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!is.logical(byplot)){
    stop("byplot must be logical")
  }

  if(!is.logical(correct_status)){
    stop("byplot must be logical")
  }

  if(!(is.character(measure_type) & length(measure_type) == 1))
    stop("measure_type must be a character of length 1")
  data <- check_rename_variable_col(time_col, "time_col",data)
  res <- try(check_rename_variable_col("status_corr", "status_corr_col",data))
  if(inherits(res,'try-error') & correct_status == T){
    data <- check_rename_variable_col(status_col, "status_col",data)
  }
  else if (inherits(res,'try-error') & !correct_status){
    data <- check_rename_variable_col(status_col,"status_corr_col",data)
  }

  data <- check_rename_variable_col(id_col, "id_col",data)
  data <- check_rename_variable_col(size_col, "size_col",data)
  if(byplot) data <- check_rename_variable_col(plot_col, "plot_col",data)

  if(correct_status){
    data <- correct_alive(data,
                          id_col = "id",
                          time_col = "time",
                          status_col = "status",
                          plot_col,
                          byplot,
                          dead_confirmation_censuses = 2,
                          use_size = ifelse(is.na(status_col),
                                            size_col,
                                            FALSE)
    )
  }

  if(!measure_type %in% c("circumference",
                          "circ","c",
                          "Circumference",
                          "Circ",
                          "C",
                          "diameter",
                          "diam",
                          "d",
                          "Diameter",
                          "Diam",
                          "D")){
    stop("Please indicate if the tree size measurements (argument measure_type) are circumferences (C) or diameters (D)")
  }

  if(!measure_type %in% c("circumference",
                          "circ","c",
                          "Circumference",
                          "Circ",
                          "C")){
    # data$diameter <- data$circumference/pi
    diameter_growth_limit <- diameter_growth_limit*pi
    dbh_min <- dbh_min*pi
  }


# Call internals by plot or not -------------------------------------------



  if(byplot){
    plots <- unique(data$plot)

    # for(p in plots){
    #   data <- rbind(data[which(data$plot != p),], .correct_recruits_plot(data_plot=data[which(data$plot == p),],
    #                                                                      dbh_min = dbh_min,
    #                                                                      growth_limit = growth_limit))
    # }
    data <- do.call(rbind,lapply(plots, function(p){.correct_recruits_plot(data_plot=data[which(data$plot == p),],
                                                                           dbh_min = dbh_min,
                                                                           growth_limit = growth_limit)}))
  }
  else{
    data <- .correct_recruits_plot(data_plot=data,
                                   dbh_min = dbh_min,
                                   growth_limit = growth_limit)
  }


# Put back colnames and return --------------------------------------------

  names(data)[which(names(data == "id"))] <- id_col
  names(data)[which(names(data == "time"))] <- time_col
  names(data)[which(names(data == "status"))] <- status_col
  names(data)[which(names(data == "size"))] <- size_col
  if(byplot) names(data)[which(names(data == "plot"))] <- plot_col
  return(data)
}

#' Internal function. plot-level correction for overgrown recruits.
#'
#' @param data_plot A data.frame containing a subset of forest inventory corresponding to one plot, formatted as in "correct_recruits".
#' @param dbh_min A scalar, integer or numeric, indicating the minimum DIAMETER at breast height at which trees are recorded, in centimeters.
#' @param growth_limit A scalar, integer or numeric, indicating the maximum tolerated annual DIAMETER growth rate -in centimeters- over which a tree's growh is considered abnormal.
#'
#' @return A data.frame containing the corrected plot
#'
#' @examples
#' \dontrun{
#' data("Paracou6")
#' plots <- unique(Paracou6$Plot)
#' for(p in plots) correct_recruits_plot(Paracou[Paracou$Plot == p,], 10, 5)
#' }
.correct_recruits_plot <- function(data_plot,
                                  dbh_min,
                                  growth_limit){

  ids <- unique(data_plot$id)
  censuses <- unique(data_plot$time)


  for(i in ids){
    tree_corrected <- .correct_recruits_tree(data_plot[which(data_plot$id == i),],
                                             dbh_min = dbh_min,
                                             growth_limit = growth_limit,
                                             censuses = censuses)

    if(!identical(data_plot[which(data_plot$id == i),], tree_corrected)){
      data_plot <- rbind(data_plot[which(data_plot$id != i),],tree_corrected)
      print("ok")
    }
  }
}

#' Internal function. tree-level correction for overgrown recruits.
#'
#' @param dbh_min A scalar, integer or numeric, indicating the minimum DIAMETER at breast height at which trees are recorded, in centimeters.
#' @param growth_limit A scalar, integer or numeric, indicating the maximum tolerated annual DIAMETER growth rate -in centimeters- over which a tree's growh is considered abnormal.
#' @param tree A data.frame containing one single tree's time series of measurements.
#' @param censuses A numeric vector containing all the years for which the plot to which the tree belongs were censused.
#'
#' @return A data.frame containing the corrected indiv
#'
#' @examples
#' \dontrun{
#' data("Paracou6")
#' cens <- unique(Paracou6$CensusYear)
#' ids <- unique(Paracou6$idTree)
#' for(i in ids) correct_recruits_tree(Paracou[Paracou$idTree == i,], 10, 5,cens)
#' }
.correct_recruits_tree <- function(tree,
                                   dbh_min,
                                   growth_limit,
                                   censuses){
  #Make sure that it is ordered by increasing census time
  tree <- tree[order(tree_temp$time),]

  recruitment_time <- min(tree$time)

  if(min(tree$time)>min(censuses) & sum(!is.na(tree$diameter))>0){
    prev_inv <- censuses[which(censuses == recruitment_time)-1]
    if(tree$diameter[1] > dbh_min +(recruitment_time-prev_inv)*growth_limit){

      missing_censuses <- censuses[which(censuses < recruitment_time)]

      # Create new rows, but without knowing how many column are in data and what they contain !
      if(length(missing_censuses) > 0){
        if("plot" %in% names(tree)){
          new.rows <- data.frame(id = i,
                                 time = missing_censuses,
                                 plot = rep(tree_temp$plot[1],length(missing_censuses)))
        }
        else{
          new.rows <- data.frame(id = i,
                                 time = missing_censuses)
        }

        # Add other columns which names are unknown (to allow subsequent rbinding), and default-set it to NA
        new.rows[,names(tree)[-which(names(new.rows)%in%names(tree))]] <- NA
        tree[(nrow(tree)+1):(nrow(tree)+nrow(new.rows)),names(new.rows)] <- new.rows
      }

      #Make sure that it is ordered by increasing census time
      tree <- tree[order(tree_temp$time),]

      coef <- lm(tree$diameter[which(!is.na(tree$diameter))]~tree$time[which(!is.na(tree$diameter))])$coefficients

      if (is.na(coef[2])) {
        # tree$diameter[which(is.na(tree$status))] <- coef[1]
        tree$diameter[which(tree$time < recruitment_time)] <- coef[1]
      } ### only 1 dbh value: replace all non-recruited dbh by this value
      else {
        tree$diameter[which(tree$time < recruitment_time)] <- min(coef[1] + tree$time[which(is.na(tree$status))]*coef[2],
                                                        tree$diameter[which(!is.na(tree$diameter))][1])
      }
    }
  }
  return(tree)
}




