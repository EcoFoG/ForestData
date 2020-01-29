#' Compute Annual Recruitment Rates in Forest Inventories
#'
#' This function computes recruitment in forest inventories according to plot
#'
#' @param data A data.frame containing a time-series tree-wise forest inventory -i.e. every line is a single tree measurement for a single year.
#' @param status_col Character. The name of the column containing tree vital status - 0=dead; 1=alive.
#' @param time_col Character. The name of the column containing census year
#' @param id_col Character. The name of the column containing trees unique ids
#' @param dead_confirmation_censuses Integer, defaults to 2. This is the number of censuses needed to state that a tree is considered dead, if unseen. In Paracou, we use the rule-of-thumb that if a tree is unseen twice, its probability to be actually dead is close to 1. The choice of this value involves that trees unseen during the X-1 last inventories can not be corrected for death, and thus mortality rates should not be calculated for these censuses.
#' @param plot_col Character. The name of the column containing the plots indices.
#' @param byplot Logical. If there are several plots in your dataset, the correction is performed by plot, in case these would not be censuses the same years or with the same frequencies one another.
#' @param corrected Logical. Indicates whether the dataset has been corrected for errors in tree life status; if not it will be corrected beforehand using correct_alive function
#'
#' @return A data.frame with absolute and annual recruitment rates by plot and census interval.
#' @export
#'
#' @examples
#' \dontrun{
#' data("Paracou6")
#' compute_recruitment(Paracou6,
#' status_col="status_corr",
#' time_col="CensusYear",
#' id_col="idTree",
#' dead_confirmation_censuses=2,
#' byplot = TRUE,
#' plot_col = "Plot",
#' corrected = TRUE)
#' }
compute_recruitment <- function(data,
                                status_col="status_corr",
                                time_col=ifelse(is.null(getOption("time_col")),
                                                "CensusYear",
                                                getOption("time_col")),
                                id_col=ifelse(is.null(getOption("id_col")),
                                              "idTree",
                                              getOption("id_col")),
                                dead_confirmation_censuses=2,
                                byplot = TRUE,
                                plot_col = ifelse(is.null(getOption("plot_col")),
                                                  "Plot",
                                                  getOption("plot_col")),
                                corrected = TRUE){

  # Checks ------------------------------------------------------------------

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!is.logical(byplot)){
    stop("byplot must be logical")
  }

  data <- check_rename_variable_col(time_col, "time_col",data)
  data <- check_rename_variable_col(status_col, "status_corr_col",data)
  data <- check_rename_variable_col(id_col, "id_col",data)
  if(byplot) data <- check_rename_variable_col(plot_col, "plot_col",data)

  if(!corrected){
    print(names(data))
    data <- correct_alive(data,
                          status_col="status_corr",
                          id_col = "id",
                          time_col="time",
                          byplot=byplot,
                          plot_col = ifelse(byplot, "plot",plot_col),
                          dead_confirmation_censuses = dead_confirmation_censuses,
                          invariant_columns = ifelse(byplot,
                                                     c("plot","id"),
                                                     "id"),
                          use_size = F)
    warning("You specified that your dataset was not corrected beforehand. It has been automatically corrected prior to recruitment rate computation.")
  }
  # prepare dataset ---------------------------------------------------------
  data <- data[order(data$plot,data$id,data$time),]

  if(is.factor(data$id)) {
    data$id <- as.character(data$id)
  }

  data$id_lag = c(NA, data$id[-length(data$id)])

  data$status_lagged <- c(NA, data$status_corr[-length(data$status_corr)])
  data$status_lagged[which(!data$id == data$id_lag)] <- NA

  data$time_lagged <- c(NA, data$status[-length(data$status)])
  data$time_lagged[which(!data$id == data$id_lag)] <- NA

  # Call internals by plot or not --------------------------------------------


  if(byplot){

    plots <- unique(data$plot)

    times <- sort(unique(data$time[which(data$plot == plots[1])]), decreasing = F)
    recruitment <- data.frame(interval = paste(times[-((length(times)-(dead_confirmation_censuses-1)):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                              time = times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))],
                              annual_recruitment_rate = NA,
                              plot = plots[1])
    # recruitment$time
    for(p in plots[-1]){
      times <- sort(unique(data$time[which(data$plot == p)]), decreasing = F)
      temp <- data.frame(interval = paste(times[-((length(times)-(dead_confirmation_censuses-1)):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                         time = times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))],
                         annual_recruitment_rate = NA,
                         plot = p)
      recruitment <- rbind(recruitment, temp)
      rm(temp); rm(times)
    }
  }
  else{
    times <- sort(unique(data$time), decreasing = F)
    recruitment <- data.frame(interval = paste(times[-((length(times)-dead_confirmation_censuses):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                              time = times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))],
                              annual_recruitment_rate = NA)
  }


  if(byplot){
    plots <- unique(data$plot)
    for(p in plots){
      # print(recruitment[which(recruitment$plot == p),])
      recruitment <- rbind(recruitment[which(recruitment$plot != p),],
                         .compute_recruitment_plotlevel(data[which(data$plot == p),],
                                                      recruitment[which(recruitment$plot == p),],
                                                      dead_confirmation_censuses
                         )
      )
    }
  }
  else recruitment <- .compute_recruitment_plotlevel(data, recruitment, dead_confirmation_censuses)

  ## Add time column and fix plot
  if(byplot)
    # print(plot_col)
    if(!is.factor(recruitment[,"plot"])) recruitment[,"plot"] <- factor(recruitment[,"plot"])

  return(recruitment)

}



# internals ---------------------------------------------------------------

.compute_recruitment_plotlevel <- function(data_plot, recruitment_plot, dead_confirmation_censuses){



  times <- sort(unique(data_plot$time), decreasing = F)

  for(i in 1:(length(times) - max(1,dead_confirmation_censuses-1))){
    t0 <- times[i]
    t1 <- times[i+1]

    N0 <- sum(!is.na(data_plot$status_corr) &
                data_plot$time == t1 &
                data_plot$status_corr == 1)

    N1 <- sum(data_plot$time == t1 &
                !is.na(data_plot$status_corr)&
                data_plot$status_corr == 1 &
                is.na(data_plot$status_lagged))

    recruitment_plot[which(recruitment_plot$interval == paste(t0, t1, sep = "_")),"annual_recruitment_rate"] <- ((N1/N0) ^ (1/(t1-t0)))

  }
  return(recruitment_plot)
}

