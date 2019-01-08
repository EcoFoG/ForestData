#' Title
#'
#' @param data
#' @param alive_col
#' @param time_col
#' @param id_col
#' @param dead_confirmation_censuses
#' @param byplot
#' @param plot_col
#' @param corrected
#'
#' @return
#' @export
#'
#' @examples
compute_recruitment <- function(data,
                              alive_col="status_corr",
                              time_col,
                              id_col,
                              dead_confirmation_censuses=2,
                              byplot = TRUE,
                              plot_col = "plot",
                              corrected = TRUE){

  # Checks ------------------------------------------------------------------

  if(!is.data.frame(data)){
    stop("data must be a data.frame object")
  }

  if(!is.logical(byplot)){
    stop("byplot must be logical")
  }

  data <- check_rename_variable_col(time_col, "time_col",data)
  data <- check_rename_variable_col(alive_col, "status_corr_col",data)
  data <- check_rename_variable_col(id_col, "id_col",data)
  if(byplot) data <- check_rename_variable_col(plot_col, "plot_col",data)

  if(!corrected){
    data <- correct_alive(data,alive_col=alive_col,time_col=time_col)
    warning("You specified that your dataset was not corrected beforehand. It has been automatically corrected prior to recruitment rate computation.")
  }
  # prepare dataset ---------------------------------------------------------
  # print(order(data$id,data$time))
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
    recruitment <- data.frame(time = paste(times[-((length(times)-(dead_confirmation_censuses-1)):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                            annual_recruitment_rate = NA,
                            plot = plots[1])
    recruitment$time
    for(p in plots[-1]){
      times <- sort(unique(data$time[which(data$plot == p)]), decreasing = F)
      temp <- data.frame(time = paste(times[-((length(times)-(dead_confirmation_censuses-1)):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                         annual_recruitment_rate = NA,
                         plot = p)
      recruitment <- rbind(recruitment, temp)
      # times
      # times[-((length(times)-(dead_confirmation_censuses-1)):length(times))]
      # times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))]
      rm(temp); rm(times)
    }
  }
  else{
    times <- sort(unique(data$time), decreasing = F)
    recruitment <- data.frame(time = paste(times[-((length(times)-dead_confirmation_censuses):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                            annual_recruitment_rate = NA)
  }


  if(byplot){
    plots <- unique(data$plot)
    for(p in plots){
      print(recruitment[which(recruitment$plot == p),])
      recruitment <- rbind(recruitment[which(recruitment$plot != p),],
                         .compute_recruitment_plotlevel(data[which(data$plot == p),],
                                                      recruitment[which(recruitment$plot == p),],
                                                      dead_confirmation_censuses
                         )
      )
    }
    return(recruitment)
  }
  else return(.compute_recruitment_plotlevel(data, recruitment, dead_confirmation_censuses))
}


# internals ---------------------------------------------------------------

.compute_recruitment_plotlevel <- function(data_plot, recruitment_plot, dead_confirmation_censuses){



  times <- sort(unique(data_plot$time), decreasing = F)

  # print(1:(length(times) - max(1,dead_confirmation_censuses-1)))
  for(i in 1:(length(times) - max(1,dead_confirmation_censuses-1))){
    t0 <- times[i]
    t1 <- times[i+1]

    N0 <- sum(!is.na(data_plot$status_corr) &
                data_plot$time == t1 &
                data_plot$status_corr == 1)
    # N1 <- sum(!is.na(data_plot$status_corr) &
    #             !is.na(data_plot$status_lagged) &
    #             data_plot$time == t1 &
    #             data_plot$status_corr == 1 &
    #             data_plot$status_lagged == 1)
    N1 <- sum(data_plot$time == t1 &
                !is.na(data_plot$status_corr)&
                data_plot$status_corr == 1 &
                is.na(data_plot$status_lagged))
    print(paste("t0 :", t0))
    print(paste("N0 :", N0))
    print(paste("t1 :", t1))
    print(paste("N1 :", N1))
    # print(data$status_corr == 1)
    # print(data$status_lagged)
    print("next")
    recruitment_plot[which(recruitment_plot$time == paste(t0, t1, sep = "_")),"annual_recruitment_rate"] <- ((N1/N0) ^ (1/(t1-t0)))

  }
  return(recruitment_plot)
}

