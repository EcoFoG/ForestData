# Unit tests for correct_alive function -----------------------------------
context("Mortality")
compute_mortality(res,
                  alive_col="status_corr",
                  time_col="time",
                  id_col="id",
                  dead_confirmation_censuses=2,
                  byplot = TRUE,
                  plot_col = "plot",
                  corrected = TRUE)


data <- res


data <- data[order(data$id,data$time),]

data$id <- as.character(data$id)
data$id_lag = c(NA, data$id[-length(data$id)])

data$status_lagged <- c(NA, data$status_corr[-length(data$status_corr)])
data$status_lagged[which(data$id != data$id_lag)] <- NA
# data$id %>% class
# data$id_lag %>% class

data$time_lagged <- c(NA, data$status[-length(data$status)])
data$time_lagged[which(!data$id == data$id_lag)] <- NA


plots <- unique(data$plot)

times <- sort(unique(data$time[which(data$plot == plots[1])]), decreasing = F)
mortality <- data.frame(time = paste(times[-((length(times)-(dead_confirmation_censuses-1)):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                        annual_deathrate = NA,
                        plot = plots[1])
mortality$time
for(p in plots[-1]){
  times <- sort(unique(data$time[which(data$plot == p)]), decreasing = F)
  temp <- data.frame(time = paste(times[-((length(times)-(dead_confirmation_censuses-1)):length(times))],times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))], sep = "_" ),
                     annual_deathrate = NA,
                     plot = p)
  mortality <- rbind(mortality, temp)
  # times
  # times[-((length(times)-(dead_confirmation_censuses-1)):length(times))]
  # times[c(-1,-((length(times)-(dead_confirmation_censuses-2)):length(times)))]
  rm(temp); rm(times)
}

mortality
p <- 1
dead_confirmation_censuses = 2
.compute_mortality_plotlevel(data[which(data$plot == p),],
                             mortality[which(mortality$plot == p),],
                             dead_confirmation_censuses
)

toto



