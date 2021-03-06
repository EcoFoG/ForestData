#' Title
#'
#'display_corrected_sizes allows to visualise the corrections performed on tree size measurements using ggplot2 utilities to make a points graph displaying raw (uncorrected) and corrected measurement series for censused trees, with all the specified relevant information.
#'
#' @inheritParams correct_all
#' @inheritParams display_rates
#' @param code_corr_col Single character, the name of the column containing tree correction codes (see the output of correct_size)
#' @param size_corr_col Single character, the name of the column containing tree corrected size measurements
#' @param status_corr_col Single character, the name of the column containing tree corrected status
#' @param info_cols Single or multiple character, the name(s) of the column containing the information that has to appear in the graphs' subtitle.
#' @param tag_pom Logical, whether POM shifts should be displayed with a vertical line on the graph, or not. Use only if you have a POm field in your dataset.
#'
#' @return Nothing, in practical terms, this function is designed to save the graphs directly into a directory.
#' @export
display_corrected_trees <- function(data,
                                    code_corr_col="code_corr",
                                    size_corr_col = "size_corr",
                                    size_col,
                                    plot_col = "plot",
                                    measure_type = "Circ",
                                    time_col = "CensusYear",
                                    status_corr_col = "status_corr",
                                    id_col = "id",
                                    info_cols = c("species", "plot"),
                                    path_save = "./graphs",
                                    name = "diameter_corrections_",
                                    tag_pom = FALSE,
                                    create_folder = TRUE,
                                    overwrite = TRUE,
                                    device = "jpeg",
                                    save_graph = TRUE){

# Checks ------------------------------------------------------------------
  if(!is.data.frame(data))
    stop("data must be a data.frame object")



  data <- check_rename_variable_col(code_corr_col, "code_corr_col",data)
  data <- check_rename_variable_col(time_col, "time_col",data)
  data <- check_rename_variable_col(size_col, "size_col",data)
  data <- check_rename_variable_col(plot_col, "plot_col",data)
  data <- check_rename_variable_col(size_corr_col, "size_corr_col",data)
  data <- check_rename_variable_col(status_corr_col, "status_cor_col",data)

  if(pmatch(measure_type, "Circumferenre")== 1 | pmatch(measure_type, "circumferenre")== 1){
    data$size <- data$size/pi
    data$size_corr <- data$size_corr/pi
  }
  else if(!(pmatch(measure_type, "Diameter")== 1 | pmatch(measure_type, "diameter")== 1)){
    stop("Argument measure_type must partially match either 'Circumference' or 'Diameter'")
  }

  for(n in info_cols){
    if(!n %in% names(data))
      stop(paste(n,"is not a dataset's column name"))
  }

  # .test_install_package("ggrepel","display_corrected_trees")
  if(!dir.exists(path_save)){
    if(create_folder){
      dir.create(path_save)
      message("The directory has been created")
    }
    else stop("Cannot access the specified directory to save the graphs.")
  }

  if(!isFALSE(tag_pom)){
    if(!tag_pom %in% names(data)){
      stop("tag_pom defaults to false, but must contain the name of the column containing the Point Of Measurement if you want to mark POM shifts with a vertical dotted line")
    }
    else{
      data <- data[order(data$id, data$time),]
      data$pom_lag <- c(NA, data$pom[-nrow(data)])
      data$id_lag <- c(NA, data$id[-nrow(data)])
      data$shift <- ifelse(!is.na(data$pom) & !is.na(data$pom_lag),
                           ifelse(data$pom != data$pom_lag,
                                  ifelse(!is.na(data$id_lag) & data$id == data$id_lag,
                                         TRUE,
                                         FALSE),
                                  FALSE),
                           FALSE
                           )
      data <- data[,!names(data) %in% c("id_lag","pom_lag")]
    }
  }
  ## save
  if(!is.logical(save_graph)){
    stop("Argument save_graph must be logical (TRUE/FALSE)")
  }
  else{
    if(save_graph){
      #device
      if(!is.character(device)){
        stop("device must be a character (see ggsave() documentation for explanation)")
      }
      else if(!length(device) == 1){
        stop("Please select one, and only one device.")
      }
      #path
      if(!(is.character(path_save) & length(path_save) == 1)){
        stop("path_save must be a character of length 1")
      }
      #name
      if(!(is.character(name) & length(name) == 1)){
        stop("name must be a character of length 1")
      }

      #create_folder
      if(!is.logical(create_folder)){
        stop("Argument create_folder must be logical.")
      }
      #overwrite
      if(!is.logical(overwrite)){
        stop("Argument overwrite must be logical.")
      }
    }
  }


# Extract ids -------------------------------------------------------------


  ids_corr <- data[!is.na(data$code_corr) & data$code_corr != 0,which(names(data)== id_col)]
  ids_corr <- unique(ids_corr)

  data_corr <- data[data$id %in% ids_corr & data$status_corr == 1,]

  data_corr <- data_corr[,c("id","time","plot","size","size_corr", info_cols[!info_cols%in%c(plot_col, id_col, size_col)])]
  names(data_corr)[1] <- "id_t"


  names(data_corr)[which(names(data_corr) %in% c("time","plot","size","size_corr"))] <- c("census","plot","size.1","size.2")
# print(head(data_corr))
  #Use base's reshape function
  reshaped <- stats::reshape(data_corr,        #dataframe
                             direction="long",       #wide to long
                             varying=c(4,5),
                             times = c("Raw","Corrected"),
                             timevar = "size",
                             v.names = "value")

  #Take away the last column, that is useless

  reshaped<-reshaped[,-length(names(reshaped))]

  names(reshaped)[names(reshaped) == "id_t"] <- "id"
  #Set the rownames back to 1:nrow(reshaped)
  row.names(reshaped) <- NULL
  # print(head(reshaped))


  # variables = c("c","d")
  # test[,variables[1]] <- "c";test[,variables[2]] <- "d"
  # paste0(paste(variables,unique(test[,variables]), sep = " : "),collapse = " ; ")
 print(ids_corr)

  for(i in ids_corr){

    tree <- reshaped[reshaped$id == i,]
    print(tree)
    recruitment <- min(tree$census)
    death <- max(tree$census)
    # info_vals <- unique(tree[,info_cols])
    # print(c(info_cols, info_vals))
    # ic <- c("spcies")
    # iv <- c("zefzegfz")
    # paste(ic,paste0(iv, collapse = "/"),sep = " : ")

    subtitle <- paste(paste0("Individual: ",i), paste0("Plot: ", unique(tree$plot)), sep = " - ")
    for(c in info_cols[!info_cols%in%c(plot_col, id_col, size_col)]){
      subtitle <- paste(subtitle, paste(c,paste0(unique(tree[,c]),collapse="/"),sep = ": "), sep = " - ")
    }

    # subtitle <- paste0(c(paste(paste(info_cols, info_vals, sep = ": ")), collapse=" / "))
    print(subtitle)
    title <- paste0("Raw and Corrected ", measure_type," for tree ",i)


      g <- ggplot2::ggplot(data=tree, mapping=ggplot2::aes_string(x="census", y="value", color="size"))+
        ggplot2::geom_point()+
        ggrepel::geom_text_repel(ggplot2::aes(label = tree$size),size = 3)
      if(!isFALSE(tag_pom) & any(tree$shift)){
        g <- g+
          ggplot2::geom_vline(xintercept = tree$time[which(tree$shift)],
                     color = "brown",
                     size = 1,
                     linetype = 2,
                     alpha = 0.4)
      }

      g <- g +
        ggplot2::ggtitle(label = title, subtitle = subtitle)+
        ggplot2::scale_x_continuous(breaks = recruitment:death)+
        ggplot2::xlab("Census year")+
        ggplot2::ylab("Diameter (cm)")+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45))+
        ggplot2::labs(color = "Series")

      ggplot2::ggsave(g, filename = file.path(path_save,paste0(name,i,".",device)), device = device)
  }
}
