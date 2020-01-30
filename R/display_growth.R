#' Display annual growth rates using ggplot2
#'
#' @param growth A data.frame outputed from compute_growth
#' @param type Character, the type of graph (recommended: "line" or "ribbon")
#' @param growth_variable Character
#' @inheritParams display_rates
#' @param ordered_percentiles Numeric, if plotting a ribbon, please indicate the percentiles used when aggregating the data.
#' @param ... Additional arguments. Notably, verbose = F turns most warnings off
#'
#' @return A ggplot graph
#' @export
#'
#' @examples
#'
#' data(example_size_corr)
#' growth <- suppressWarnings(compute_growth(example_size_corr,
#'                          size_col = "size_corr",
#'                          measure_type = "cir",
#'                          status_col = "CodeAlive",
#'                          id_col= "idTree",
#'                          time_col = "CensusYear",
#'                          what_output = "annual",
#'                          aggregate = TRUE,
#'                          by = c("Plot"),
#'                          stat = "mean",
#'                          percentiles = c(5,95)))
#'  display_growth(growth)
display_growth <- function(growth,
                           type = "ribbon",
                           time_col = "time",
                           color_col = "Plot",
                           growth_variable = "mean",
                           faceting = FALSE,
                           title = "Annual growth in function of census intervals.",
                           subtitle = NULL,
                           save_graph = FALSE,
                           device="png",
                           path_save = file.path("ForestGraphs",paste0("annual_mortality_recruitment_",type,".png")),
                           name = "annual_growth.png",
                           create_folder = FALSE,
                           overwrite = FALSE,
                           ordered_percentiles = c("P5","P95"),
                           ...){

# Checks ------------------------------------------------------------------



## Mortality and recruitment, or growth

  if(!is.data.frame(growth)){
    stop("The growth table must be a data.frame outputed by the function compute_growth, or of the exactly same format")
  }






## character arguments
if(!is.character(type) & length(type) == 1){
  stop("The 'type' argument must be a character of length one")
}
else if(!type %in% c("line","histogram","barplot","ribbon")){
  stop("argument 'type' must be one of the following: ribbon, line, histogram, barplot.")
}

if(!(time_col %in% names(growth) & length(time_col)==1)){
  stop("The name of the dataset's column containing census intervals (time_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
}
if(!(color_col %in% names(growth) & length(color_col)==1)){
  # print(names(growth))
  stop("The name of the dataset's unique column containing the categories used as colors (color_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
}


## Package ggplot2
# test <-.test_install_package("ggplot2","display_growth")
# if(!test == 0){
#   stop("ggplot2 is needed to run display_rates, but unavailable...")
# }


## subtitle
if(!is.null(subtitle)){
  if(!is.character(subtitle) & length(subtitle) == 1){
    stop("subtitle must be a character of length one")
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


# Set the arguments from the ellipsis content -----------------------------
arguments <- list(...)

x.name <- "Census year"
y.name <- "Annual rate"
trans <- ifelse(type=="histogram",
                1,
                0.4)
lw <- 0.72
x.angle <- ifelse(type=="histogram",
                  0,
                  90)
y.angle <- 0
position <- "dodge"
linetype <- "rate"
fill <- color_col

# print(names(arguments))
for(a in names(arguments)){
  switch(a,
         "x.axis.name" = {
           x.name <- arguments[[a]]
         },
         "y.axis.name" = {
           y.name <- arguments[[a]]
         },
         "transparence" = {
           trans <- arguments[[a]]
         },
         "linewidth" = {
           lw <- arguments[[a]]
         },
         "x.text.angle" = {
           x.angle <- arguments[[a]]
         },
         "y.text.angle" = {
           y.angle <- arguments[[a]]
         },
         "linetype" = {
           linetype <- arguments[[a]]
         },
         "fill" = {
           fill <- arguments[[a]]
         },
         "position" = {
           position <- arguments[[a]]
         },
         warning(paste0("argument ", a, " is unused")))
}
# print("ok")

# Do the appropriate graph ------------------------------------------------
names(growth)[names(growth)==color_col] <- firstup(color_col)
if(!is.null(linetype))
  names(growth)[names(growth)==linetype] <- ifelse(is.character(linetype),
                                                       firstup(linetype),
                                                       linetype)
else
  print(ifelse(is.null(linetype),
               1,
               firstup(linetype)))
switch(type,
       "ribbon" = {graph <- .do_graph_ribbon(table = growth,
                                              x_variable = "time",
                                              y_variable = growth_variable,
                                              upper_quantile=ordered_percentiles[1],
                                              lower_quantile=ordered_percentiles[2],
                                              color=color_col,
                                              title = "Default title that can be modified",
                                              subtitle = "Default subtitle that has to be user-specified",
                                              x.axis.name = "Census year",
                                              y.axis.name = "Annual rate",
                                              transparence = trans,
                                              linewidth = lw,
                                              x.text.angle = x.angle,
                                              y.text.angle = y.angle)},
       "histogram" = {graph <- .do_graph_histogram(table=growth,
                                                   x_variable= growth_variable,
                                                   position = "dodge",
                                                   ...,
                                                   title = "Default title that can be modified",
                                                   subtitle = "Default subtitle that has to be user-specified",
                                                   x.axis.name = "Census year",
                                                   y.axis.name = "Annual rate",
                                                   transparence = trans,
                                                   x.text.angle = x.angle,
                                                   y.text.angle = y.angle)},
       "line" = {graph <- .do_graph_line(table=growth,
                                         x_variable = "time",
                                         y_variable = "value",
                                         color = firstup(color_col),
                                         linetype = ifelse(is.character(linetype),
                                                           firstup(linetype),
                                                           linetype),
                                         title = title,
                                         subtitle = subtitle,
                                         x.axis.name = x.name ,
                                         y.axis.name = y.name,
                                         transparence = trans,
                                         linewidth = lw,
                                         x.text.angle = x.angle,
                                         y.text.angle = y.angle)},
       stop("Type must be one of the following: line, histogram, bar")
)


# Facet, or not -----------------------------------------------------------

# print("ok2")
## Faceting variable
if(!isFALSE(faceting)){
  if(!is.character(faceting))
    stop("faceting must be a character of length 1")
  if(!length(faceting)== 1){
    stop("Please specify only 1 faceting column.")
  }
  else if(!(faceting %in% names(growth))){
    if(firstup(faceting) %in% names(growth)){

      graph <- graph+ggplot2::facet_wrap(stats::as.formula(paste("~", firstup(faceting))), scales = "free_x")
    }
    else
      stop("The faceting variable you indicated is apparently not in your dataset")
  }
  else{
    print(faceting)
    graph <- graph+ggplot2::facet_wrap(stats::as.formula(paste("~", faceting)), scales = "free_x")
  }
}



# Save if possible --------------------------------------------------------
if(save_graph){
  .save_graph(graph,
              path_save,
              name,
              device,
              create_folder,
              overwrite)
}
return(graph)
}

