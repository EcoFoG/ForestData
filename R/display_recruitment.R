#' Display recruitment Rates with ggplot2
#'
#' @param recruitment data.frame, output of compute_recruitment
#' @param type character, partially matching one of the following: "line", "histogram", "bar", "smooth". For the moment, only "line" is implemented, being the most relevant in this case. Corresponds to the type of graph to be done, for examples see ggplot2 examples for geom_line, geom_histogram, geom_bar and geom_smooth.
#' @param time_col Character, name of the column corresponding to census time
#' @param color_col Character, name of the colomn used to define lines' colors, defaults to "Plot".
#' @param faceting Character, name of the variable used for faceting -see after- but defaults to FALSE i.e. no faceting. Faceting refers, here, to using a categorical, grouping variable to layout multiple plots, each corresponding to a category of the grouping variable. For example, one can do a single graph per station -if there are several forest plots in each station-, or one per taxon if looking to several specific taxa' recruitment and recruitment rates in detail. If offers the advantage to avoir plotting too much information on a graph, but it can lead to difficulties to compare lines with one another. The scales are free on the x axis -basically, it correspond to several groups not necessarily having the same censusing temporal resolution-, but are bound on y -i.e., the rates are displayed on the same scale for comparison purposes.
#' @param title Character,title of the graph.
#' @param subtitle Character, defaulting to null. The subtitle of the graph
#' @param save_graph Logical, indicates whether the graph must be saved or not. If TRUE, please set the above described arguments in an appropriate way.
#' @param device Character, the graphical device to be used to save the graph
#' @param path_save Character, a path indicating in which FOLDER the graph has to be saved
#' @param name Character, the name of the folder containing the graph. It can be followd by the extension corresponding to the device - avoid .jpg for the jpeg device, use .jpeg instead. If the extension is missing, it is automatically added according to the selected device.
#' @param create_folder Logical, indicated whether the folders in the given path must be created in case they do not exist yet, or not
#' @param overwrite Logical, indicating whether a file already existing under the same name must be overwritten, or kept. In the second case, the function aborts with an explicit error message.
#'
#' @export
#' @return A ggplot2 graphical object.
#'
#' @examples
display_recruitment <- function(recruitment = NULL,
                          type = "line",
                          time_col = "time",
                          color_col = "plot",
                          faceting = FALSE,
                          title = "Annual recruitment and recruitment rates in function of census intervals.",
                          subtitle = NULL,
                          save_graph = FALSE,
                          device="png",
                          path_save = file.path("ForestGraphs",paste0("annual_recruitment_recruitment_",type,".png")),
                          name = "Annual recruitment and Recruitment rates for each between-census interval",
                          create_folder = FALSE,
                          overwrite = FALSE,
                          ...){


  # Checks ------------------------------------------------------------------



  ## recruitment and recruitment, or merged
  if(!is.data.frame(recruitment)){
    stop("The recruitment table must be a data.frame outputed by the function compute_recruitment, or of the exactly same format")
  }

  ## character arguments
  if(!is.character(type) & length(type) == 1){
    stop("The 'type' argument must be a character of length one")
  }
  else if(!type %in% c("line","histogram","barplot")){
    stop("argument 'type' must be one of the following: line, histogram, barplot.")
  }

  if(!(time_col %in% names(merged) & length(time_col)==1)){
    stop("The name of the dataset's column containing census intervals (time_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
  }
  if(!(color_col %in% names(merged) & length(color_col)==1)){
    stop("The name of the dataset's column containing the categories used as colors (color_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
  }


  ## Package ggplot2
  test <-.test_install_package("ggplot2","display_rates")
  if(!test == 0){
    stop("ggplot2 is needed to run display_rates, but unavailable...")
  }

  ## Faceting variable
  if(!isFALSE(faceting)){
    if(!is.character(faceting))
      stop("faceting must be a character of length 1")
    if(!length(faceting)== 1){
      stop("Please specify only 1 faceting column.")
    }
    else if(!faceting %in% names(reshaped)){
      stop("The faceting variable you indicated is apparently not in your dataset")
    }
  }
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
  trans <- 0.6
  lw <- 0.72
  x.angle <- 90
  y.angle <- 0
  position <- "position"
  linetype <- NULL

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
           warning(paste0("argument ", a, " is unused")))
  }

  # Do the appropriate graph ------------------------------------------------
  switch(type,
         "line" = {graph <- .do_graph_line(recruitment,
                                           x_variable = "time",
                                           y_variable = "value",
                                           colour = color_col,
                                           linetype = linetype,
                                           title = title,
                                           subtitle = subtitle,
                                           x.axis.name = x.name ,
                                           y.axis.name = y.name,
                                           transparence = trans,
                                           linewidth = lw,
                                           x.text.angle = x.angle,
                                           y.text.angle = y.angle)},
         "barplot" = {graph <- .do_graph_barplot(reshaped,
                                                 x_variable = "time",
                                                 y_variable = "value",
                                                 fill = color_col,
                                                 position = position,
                                                 title = title,
                                                 subtitle = subtitle,
                                                 x.axis.name = x.name,
                                                 y.axis.name = y.name,
                                                 x.text.angle = x.angle,
                                                 y.text.angle = y.angle)},
         "histogram" = {graph <- .do_graph_histogram(reshaped,
                                                     x_variable,
                                                     color_col,
                                                     title = title,
                                                     subtitle = subtitle,
                                                     x.axis.name =  "Annual rate",
                                                     y.axis.name = "Count")},
         stop("Type must be one of the following: line, histogram, bar")
  )


  # Facet, or not -----------------------------------------------------------


  if(!isFALSE(faceting)){
    graph <- graph+facet_wrap(as.formula(paste("~", faceting)), scales = "free.x")
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

