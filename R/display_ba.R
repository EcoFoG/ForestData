#' Display Forest Mortality and Recruitment Data with ggplot2
#'
#' display_ba take a basal area table, obtained with thes \code{compute_ba}
#' function, and returns a nice graph according to user-specified parameters.
#' This function requires the installation of the package
#' \code{\link[ggplot2]{ggplot2}}.
#'
#' @param basal_area data.frame, output of compute_ba, or if obtained another
#'   way, with a column named "basal_area_per_ha" or adding an argument
#'   variable.name, containing the corresponding column name, to the function
#'   call. Note that if the basal_area have been computed other than by plot,
#'   the arguments of this function must be set accordingly.
#' @inheritParams display_rates
#'
#' @details
#' \strong{\emph{Displaying multiple variables to avoid unreadability}}
#'
#' Imagine a basal area \code{table} computed for 3 species in 4 different
#' plots, for 20 censuses, with corresponding columns named \code{"Plot"},
#' \code{"Species"} and \code{"time"}. Then, imagine displaying it with this
#' function using \code{type = 'line'}. All this information can be displayed in
#' one call of display_ba, with different possible combinations:
#'
#' - One graph per forest plot, with the color of each line segregating species.
#' This can be done with the arguments \code{color_col = "Species"} and
#' \code{faceting = "Plot"}.
#'
#' - One graph per species, with the color of each line segregating plots. The
#' function call has thus the arguments \code{color_col = "Species"} and
#' \code{faceting = "Plot"}.
#'
#' The linetype,defaults to 1 -i.e. solid line- but can also be used to display
#' additional categories, simply adding e.g. \code{linetype  = "Species"} to the
#' function call. In this case, it must be done with some caution: if there are
#' too many categories in the grouping variable, they would hardly -if not
#' impossibly- be visually segregated by line types.
#'
#' \strong{\emph{Detail of the additionnal arguments}}
#'
#' The arguments corresponding to \code{...} here encompass a limited number of
#' parameters that are \emph{internally set if not specified}. These parameters
#' are passed to different sub-functions. Here is the detailed list of these
#' arguments.
#'
#' \code{x.axis.name} and \code{y.axis.name} are the axis labels to be passed to
#' \code{ggplot2::xlab} and \code{ggplot2::ylab}
#'
#' \code{transparence} is the scalar value of \code{alpha} in
#' \code{ggplot2::geom_*} functions.
#'
#' \code{linewidth} is the scalar value of \code{size} in
#' \code{ggplot2::geom_line} -to be used with
#' \code{type='line'}.
#'
#' Note that a variable's name -in the dataset- can also be provided to make
#' vary these parameters according to the data. In such case, these arguments
#' are passed to \code{ggplot2::aes_string} instead.
#'
#' display_ba originally uses \code{linetype=1} with \code{type = 'line'}. It
#' means that linetype is passed to
#' \code{ggplot2::geom_line} as it is a constant scalar.
#' In case the user wants to use it to differenciate groups, and use a variable
#' \code{linetype} they just have to specify e.g.
#' \code{linetype="variable_name"} and it will be passed to
#' \code{ggplot2::aes_string} instead.
#'
#' \code{x.text.angle} and \code{y.text.angle} is the angle of the axis ticks
#' labels. It is set to be 0 -horizontal- for y and 90 -vertical- for x, because
#' it helps reading census times by avoiding overlaps. These are passed to
#' \code{ggplot2::element_text} within \code{ggplot2::theme}.
#'
#' @return A \code{ggplot2} graphical object. See
#'   \code{\link[ggplot2]{ggplot2}}
#' @export
display_ba <- function(basal_area,
                       type = "line",
                       time_col = "time",
                       color_col = "PlotSub",
                       faceting = FALSE,
                       title = "Basal area per hectare measured at each census",
                       subtitle = NULL,
                       save_graph = FALSE,
                       device="png",
                       path_save = file.path("ForestGraphs",paste0("annual_mortality_recruitment_",type,".png")),
                       name = "annual_recruitment_mortality.png",
                       create_folder = FALSE,
                       overwrite = FALSE,
                       ...){


# Checks ------------------------------------------------------------------



  ## Mortality and recruitment, or merged

  if(!is.data.frame(basal_area)){
    stop("The basal_area table must be a data.frame")
  }


  ## character arguments
  if(!is.character(type) & length(type) == 1){
    stop("The 'type' argument must be a character of length one")
  }
  else if(!type %in% c("line","histogram","barplot")){
    stop("argument 'type' must be one of the following: line, histogram, barplot.")
  }

  if(!(time_col %in% names(basal_area) & length(time_col)==1)){
    stop("The name of the dataset's column containing census intervals (time_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
  }
  if(!(color_col %in% names(basal_area) & length(color_col)==1)){
    # print(names(merged))
    stop("The name of the dataset's column containing the categories used as colors (color_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
  }


  ## Package ggplot2
  # test <-.test_install_package("ggplot2","display_ba")
  # if(!test == 0){
  #   stop("ggplot2 is needed to run display_ba, but unavailable...")
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
y.name <- "Basal Area per hectare"
trans <- 0.6
lw <- 0.72
x.angle <- 90
y.angle <- 0
position <- "position"
linetype <- 1

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
         warning(paste0("argument ", a, " is unused")))
}
# print("ok")

# Do the appropriate graph ------------------------------------------------
names(basal_area)[names(basal_area)==color_col] <- firstup(color_col)
if(!is.null(linetype))
  names(basal_area)[names(basal_area)==linetype] <- ifelse(is.character(linetype),
                                                       firstup(linetype),
                                                       linetype)
else
  print(ifelse(is.null(linetype),
               1,
               firstup(linetype)))
switch(type,
       "line" = {graph <- .do_graph_line(table=basal_area,
                                         x_variable = time_col,
                                         y_variable = "basal_area_per_ha",
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
       "barplot" = {graph <- .do_graph_barplot(basal_area,
                                               x_variable = time_col,
                                               y_variable = "basal_area_per_ha",
                                               fill = color_col,
                                               position = position,
                                               title = title,
                                               subtitle = subtitle,
                                               x.axis.name = x.name,
                                               y.axis.name = y.name,
                                               x.text.angle = x.angle,
                                               y.text.angle = y.angle)},
       # "histogram" = {graph <- .do_graph_histogram(basal_area,
       #                                             x_variable,
       #                                             color_col,
       #                                             title = title,
       #                                             subtitle = subtitle,
       #                                             x.axis.name =  "Annual rate",
       #                                             y.axis.name = "Count")},
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
  else if(!(faceting %in% names(basal_area))){
    if(firstup(faceting) %in% names(basal_area)){

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

