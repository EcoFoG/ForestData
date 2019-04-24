#' Display Forest Mortality and Recruitment Data with ggplot2
#'
#' display_rates take either a rates rates table -obtained with
#' \code{compute_rates}- or two tables -mortality and recruitment, obtained with
#' the corresponding \code{compute_*} functions-, and returns a nice graph
#' according to user-specified parameters. This function requires the
#' installation of the package \code{\link[ggplot2]{ggplot2}}.
#'
#' @param mortality data.frame, output of \code{compute_mortality}.
#' @param recruitment data.frame, output of \code{compute_recruitment}.
#' @param rates data.frame, output of compute_rates or merged outputs of compute_mortality and compute_recruitment.
#' @param type character, partially matching one of the following: "line",
#'   "histogram", "bar", "smooth". For the moment, only "line" is implemented,
#'   being the most relevant in this case. Corresponds to the type of graph to
#'   be done, for examples see \code{ggplot2} examples for \code{geom_line},
#'   \code{geom_histogram}, \code{geom_bar} and \code{geom_smooth}.
#' @param time_col Character, name of the column corresponding to census time
#' @param color_col Character, name of the colomn used to define lines' colors,
#'   defaults to "Plot".
#' @param faceting Character, name of the variable used for faceting -see after-
#'   but defaults to FALSE i.e. no faceting. NB: faceting refers to using a
#'   grouping variable to layout multiple plots, each corresponding to a
#'   category of the grouping variable. See details for a pratical explanation.
#'   The scales are free on the x axis: t correspond to several groups not
#'   necessarily having the same censusing temporal resolution; but are bound on
#'   the y-axis: the values are displayed on the same scale for comparison
#'   purposes.
#' @param title Character,title of the graph.
#' @param subtitle Character, subtitle of the graph. Defaults to \code{NULL}
#' @param save_graph Logical, indicates whether the graph must be saved or not.
#'   Defaults to \code{FALSE}. If \code{TRUE}, please set the above described
#'   arguments in an appropriate way.
#' @param device Relevant if \code{save_graph=TRUE}. Character, the graphical
#'   device to be used to save the graph.
#' @param path_save Relevant if \code{save_graph=TRUE}. Character, a path
#'   indicating in which FOLDER the graph has to be saved.
#' @param name Relevant if \code{save_graph=TRUE}. Character, the name of the
#'   folder containing the graph. It can be followd by the extension
#'   corresponding to the device - avoid .jpg for the jpeg device, use .jpeg
#'   instead. If the extension is missing, it is automatically added according
#'   to the selected device.
#' @param create_folder Relevant if \code{save_graph=TRUE}. Logical, indicated
#'   whether the folders in the given path must be created in case they do not
#'   exist yet, or not.
#' @param overwrite Relevant if \code{save_graph=TRUE}. Logical, indicating
#'   whether a file already existing under the same name must be overwritten, or
#'   kept. In the second case, the function aborts with an explicit error
#'   message.
#' @param ... Additionnal arguments to be passed to inernals, and ggplot2
#'   utilities.
#'
#' @details
#'
#' \strong{\emph{About displaying multiple variables and avoiding
#' unreadability:}}
#'
#' Imagine a mortality and recruitment \code{table} computed for 3 species in 4
#' different plots, for 20 censuses, with corresponding columns named
#' \code{"Plot"}, \code{"Species"} and \code{"time"}. Then, imagine displaying
#' it using \code{type = 'line'}. All this information can be displayed in one
#' call of display_rates, with different possible combinations:
#'
#' - One graph per forest plot, with the color of each line segregating species,
#' and the linetype segregating mortality and recruitement rates. This can be
#' done with the arguments \code{color_col = "Species"} and  \code{faceting =
#' "Plot"}, with the argument \code{linetype} not specified, thus internally
#' defaulted.
#'
#' - One graph per species, with the color of each line segregating plots, and
#' the linetype segregating mortality and recruitement rates. The  function call
#' has thus the arguments \code{color_col = "Species"} and  \code{faceting =
#' "Plot"} again without speciying \code{linetype}.
#'
#' Mortality and recruitment are defaultly segregated with different line types
#' -e.g. full and dotted-, but this variable type can also be used to facet,
#' yielding one plot for mortality rates and another for recruitment: for
#' example, using \code{linetype  = "Species"}, \code{color_col = "Plot"}
#' \code{faceting = "rate"}.
#'
#' In this case, using the linetype additionnal argument to display another
#' variable must be done with some caution: if there are too many categories in
#' this other variable, they would hardly -if not impossibly- be visually
#' segregated by line types.
#'
#' \strong{\emph{Detail of the additionnal arguments}}
#'
#' The arguments corresponding to \code{...} here encompass a limited number of
#' parameters that are internally set if not specified. These parameters are
#' passed to different sub-functions. Here is the detailed list of these
#' arguments.
#'
#' \code{x.axis.name} and \code{y.axis.name} are the axis labels to be passed to
#' \code{ggplot2::xlab} and \code{ggplot2::ylab}
#'
#' \code{transparence} is the scalar value of \code{alpha} in
#' \code{ggplot2::geom_*} functions.
#'
#' \code{linewidth} is the scalar value of \code{size} in
#' \code{gplot2::geom_line} -to be used with \code{type='line'}.
#'
#' Note that a variable's name -in the dataset- can also be provided to make
#' vary these parameters according to the data. In such case, these arguments
#' are passed to \code{ggplot2::aes} instead.
#'
#' \code{linetype} is originally set to differenciate the rates -mortality and
#' recruitment- for use with \code{type = 'line'}. It means that
#' \code{linetype="rate"} -a name set internally as the data is reshaped to long
#' format before displaying. This argument is originally passed to
#' \code{ggplot2::aes_string}. In case the user wants to e.g. facet rates into
#' two separate plots and use a constant \code{linetype} instead, they just have
#' to specify e.g. \code{linetype=1} and it will be passed to \code{geom_line}
#' instead.
#'
#' \code{x.text.angle} and \code{y.text.angle} is the angle of the axis ticks'
#' labels. It is set to be 0 -horizontal- for y and 90 -vertical- for x, because
#' it helps reading census times by avoiding overlaps. These are passed to
#' \code{gplot2::element_text} within \code{ggplot2::theme}
#'
#' @export
#' @return A \code{ggplot2} graphical object. See
#'   \code{\link[ggplot2:ggplot]{ggplot2::ggplot}}.
#'
#' @examples
display_rates <- function(mortality = NULL,
                          recruitment = NULL,
                          rates = NULL,
                          type = "line",
                          time_col = "time",
                          color_col = "plot",
                          faceting = FALSE,
                          title = "Annual mortality and recruitment rates in function of census intervals.",
                          subtitle = NULL,
                          save_graph = FALSE,
                          device="png",
                          path_save = file.path("ForestGraphs",paste0("annual_mortality_recruitment_",type,".png")),
                          name = "annual_recruitment_mortality.png",
                          create_folder = FALSE,
                          overwrite = FALSE,
                          ...){


# Checks ------------------------------------------------------------------



## Mortality and recruitment, or rates
  if(is.null(rates)){
    if(!is.data.frame(mortality)){
      stop("The mortality table must be a data.frame outputed by the function compute_mortality, or of the exactly same format")
    }
    if(!is.data.frame(recruitment)){
      stop("The recruitment table must be a data.frame outputed by the function compute_recruitment, or of exactly the same format")
    }
    rates <- .merge_rates(mortality, recruitment, by = c(time_col, color_col))
  }
  else{
    if(!is.data.frame(rates)){
      stop("The rates rates table must be a data.frame")
    }
    if(!names(rates)[(length(names(rates))-1):length(names(rates))] == c("recruitment","mortality")){
      stop("The rates rates table must be a data.frame outputed by the function compute_rates, or of exactly the same format")
    }
  }

## character arguments
  if(!is.character(type) & length(type) == 1){
    stop("The 'type' argument must be a character of length one")
  }
  else if(!type %in% c("line","histogram","barplot")){
    stop("argument 'type' must be one of the following: line, histogram, barplot.")
  }

  if(!(time_col %in% names(rates) & length(time_col)==1)){
    stop("The name of the dataset's column containing census intervals (time_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
  }
  if(!(color_col %in% names(rates) & length(color_col)==1)){
    # print(names(rates))
    stop("The name of the dataset's column containing the categories used as colors (color_col) is apparently erroneous. It must be a character of length one corresponding to a column name.")
  }


## Package ggplot2
  test <-.test_install_package("ggplot2","display_rates")
  if(!test == 0){
    stop("ggplot2 is needed to run display_rates, but unavailable...")
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

# Merge tables ------------------------------------------------------------

  # DEPRECATED
# rates <- .merge_rates(mortality, recruitment, by = c(time_col, color_col))

# Reshape the table to long format ----------------------------------------

reshaped <- reshape_rates(rates)

# Set the arguments from the ellipsis content -----------------------------
arguments <- list(...)

x.name <- "Census year"
y.name <- "Annual rate"
trans <- 0.6
lw <- 0.72
x.angle <- 90
y.angle <- 0
position <- "position"
linetype <- "rate"

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
names(reshaped)[names(reshaped)==color_col] <- firstup(color_col)
if(!is.null(linetype))
  names(reshaped)[names(reshaped)==linetype] <- ifelse(is.character(linetype),
                                                       firstup(linetype),
                                                       linetype)
else
  print(ifelse(is.null(linetype),
               1,
               firstup(linetype)))
switch(type,
       "line" = {graph <- .do_graph_line(table=reshaped,
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

# print("ok2")
## Faceting variable
if(!isFALSE(faceting)){
  if(!is.character(faceting))
    stop("faceting must be a character of length 1")
  if(!length(faceting)== 1){
    stop("Please specify only 1 faceting column.")
  }
  else if(!(faceting %in% names(reshaped))){
    if(firstup(faceting) %in% names(reshaped)){

      graph <- graph+ggplot2::facet_wrap(as.formula(paste("~", firstup(faceting))), scales = "free_x")
    }
    else
      stop("The faceting variable you indicated is apparently not in your dataset")
  }
  else{
    print(faceting)
    graph <- graph+facet_wrap(as.formula(paste("~", faceting)), scales = "free_x")
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

