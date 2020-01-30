#' Prepare global options to run ForestData more easily
#'
#' This function counterbalances the high number of arguments in ForestData:
#' many of these simply correspond to the input dataset column names, and have
#' to be specified over and over when running each function.
#' \code{prepare_forestdata} allows the user to specify these column names only
#' once and then run all the functions without having to do it again
#'
#' @param data data.frame, the forest census dataset that you want to treat with
#'   the ForestData r-package
#' @param plot_col character, the name of the column containing plot indices
#' @param id_col character, the name of the column containing tree unique ids
#' @param time_col character, the name of the column containing census years or
#'   times
#' @param status_col character, the name of the column containing tree vital
#'   statuses
#' @param size_col character, the name of the column containing tree size
#'   measurements
#' @param measure_type character indicating whether measures are circumferences
#'   ("C") or diameter ("D")
#' @param POM_col character, the name of the column containing Point of
#'   Measurement (POM) values
#'
#' @return NULL, because this function just sets global options to fluidify
#'   ForestData's usage.
#' @export
#'
#' @examples
#' # Loading example dataset
#' data(example_census)
#' # specifying the example dataset's column names
#' prepare_forestdata(example_census,
#' plot_col="Plot",
#' id_col="idTree",
#' time_col="CensusYear",
#'  status_col = "CodeAlive",
#'  size_col="Circ",
#'  measure_type = "C",
#'  POM_col = "POM")
#' # checking that the options have been set
#' getOption("plot_col"); getOption("time_col")
#' # If the function is run twice with similar specification for one
#' #or several options, a message indicates that these specific option.s
#' #kept unchanged
#' prepare_forestdata(example_census,
#' plot_col="Plot",
#' id_col="idTree",
#' time_col="CensusYear",
#' status_col = "CodeAlive",
#' size_col="Circ",
#' measure_type = "C",
#' POM_col = "POM")
#' # If one column name is erroneous, then the function stops with explicit error message
#' \dontrun{
#' prepare_forestdata(example_census,
#' plot_col="SAUCISSON",
#' id_col="idTree",
#' time_col="CensusYear",
#' status_col = "CodeAlive",
#' size_col="Circ",
#' measure_type = "C",
#' POM_col = "POM")
#' ## "Error in prepare_forestdata(example_census,
#' ## plot_col = "SAUCISSON", id_col = "idTree",  :
#' ## plot_col is not any of your dataset's column name..."
#'   }

prepare_forestdata <- function(data,
                               plot_col = getOption("plot_col"),
                               id_col = getOption("id_col"),
                               time_col = getOption("time_col"),
                               status_col = getOption("status_col"),
                               size_col = getOption("size_col"),
                               measure_type = getOption("measure_type"),
                               POM_col = getOption("POM_col")
                               # default_POM = getOption("default_POM")
                               ){


# Plot --------------------------------------------------------------------


  # if(!is.null(getOption("plot_col")) & !is.null(plot_col) & isTRUE(plot_col != getOption("plot_col"))){

    if(!is.null(plot_col) &
       (is.null(getOption("plot_col"))|
        (!is.null(getOption("plot_col")) &
                  isTRUE(plot_col != getOption("plot_col"))
                  )
         )
        ){
      if(!plot_col %in% names(data)){
        stop("plot_col is not any of your dataset's column name...")
      }
      else options("plot_col" = get('plot_col'))
    }
    else print("plot_col let to its default or previous value")


# Id ----------------------------------------------------------------------

  if(!is.null(id_col) &
     (is.null(getOption("id_col"))|
      (!is.null(getOption("id_col")) &
       isTRUE(id_col != getOption("id_col"))
      )
     )
  ){
    if(!id_col %in% names(data)){
      stop("id_col is not any of your dataset's column name...")
    }
    else options("id_col" = get('id_col'))
  }
  else print("id_col let to its default or previous value")




# Time --------------------------------------------------------------------


  if(!is.null(time_col) &
     (is.null(getOption("time_col"))|
      (!is.null(getOption("time_col")) &
       isTRUE(time_col != getOption("time_col"))
      )
     )
  ){
    if(!time_col %in% names(data)){
      stop("time_col is not any of your dataset's column name...")
    }
    else options("time_col" = get('time_col'))
  }
  else print("time_col let to its default or previous value")

# Status ------------------------------------------------------------------


  if(!is.null(status_col) &
     (is.null(getOption("status_col"))|
      (!is.null(getOption("status_col")) &
       isTRUE(status_col != getOption("status_col"))
      )
     )
  ){
    if(!status_col %in% names(data)){
      stop("status_col is not any of your dataset's column name...")
    }
    else options("status_col" = get('status_col'))
  }
  else print("status_col let to its default or previous value")

# Measure -----------------------------------------------------------------


  if(!is.null(size_col) &
     (is.null(getOption("size_col"))|
      (!is.null(getOption("size_col")) &
       isTRUE(size_col != getOption("size_col"))
      )
     )
  ){
    if(!size_col %in% names(data)){
      stop("size_col is not any of your dataset's column name...")
    }
    else options("size_col" = get('size_col'))
  }
  else print("size_col let to its default or previous value")



  if(!is.null(measure_type) & !identical(measure_type, getOption("measure_type"))){
    if(!measure_type %in% c("circumference",
                            "circ",
                            "c",
                            "Circumference",
                            "Circ",
                            "C",
                            "diameter",
                            "diam",
                            "d",
                            "Diameter",
                            "Diam",
                            "D")){
      stop("argument measure_type: Please indicate if the tree size measurements (argument measure_type) are circumferences (C) or diameters (D)")
    }
    else options("measure_type" = measure_type)
  }
  else print("measure_type let to its default or previous value")

# POM ---------------------------------------------------------------------

  if(!is.null(POM_col) &
     (is.null(getOption("POM_col"))|
      (!is.null(getOption("POM_col")) &
       isTRUE(POM_col != getOption("POM_col"))
      )
     )
  ){
    if(!POM_col %in% names(data)){
      stop("POM_col is not any of your dataset's column name...")
    }
    else options("POM_col" = get('POM_col'))
  }
  else print("POM_col let to its default or previous value")
  # if(!is.null(getOption("default_POM")) & !is.null(default_POM) & isTRUE(default_POM != getOption("default_POM"))){
  #   if(default_POM < 0){
  #     stop("argument default_POMcan't be negative")
  #   }
  #   else if(default_POM > 2 | default_POM < 1) stop("Your default_POM seems odd. Please check it.")
  #   else options("default_POM" = default_POM)
  # }
  # else print("default_POM let to its default or previous value")
  # return(NULL)
}


