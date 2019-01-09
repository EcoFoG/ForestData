#' Title
#'
#' @param arg
#' @param argname
#' @param data
#'
#' @return
#'
#' @examples
check_rename_variable_col <- function(arg, argname, data){
  if(!inherits(argname,"character")){
    stop('Internal error in check_variable_col(): argname must be a character')
  }
  if(!inherits(data, "data.frame")){
    stop('Internal error in check_variable_col(): data must be a data.frame')
  }

  if(!is.character(arg)){
    stop(past0(argname, ' must be a character corresponding to a column name. For more informations on what this argument corresponds to, see the help section.'))
  }
  else if(!arg %in% names(data)){
    stop(paste0('The specified name for ',argname,' (',arg,') does not match with the names of your dataset. For more informations on what this argument corresponds to, see the help section.'))
  }
  else {
    names(data)[which(names(data) == arg)] <- strsplit(argname, split = "_col")[[1]]
  }
  return(data)
}


