#' Check that a Column Name is in a Dataset and Replace it by a Temp Name
#'
#' Internal and unexported function of ForestData
#'
#' @param arg A character argument corresponding to a column name, that must be checked
#' @param name A name which replaces the original column name inside the function where the check/replace is called
#' @param data A data.frame
#'
#' @return A data.frame in which the target colname has been replaced by one that will be used internally
#'
#' @examples
#' \dontrun{
#' data <- check_rename_variable_col(status_col, "status",data)
#' }
check_rename_variable_col <- function(arg, name, data){
  if(!inherits(name,"character")){
    stop('Internal error in check_variable_col(): name must be a character')
  }
  if(length(name) != 1){
    stop('Internal error in check_variable_col(): name must be a character of length 1')
  }
  if(is.na(name)){
    stop('Internal error in check_variable_col(): name must be non-NA')
  }
  if(!inherits(data, "data.frame")){
    stop('Internal error in check_variable_col(): data must be a data.frame')
  }

  if(!inherits(arg,"character")){
    stop(paste0(arg, ' must be a non NA, length 1 character corresponding to a column name. For more informations on what this argument corresponds to, see the help section.'))
  }
  if(length(arg) != 1){
    stop((paste0(arg,' must be a character of length 1')))
  }
  if(is.na(arg)){
    stop((paste0(arg,' must be non-NA')))
  }
  else if(!arg %in% names(data)){
    stop(paste0('The specified name for ', name,' (',arg,') does not match with the names of your dataset. For more informations on what this argument corresponds to, see the help section.'))
  }
  else {
    if(grepl("_col",name)){
      names(data)[which(names(data) == arg)] <- strsplit(name, split = "_col")[[1]]
    }
    else names(data)[which(names(data) == arg)] <- name
  }
  return(data)
}


