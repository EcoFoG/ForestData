
# check rename7 -----------------------------------------------------------

# Check that a Column Name is in a Dataset and Replace it by a Temp Name
#
# Internal and unexported function of ForestData
#
# @param arg A character argument corresponding to a column name, that must be checked
# @param name A name which replaces the original column name inside the function where the check/replace is called
# @param data A data.frame
#
# @return A data.frame in which the target colname has been replaced by one that will be used internally
#
# @examples
# \dontrun{
# data <- check_rename_variable_col(status_col, "status",data)
# }
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


# Use geom_ribbon to make a graph -----------------------------------------

.do_graph_ribbon <- function(table,
                           x_variable = "time",
                           y_variable = "mean",
                           upper_quantile,
                           lower_quantile,
                           ...,
                           title = "Default title that can be modified",
                           subtitle = "Default subtitle that has to be user-specified",
                           x.axis.name = "Census year",
                           y.axis.name = "Annual rate",
                           transparence = 0.4,
                           linewidth = 0.72,
                           x.text.angle = 90,
                           y.text.angle = 0){

  # Grab the ellipsis args
  args_dots <- list(...)

  if(length(args_dots[names(args_dots)=="fill"])== 0 & length(args_dots[names(args_dots)=="color"])== 1){
    args_dots["fill"] <- args_dots["color"]
  }
  else if(is.null(args_dots[names(args_dots)=="fill"])){
    args_dots <- args_dots[-"fill"]
  }

  # separate them into two categories: variable names, or not
  variables <- unlist(lapply(names(args_dots),
                      function(n){
                        if(args_dots[[n]] %in% names(table)){
                          return(TRUE)
                        }
                        else
                          return(FALSE)
                      }))
  # print(args_dots[unlist(variables)])

  # Isolate variable names to be passed to ggplot2::aes()
  if(!all(isFALSE(variables))){
    aesthetics_line <- c(list(x = x_variable, y = y_variable),args_dots[variables & !names(args_dots)=="fill"])
    aesthetics_ribbon <- c(list(x = x_variable, ymin = lower_quantile, ymax = upper_quantile), args_dots[unlist(variables) & names(args_dots)=="fill"])
  }else{
    aesthetics_line <- list(x = x_variable, y = y_variable)
    aesthetics_ribbon <- list(x = x_variable, ymin = lower_quantile, ymax = upper_quantile)
  }

  # Conversely, isolate scalar constants to be passed to ggplot2::geom_line()
  if(!all(isTRUE(variables))){
    # scalars_line <- c(list(alpha= transparence, size=linewidth), args_dots[!variables])
    scalars_line <- c(list( size=linewidth), args_dots[!variables])
    scalars_ribbon <- c(list(alpha= transparence), args_dots[!variables & !names(args_dots)%in% c("linetype","size")])
  }else{
    # scalars_line <- list(alpha= transparence, size=linewidth)
    scalars_line <- list(size=linewidth)
    scalars_ribbon <- list(alpha= transparence)
  }
  # print(aesthetics_ribbon)
  # print("line")
  # print(aesthetics_line)

  args_line <- c(list(mapping=do.call(what = ggplot2::aes_string, args = aesthetics_line)),scalars_line)
  args_ribbon <- c(list(mapping = do.call(what = ggplot2::aes_string, args = aesthetics_ribbon)),scalars_ribbon)

  print(args_ribbon)
  # argues <-  c(list(mapping = do.call(aes_string,list(x="time",y="mean", color="Plot"))), list(linetype = 2, size = 3))
  # Make the graph. Variables are passed to aes() and constants to geom_line()
  graph <- ggplot2::ggplot(data = table)+
    do.call(what = ggplot2::geom_ribbon, args = args_ribbon)+
    do.call(what = ggplot2::geom_line, args = args_line)+
    ggplot2::scale_x_continuous(breaks = table[,which(names(table)==x_variable)], labels = table[,which(names(table)==x_variable)])+
    ggplot2::xlab(x.axis.name)+
    ggplot2::ylab(y.axis.name)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=x.text.angle),
                   axis.text.y = ggplot2::element_text(angle=y.text.angle))
  # print(graph)
  return(graph)

}
# Use geom_line to make a graph -------------------------------------------

# reshaped%>%
#   ggplot(aes(x=quo("time"), y = value, linetype = rate, color=  Plot))+
#   geom_line(alpha = 0.6, size = 0.72)+
#   scale_x_continuous(breaks = as.numeric(reshaped$time))
#   theme(axis.text.x = element_text(angle = 90))


# Internally Producing a geom_line Graph with ggplot2
#
# To be used in display_rates, display_mortality, display_recruitment, display_ba and display_all
#
# @param table A long-format table -supposedly a time-series- containing the summary statistics to be displayed, be it by plot or species, etc...
# @param x_variable The variable constituting the abscisse dimension of the graph, to be passed as "x" to aes function
# @param y_variable The variable constituting the ordinate of the graph,  to be passed as "y" to aes function
# @param ... Additional arguments to be passed to the aes function, typically color and linetype. Other ways to display a variable can technically be explored, but I'm afraid it might cause overplotting or ugliness
# @param title Character, internally defaulting to something dummy, but destinated to be user-specified or defaulted in the display functions, then passed to this internal.
# @param subtitle Character, internally defaulting to something dummy, but destinated to be user-specified or defaulted in the display functions, then passed to this internal.
# @param x.axis.name Character, internally defaulting to "Census year"
# @param y.axis.name Character, internally defaulting to "Annual rate"
# @param transparence Scalar numeric, to be passed to geom_line as alpha = transparence
# @param linewidth  Scalar numeric, to be passed to geom_line as size = linewidth
# @param x.text.angle Scalar numeric, to be passed to element_text as angle = x.text.angle, within the axis.text.x argument of ggplot2 theme function
# @param y.text.angle Scalar numeric, to be passed to element_text as angle = y.text.angle, within the axis.text.y argument of ggplot2 theme function
#
# @return A graph made with ggplot2 - thus, fondamentally a list but perhaps a little bit more than it.
#
# @examples
.do_graph_line <- function(mytable,
                           x_variable,
                           y_variable,
                           ...,
                           title = "Default title that can be modified",
                           subtitle = "Default subtitle that has to be user-specified",
                           x.axis.name = "Census year",
                           y.axis.name = "Annual rate",
                           transparence = 0.6,
                           linewidth = 0.72,
                           x.text.angle = 90,
                           y.text.angle = 0){

  # Grab the ellipsis args
  args_dots <- list(...)
  # separate them into two categories: variable names, or not
  # print(args_dots)
  variables <- lapply(names(args_dots),
                      function(n){
                        # print(args_dots[[n]])
                        if(args_dots[[n]] %in% names(mytable)){
                          return(TRUE)
                        }
                        else
                          return(FALSE)
                      })
  # print(args_dots[unlist(variables)])

  # Isolate variable names to be passed to ggplot2::aes()
  if(any(unlist(variables) == T)){
    aesthetics <- c(x = x_variable, y = y_variable,args_dots[unlist(variables)])
    # print(aesthetics)
  }else{
    aesthetics <- list(x = x_variable, y = y_variable)
  }
  # print(mytable)
  # Conversely, isolate scalar constants to be passed to ggplot2::geom_line()
  # print(unlist(variables))
  if(any(unlist(variables) == F)){
    # print("args_dots[!unlist(variables)]")
    scalars <- c(alpha= transparence, size=linewidth, args_dots[!unlist(variables)])
  }else{
    scalars <- list(alpha= transparence, size=linewidth)
  }
  # print(aesthetics)
  # print(scalars)
  # print(names(args_dots))
# Ancien code
  # graph <- ggplot2::ggplot(data = mytable, mapping=aes_string(x = x_variable, y = y_variable, ...))+
  # geom_line(alpha = transparence,size = linewidth)+

  # Make the graph. Variables are passed to aes() and constants to geom_line()
  graph <- ggplot2::ggplot(data = mytable, mapping=do.call(what = ggplot2::aes_string, args = aesthetics))+
    do.call(what = ggplot2::geom_line, args = scalars)+
    ggplot2::scale_x_continuous(breaks = mytable[,which(names(mytable)==x_variable)], labels = mytable[,which(names(mytable)==x_variable)])+
    ggplot2::xlab(x.axis.name)+
    ggplot2::ylab(y.axis.name)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=x.text.angle),
                   axis.text.y = ggplot2::element_text(angle=y.text.angle))
  # print(graph)
  return(graph)

}



# Use geom_histogram to make a graph --------------------------------------
#
# reshaped2 %>%
#     filter(Plot%in%1:16) %>%
#     mutate(treatment = ifelse(Plot%in% c(1,6,11,13,14,15,16),
#                               '0',
#                               ifelse(Plot%in% c(2,7,9),
#                                      '1',
#                                      ifelse(Plot%in% c(3,5,10),
#                                             '2',
#                                             '3')))) %>%
#   ggplot(aes(x = value, fill = rate))+
#   geom_histogram(position = "dodge")+
#   facet_wrap(~treatment)
.do_graph_histogram  <- function(mytable,
                                 x_variable,
                                 position = "dodge",
                                 ...,
                                 title = "Default title that can be modified",
                                 subtitle = "Default subtitle that has to be user-specified",
                                 x.axis.name = "Census year",
                                 y.axis.name = "Annual rate",
                                 transparence = 1,
                                 x.text.angle = 0,
                                 y.text.angle = 0){

  # Grab the ellipsis args
  args_dots <- list(...)
  print(transparence)

  # separate them into two categories: variable names, or not
  variables <- lapply(names(args_dots),
                      function(n){
                        if(args_dots[[n]] %in% names(mytable)){
                          return(TRUE)
                        }
                        else
                          return(FALSE)
                      })
  print(variables)

  # Isolate variable names to be passed to ggplot2::aes()
  if(length(variables) > 0 & !all(isFALSE(unlist(variables)))){
    aesthetics <- c(list(x = x_variable),args_dots[unlist(variables)])
  }else{
    aesthetics <- list(x = x_variable)
  }
print(unlist(variables))
  # Conversely, isolate scalar constants to be passed to ggplot2::geom_line()
  if(length(variables) > 0 & !all(isTRUE(unlist(variables)))){
    scalars <- c(list(alpha = transparence, position=position), args_dots[!unlist(variables)])
  }else{
    scalars <- list(alpha= transparence, position=position)
  }

  # Ancien code
  # graph <- ggplot2::ggplot(data = mytable, mapping=aes_string(x = x_variable, y = y_variable, ...))+
  # geom_line(alpha = transparence,size = linewidth)+

  # Make the graph. Variables are passed to aes() and constants to geom_line()
  graph <- ggplot2::ggplot(data = mytable, mapping=do.call(what = ggplot2::aes_string, args = aesthetics))+
    do.call(what = ggplot2::geom_histogram, args = scalars)+
    ggplot2::scale_x_continuous(breaks = mytable[,which(names(mytable)==x_variable)], labels = mytable[,which(names(mytable)==x_variable)])+
    ggplot2::xlab(x.axis.name)+
    ggplot2::ylab(y.axis.name)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)+
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=x.text.angle),
                   axis.text.y = ggplot2::element_text(angle=y.text.angle))
  # print(graph)
  return(graph)

}

# Use geom_bar to make a graph --------------------------------------------
# Title
#
# @param mytable
# @param x_variable
# @param y_variable
# @param linetype
# @param ...
# @param title
# @param subtitle
# @param x.axis.name
# @param y.axis.name
# @param position
# @param x.text.angle
# @param y.text.angle
#
# @return
#
# @examples
.do_graph_barplot <- function(mytable,
                           x_variable,
                           y_variable,
                           linetype = 1,
                           ...,
                           title = "Default title that can be modified",
                           subtitle = "Default subtitle that has to be user-specified",
                           x.axis.name = "Census year",
                           y.axis.name = "Annual rate",
                           position = "dodge",
                           x.text.angle = 90,
                           y.text.angle = 0){
  ggplot2::ggplot(mytable, aes(x = x_variable, y = y_variable, linetype = linetype, ...))+
    geom_bar(stat = "identity", position=position)+
    xlab(x.axis.name)+
    ylab(x.axis.name)+
    ggtitle(label = title, subtitle = subtitle)+
    theme(axis.text.x = element_text(angle=x.text.angle),
          axis.text.y = element_text(angle=y.text.angle))

}
# reshaped2 <- reshaped
# reshaped2[which(reshaped$rate == "mortality"),"value"] <- -reshaped2[which(reshaped$rate == "mortality"),"value"]
#
# reshaped2 %>%
#   filter(Plot%in%1:16) %>%
#   mutate(treatment = ifelse(Plot%in% c(1,6,11,13,14,15,16),
#                             '0',
#                             ifelse(Plot%in% c(2,7,9),
#                                    '1',
#                                    ifelse(Plot%in% c(3,5,10),
#                                           '2',
#                                           '3')))) %>%
#   ggplot(aes(x = time, y = value, color = rate, fill = Plot)) +
#   geom_bar(stat = "identity", position = "dodge")+
#   facet_wrap(~treatment)

# Merge recruitment and mortality mytables with checks ----------------------


# Internally Merging Recruitment and Mortality Rates with Checks
#
# Simply uses base's merge function to merge the outputs of compute_mortality and compute_recruitment, but checks that these are corresponding beforehand
#
# @param mortality data.frame, output of compute_mortality
# @param recruitment data.frame, output of compute_recruitment
# @param by character, specified the columns that have to correspond - defaults to both census time and plot index, but could be time and taxonomic group if the purpose of the analysis is to do so.
#
# @return a merged mytable of #'
# @examplesmortality and recruitment rates for the categories you chooses -no matter whether plot, subplot, species- and census time... Or any other variable to be used as a graph's x axis.

.merge_rates <- function(mortality,
                         recruitment,
                         by = c("time","plot")){

  # Check args
  if(length(names(mortality)) != length(names(recruitment))){
    stop("The mortality and recruitment mytables must have corresponding columns.")
  }
  # if(nrow(mortality) != nrow(recruitment)){
  #   stop("The mortality and recruitment mytables must have corresponding information (i.e. must be computed from the same inventories).")
  # }
  if(length(names(mortality)) != length(by)+2){
    message <- paste0("The mortality mytable is supposed to have ",length(by)+1, "columns: you specified that by=c(",paste(by, sep = ","),"). If there are other variables in the mytable, you should 1. specify it, 2. ensure that both mytables contain the same information. ")
    stop(message)
  }
  if(length(names(recruitment)) != length(by)+2){
    message <- paste0("The recruitment table is supposed to have ",length(by)+1, "columns: you specified that by=c(",paste(by, sep = ","),"). If there are other variables in the table, you should 1. specify it, 2. ensure that both tables contain the same information. ")
    stop(message)
  }

  # Merge tables

  return(merge(recruitment, mortality))
}


# Save a graph using ggsave -----------------------------------------------


# Save a Graph
#
# Saves a graph with ggsave with additionnal checks and options.
#
# @param graph A graph obtained with ggplot2, to be saved
# @param path_save Character, a path indicating in which FOLDER the graph has to be saved
# @param name Character, the name of the folder containing the graph. It can be followd by the extension corresponding to the device - avoid .jpg for the jpeg device, use .jpeg instead. If the extension is missing, it is automatically added according to the selected device.
# @param device Character, the graphical device to be used to save the graph
# @param create_folder Logical, indicated whether the folders in the given path must be created in case they do not exist yet, or not
# @param overwrite Logical, indicating whether a file already existing under the same name must be overwritten, or kept. In the second case, the function aborts with an explicit error message.
#
# @return Nothing
#
# @examples
.save_graph <- function(graph,
                        path_save,
                        name,
                        device,
                        create_folder,
                        overwrite){
  # .test_install_package("ggplot2",".save_graph")
  path_elements <- strsplit(path_save, split = "/")[[1]]
  if(length(path_elements) > 0){
    for(i in 1:length(path_elements[-length(path_elements)])){
      pathtemp <- file.path(path_elements[1:i])
      if(dir.exists(pathtemp)){
        next
      }
      else{
        if(create_folder){
          dir.create(pathtemp)
          message(paste0("The directory ", pathtemp, " has been created"))
        }
        else{
          stop(paste0("The directory [",pathtemp,"] does not exist. Create it manually or set the arg creat_folder = TRUE"))
        }
      }
    }
  }
  else{
    stop("The path to save the graph is empty")
  }

  if(!grepl(paste0(".",device), name)){
    if(grepl(".",name)){
      stop("The filename you provide contains an extension that is different to the device you specified. Try again and make sure these are identical")
    }
    else{
      name <- paste0(name,".",device)
    }
  }
  if(name %in% list.files(path_save)){
    if(overwrite){
      ggsave(filename=file.path(path_save,name), plot=graph, device=device)
    }
    else{
      stop(paste0("The file ",file.path(path_save, name)," already exists. To overwrite it, run the function again with the argument setting 'overwrite=TRUE' "))
    }
  }
  else{
    ggplot2::ggsave(filename=file.path(path_save,name), plot=graph, device=device)
  }
}



# Check a package installation when needed inside a function --------------


# Checks if a Package is Installed, and Tries to Install it if not
#
# If one wants to use the display_* functions, ggplot2 must be installed. However, it is not part of ForestData dependencies as these functions are optional, and the corrections are designed to run with R's core packages - base, stats -, so this internal cheks if the package is installed, and if not, checks the internet connection and asks the user if they wan ggplot2 to be installed. In the case the user refuses, it terminates with an error message stating that the function in which this internal is called cannot run without the package. This function can be used with other general cases.
#
# @param package The package that is required inside fun - Here, we use it for the display_ functions but this code may be re-used for other purpose.
# @param fun The function inside which the package is required and need to be checked for
#
# @return Basically, nothing... Well, maybe a 0 or a null, but who cares.
#
# @examples
.test_install_package <- function(package,fun){
  # package = "ggplot2"
  # fun = "fun"
  if(!package %in% loadedNamespaces()){
    if(!package %in% installed.packages()){
      if(.canPingSite("www.cran.rstudio.com")){
        mess <- paste0(package," is needed to run the ",fun," function , but is not installed on your computer.")
        message(mess)
        agreement <- "dunno"
        while(!agreement %in% c("y","n")){
          agreement <- readline(prompt="Do you want to install it now? y(yes)/n(no): ")
          # print("ok")

          if(agreement == "y"){
            install.packages(package, dependencies = TRUE)
            if(!package %in% installed.packages()){
              stop(paste0(package, " could not be installed. It could be because of version incompatibility? Try to update R and your packages, then try to install ",package," again."))
            }
            ok <- require(package, character.only = TRUE)

            if(!ok){
              print(ok)
              message(paste0("An error has occurred when trying to load ggplot2. Please re-install it or check what it going bad with it."))
            }
            else return(0)
          }
          else if(agreement == "n"){
            mess <- paste0("Terminating. Sorry, the ",fun," function need to have the ",package," package installed to run.")
            stop(mess)
          }
          else{
            message("You neither typed 'y' (for YES) nor 'n' (for NO). Thus, lets try again:")
          }
        }
      }
      else{
        mess <- paste0(package," is not installed on your computer, and you apparently have no access to the internet. The",fun," function cannot run without this package.")
        stop(mess)
      }
    }
    else{
      message(paste0("Loading package ",package," for the function ",fun," to run."))
      ok <- require(package)
      if(!ok){
       return(0) #dirtyhack
        # stop(paste0("An error has occurred when trying to load ggplot2. Please re-install it or check what it going bad with it."))
      }
      else return(0)
    }
  }
  else return(0)
}


# Test connexion to a site ------------------------------------------------


# Test if a site can be Pinged
#
# Test if a site can be reached, thus tests for both active internet connection on the user's side and availability of the required website.
#
#Found on https://stackoverflow.com/questions/5076593/how-to-determine-if-you-have-an-internet-connection-in-r/5078712 on an answer by Tony Breyal.
#
# @param test.site An url to a site to check connection to.
#
# @return A logical value indicating whether the site have been reached, or not.
#
# @examples
.canPingSite <- function(test.site) {
  !as.logical(system(paste("ping", test.site),show.output.on.console=F))
}


# Reshape a mortality-recruitment table -----------------------------------


# Reshape a Rates Table to Long Format
#
# Makes a long format table to use with ggplot2 -1 col for rates, 1 for value- from a wide rates table -1 col for mortality, 1 for recruitment-
#
# @param merged Data.frame outputed by merge_rates, to be switched to long-format
#
# @return A reshaped data.frame, in which mortality and recruitment are put into long format, ready to be used by ggplot2
#
# @examples
reshape_rates <- function(merged){
  #Set the names as reshape "wants" it.
  # print(names(merged))
  merged <- merged[,-which(names(merged) == "interval")]
  names(merged)[which(names(merged) %in% c("time","plot","annual_recruitment_rate","annual_deathrate"))] <- c("time","plot","rate.1","rate.2")
  #Use base's reshape function
  reshaped <- stats::reshape(merged,        #dataframe
                            direction="long",       #wide to long
                            varying=c(3,4),
                            times = c("recruitment","mortality"),
                            timevar = "rate",
                            v.names = "value")
  #Take away the last column, that is useless

  reshaped<-reshaped[,-length(names(reshaped))]
  #Set the rownames back to 1:nrow(reshaped)
  row.names(reshaped) <- NULL
  #Get rid of the factors. I don't like factors.
  # factors <- unlist(lapply(1:length(names(reshaped)),
  #                          function(n){
  #                            if(is.factor(reshaped[,names(reshaped[n])]))
  #                              return(TRUE)
  #                            else
  #                              return(FALSE)
  #                          }))

  # print(factors)

  # for(i in 1:length(names(reshaped))){
  #   if(is.factor(reshaped[,i])){
  #     reshaped[,i] <- as.character(reshaped[,i])
  #   }
  # }

  # reshaped[,factors] <- as.character(reshaped[,factors])
# print("resh")
# print(reshaped)
  #Create a new time column rather than time interval... we'll implement the use of intervals later on, but for the moment it is problematic since between-census intervals are not homogenous.
  # reshaped$time <- as.numeric(unlist(strsplit(reshaped$interval, split = "_"))[seq.default(from = 2, to = 2*nrow(reshaped), by = 2)])
  return(reshaped)
}


# Put the first letter of a string to upper case --------------------------

# found on https://stackoverflow.com/questions/18509527/first-letter-to-upper-case, answer by alko989
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
