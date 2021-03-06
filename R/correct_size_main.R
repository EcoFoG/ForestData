#' Correct Tree Size Measurements
#'
#' This function provides corrections for tree size measurements in Forest
#' Inventories. POM are to be given as an input, and POM shifts are accounted
#' for by adjusting the after-shifts value translating the points to offest the
#' observed size loss by: after_shift_size_corrected = after_shift_size_original
#' + difference_before_after + expected_growth.
#'
#' @inheritParams correct_all
#' @param ignore_POM Logical, defaults to FALSE. If TRUE, POM shifts are not
#'   accounted for to explicitely correct the measurements altered by these
#'   events. To use with care and ONLY if POMs were not reported during the
#'   cnsuses. Important is to note that using POM data (even when confidence on
#'   it is rather low) is often better than selecting this option. The user is
#'   responsible for the possible anomalies generated by correcting without POM
#'   data and has the responsibility to ensure that the corrections are overall
#'   good by checking the growth trajectories for at least a suficient subset of
#'   trees, using display_corrected_trees().
#'
#' @details
#'
#' Signification of the code_corr values (short):
#' - p_incr: punctual increase corrected by inter- or extrapolation
#' - p_decr: punctual decrease corrected by inter- or extrapolation
#' - def_incr_rp: definitive increase (or positive shift) corrected by realigning the most
#' recent series upon the previous one.
#' - def_decr_rp: definitive decrease (or negative shift) corrected by realigning the most
#' recent series upon the previous one.
#' - def_incr_rl: definitive increase (or positive shift) corrected by realigning the previous
#' series on the most recent
#' - def_decr_rl: definitive decrease (or negative shift) corrected by realigning the previous
#' series on the most recent
#'
#' This is an adaptation from Camille Piponiot's original correction function.
#' This version is primarily designed to explicitely account for POM shifts. If
#' the POM were registered reliably, using it enables 1- to eliminate anomalies
#' due to POM shifts (typically, abnormal decreases in size without return to
#' "normal values") and 2- to detect cases that would not be detected with the
#' applied thresholds.
#'
#' The optional argument "ignore_POM" triggers the version of the algorithm
#' designed for cases in which POM shifts have not been registered. This should
#' not be used if the information is available.
#'
#' This function automates a quite simple yet efficient detection method based
#' on using meaningful thresholds to detect errors, and using linear inter- or
#' extrapolation from neighboring points to correct anomalous values or missing
#' measurements.
#'
#'
#' For example, the Paracou Disturbance Experiment database is corrected with
#' the following rules:
#'
#' - An annual diameter growth that exceeds 5cm / year between two censuses is
#' considered abnormal.
#'
#' - This applies to avery species except a handful of well-known pioneer trees
#' that sometimes exhibit explosive growth rushes (e.g. Cecropia obtusa) in the
#' first years following recruitment, and especially for disturbed forest plots.
#'
#' - For these species, annual growths up to 8cm can be tolerated.
#'
#' - An absolute diameter decrease of 2cm or more between two censuses is also
#' considered abnormal.
#'
#' - Anomalies can be separated into two categories: punctual error and
#' permanent shift. The nuance relies on the existance of an event marking a
#' "return to normal size trajectory".
#'
#' - A punctual error is an abnormal increase or decrease in size, that is
#' offset by a complementary decrease or increase in size. In certain cases,
#' this offsetting decrease of increase may not come immediately after the first
#' outlyer, for some reason (this has been observed in the base but not yet
#' explained). To separate efficiently shifts from punctual errors, the
#' algorithm search for the complementary diameter variation up to 2 censuses
#' after the first anomaly. The values of code_corr corresponding to punctual
#' increase or decrease are respectively p_incr and p_decr
#'
#' - A shift is defined as an abnormal increase or decrease in diameter that is
#' not compensated by neighboring measurements.
#'
#' - In most cases, shifts are negative and ared due to changes in Point Of
#' Measurement (POM). In Paracou, for hihly non-cylindric tree stems that cause
#' difficulties to accurately measure diameter or circumference, the size is
#' estimated and the information about this is reported in a "measurement code".
#' Estimations can cause positive as well as negative shifts in the Paracou
#' database.
#'
#' - The changes in POM for the Paracou can be retrieved at least partially from
#' the "measurement code" field. This seems to be the most accurate way to
#' correct the dataset, even in the presence of unreported or falsely reported
#' POM changes, because it eliminates a great part of detectable and
#' undetectable negative shifts.
#'
#' - Tree growth trajectories depend more on individual effect than species
#' identity. Moreover, due to the abundances distribution in this super-diverse
#' rainforest, species-specific growth patterns cannot be established in order
#' to be used for the corrections. Thus, the available growth values, for the
#' individuals to correct, are used in order to estimate growth or size
#' expectations.
#'
#' - Growth is auto-correlated on the temporal dimension in many cases, thus not
#' all the measurement available for such individuals should be used to estimate
#' the corrected values. The 4 nearest values, if available and not abnormal,
#' are supposedly sufficient to estimate a "local mean growth rate".
#'
#' Once detected, the correction is done according to the category of the
#' anomaly:
#'
#' - for punctual increases or decreases, new values are computed according to
#' the 4 nearest non-NA and non-outlyer measurements (2 before the anomaly, 2
#' after),if existing. If values are available before and after the anomaly, the
#' corrected circumference.s or diameter.s are linearly interpolated with a
#' "local mean growth rate" computed from these values. If not, corrected values
#' are extrapolated.
#'
#' - For shifts, one of the two "series" of measurement have to be re-aligned
#' upon the other. The criteria used to choose which series should be taken as a
#' reference depend on whether the POM are explicitely accounted for in the
#' correction, or not. If POM are not available, most of the shifts to correct
#' are supposed to be due to POM changes, leading to the choice to
#' systematically re-align the more recent series upon the older one in case of
#' negative shift. If POM is available, and is reliable enough to allow
#' eliminating most POM shifts, remaining negative shift have roughly the same
#' chances to correspond to e.g. erroneous estimation on non-cylindric stems. In
#' this case, the series having the highest number of values is supposed to be
#' more reliable than the other, and if both series are of same length, the most
#' recent is picked.
#'
#' - The codes corresponding to shifts are composed of a first part that describes
#' the shift (def_incr or def_decr for definitive increase or decrease respectively)
#' and a suffix that describes the realignment (_rp or _rl for realigned with previous
#' or last series, respectively)
#'
#'
#' @return The same data.frame with two additional columns: size_corr,
#'   containing corrected tree size measurements, and code_corr, containing
#'   codes that tag both corrections locations and type.
#' @export
#'
#' @examples
#' #Load the provided example dataset
#' data("example_census")
#'
#' #Take a look to its structure
#' str(example_status_corr)
#'
#'
#'
#' #Correct it (short version with column names set with prepare_forestdata)
#'
#' prepare_forestdata(example_census,
#' plot_col="Plot",
#' id_col="idTree",
#' time_col="CensusYear",
#' status_col = "CodeAlive",
#' size_col="Circ",
#' measure_type = "C",
#' POM_col = "POM")
#'
#' example_size_corr <- suppressWarnings(correct_size(example_status_corr,
#' species_col = "binomial_name",#tag pioneer
#' pioneers = c("Cecropia","Pourouma"),
#' pioneers_treshold = 7.5,
#' ignore_POM = FALSE))
#'
#' str(example_size_corr)
#'
#' #Correct it (full call)
#' example_size_corr <- suppressWarnings(correct_size(example_status_corr,
#' size_col = "Circ",
#' time_col = "CensusYear",
#' status_col = "status_corr",
#' species_col = "binomial_name",
#' id_col = "idTree",
#' POM_col = "POM",
#' measure_type ="C",
#' positive_growth_threshold = 5,
#' negative_growth_threshold = -2,
#' default_POM = 1.3,
#' pioneers = c("Cecropia","Pourouma"),
#' pioneers_treshold = 7.5,
#' ignore_POM = FALSE))
#'
#'
correct_size <- function(data,
                         size_col = getOption("size_col"),
                         time_col = getOption("time_col"),
                         status_col = "status_corr",
                         species_col = "binomial_name",#tag pioneer
                         id_col = getOption("id_col"),
                         POM_col = getOption("POM_col"),
                         measure_type =getOption("measure_type"),
                         positive_growth_threshold = 5,
                         negative_growth_threshold = -2,
                         default_POM = 1.3,
                         pioneers = c("Cecropia","Pourouma"),#tag pioneer
                         pioneers_treshold = 7.5,
                         ignore_POM = FALSE){ #tag pioneer

  # Checks and format -------------------------------------------------------

  opts_args <- c(id_col, size_col, species_col, POM_col, time_col, status_col)
  for(n in 1:length(opts_args))
    if(is.null(opts_args[n]))
      stop(paste0("The following argument need to be specified:",c("id_col", "size_col", "species_col", "POM_col", "time_col", "status_col")[n]))

  data <- check_rename_variable_col(size_col, "size",data)
  data <- check_rename_variable_col(status_col, "status",data)
  data <- check_rename_variable_col(time_col, "time",data)
  data <- check_rename_variable_col(id_col, "id",data)
  data <- check_rename_variable_col(species_col, "species",data) #tag pioneer

  if(!isTRUE(ignore_POM)) data <- check_rename_variable_col(POM_col, "POM",data)
  else{
    warning("You decided to correct tree size measurements without explicitely accounting for POM changes.")
    message("Please be aware that the corrections will therefore be sub-optimal, less reliable than what this package originally intends to provide.")
  }

# Create size and status corr ---------------------------------------------

  data$code_corr <- as.character(rep("0",nrow(data)))
  data$size_corr <- data$size

  if(measure_type == "C"){
    positive_growth_threshold <- positive_growth_threshold*pi
    negative_growth_threshold <- negative_growth_threshold*pi
    pioneers_treshold <- pioneers_treshold*pi #tag pioneer
  }
# Call internals ----------------------------------------------------------

  # Sort data
  data <- data[order(data$id,data$time),]

  #tag pioneer
  pioneer_sp <- data.frame(sp =  unique(data$species), pioneer = FALSE)
  for(i in pioneers){
    pioneer_sp$pioneer <- ifelse(pioneer_sp$pioneer,
                          TRUE,
                          grepl(i, pioneer_sp$sp))
  }
  pioneer_sp <- pioneer_sp$sp[which(pioneer_sp$pioneer)]#tag pioneer

  # Extract ids and loop on indivs

  ids <- unique(data$id)

  pb <- utils::txtProgressBar(min = 0, max = length(ids), style = 3)
  res <- do.call(rbind,lapply(ids,
                              function(i){
                                utils::setTxtProgressBar(pb, which(ids == i))
                                tree <- data[which(data$id == i),]
                                #tag pioneer
                                if(unique(tree$species %in% pioneer_sp)){
                                  thresh <- pioneers_treshold#tag pioneer
                                }
                                else{
                                  thresh <- positive_growth_threshold#tag pioneer
                                }
                                if(isTRUE(ignore_POM)){
                                  POMt <- NULL
                                }
                                else{
                                  POMt <- tree$POM
                                }
# print(tree)

                                return(.correct_size_tree(size = tree$size,
                                                          size_corr = tree$size_corr,
                                                          code_corr = tree$code_corr,
                                                          time = tree$time,
                                                          status = tree$status,
                                                          ignore_POM = ignore_POM,
                                                          POM = POMt,
                                                          default_POM = default_POM,
                                                          positive_growth_threshold = thresh, #tag pioneer
                                                          negative_growth_threshold = negative_growth_threshold,
                                                          ids = ids,
                                                          i = i))

                              }))
  close(pb)
  data[,c("size_corr","code_corr")] <- res[,c("size_corr","code_corr")]

  names(data)[which(names(data) =="size")] <- size_col
  names(data)[which(names(data) == "status")] <- status_col
  names(data)[which(names(data) == "time")] <- time_col
  names(data)[which(names(data) == "id")] <- id_col
  names(data)[which(names(data) == "species")] <- species_col

  return(data)
}

# Internals ---------------------------------------------------------------

.correct_size_tree <- function(size,
                               size_corr,
                               code_corr,
                               time,
                               status,
                               ignore_POM,
                               POM,
                               default_POM,
                               positive_growth_threshold,
                               negative_growth_threshold,
                               ids,
                               i) {

  # print(paste0("Correcting tree ",i,": ",which(ids == i),"/",length(ids)))
  # cresc_abs: absolute
  cresc_abs <- rep(0, length(size) - 1)
  cresc <- rep(0, length(size) - 1)


  if (sum(!is.na(size)) > 1) {
    cresc[which(!is.na(size))[-1] - 1] <-
      diff(size[!is.na(size)]) / diff(time[!is.na(size)])
    cresc_abs[which(!is.na(size))[-1] - 1] <- diff(size[!is.na(size)])
    if (length(cresc) > 0) {

      if(!ignore_POM){
        # print("here")
        res <- .correct_POM_changes(size,
                                    size_corr,
                                    code_corr,
                                    cresc,
                                    time,
                                    POM,
                                    default_POM,
                                    positive_growth_threshold,
                                    negative_growth_threshold,
                                    i)
        # print("here2")
        size_corr <- res$size_corr
        code_corr <- as.character(res$code_corr)
      }


      if (sum(!is.na(size_corr)) > 1) {
        cresc[which(!is.na(size_corr))[-1] - 1] <-
          diff(size_corr[!is.na(size_corr)]) / diff(time[!is.na(size_corr)])
        cresc_abs[which(!is.na(size_corr))[-1] - 1] <- diff(size_corr[!is.na(size_corr)])
      }
      else message(paste("Tree ",i," has no data to correct, all size measurements are NA"))


      res <- .correct_abnormal_growth_tree(size_corr,
                                           code_corr,
                                           cresc,
                                           cresc_abs,
                                           time,
                                           positive_growth_threshold,
                                           negative_growth_threshold,
                                           i,
                                           ignore_POM)
      # TAG CRESC ?

      # size_corr <- res$size_corr
      # code_corr <- res$code_corr



      ## replace missing values
      if (any(!is.na(res$size_corr)) & any(is.na(res$size_corr))) {
        res$size_corr <- .replace_missing(res$size_corr, time, status)
      }
    }
  }
  else{
    res <- data.frame("size_corr" = size_corr, "code_corr" = as.character(code_corr))
    if(length(size == 1)){
      warning(paste0("Tree",i," has only one measurement and has no growth to correct"))
    }
    else warning(paste0("Tree",i," has no data to correct, all size measurements are NA"))
  }




  return(res)
}



.correct_abnormal_growth_tree <- function(size_corr,
                                          code_corr,
                                          cresc,
                                          cresc_abs,
                                          time,
                                          positive_growth_threshold,
                                          negative_growth_threshold,
                                          j,
                                          ignore_POM){

  ####    if there is a DBH change > 5cm/year or < negative_growth_threshold cm   ####
  ### do as many corrections as there are abnormal DBH change values ###
  # cresc_abn = sum(abs(cresc) >= positive_growth_threshold | cresc_abs < negative_growth_threshold)

  n_cresc_abn = sum(cresc >= positive_growth_threshold | cresc_abs < negative_growth_threshold)
  if(is.na(n_cresc_abn)){
    message("It seems like the following tree is generating an exception during growth rate computation :")
    print("tree id")
    print(j)
    print("census years")
    print(time)
    print("size corr before correcting for abnormal diameter variations")
    print(size_corr)
    print("calculated annual growth rate...")
    print(cresc)
    print("calculated absolute growth")
    print(cresc_abs)
    message("this is probably due to multiple measurements for a same census year (or several), that generate a division by zero when annual growth rate is calculated: the formula is (size_n+1 - size_n) / (time_n+1 - time_n)")
    message("This error comes either from infra-annual censusing timestep (not yet handled by these methods) or multiple-stemmed trees for which stems are not discriminated using different IDs; please check it out beforehand")
  }
  if (n_cresc_abn > 0) {
    #last adding, to fix a recently observed behavior: Some negative growth were not corrected
    #mainly because of the "which.max(abs(cresc))". We detect with more strictness negative growths
    #based on ABSOLUTE GROWTH < -2. In terms of annual growth, it yields very low absolute values
    #that can be overed by whatever non-abnormal positive growth, causing "ab" to take another index
    #not corresponding to an anomaly. In this case, no correction is applied because of the IF test
    #just after having defined ab (it verifies that the value is indeed abnormal).
    #I fixed this by referencing anomalies in a data.frame containing position and associated value of cresc.
    #Then, within the for loop, the which.max(abs()) only browses abnormal values, giving a more relevant and failproof
    #direction to the function.
    #I don't know yet what unexpected behaviors are yet to solve, and which ones will be caused by this modif.


    for (i in 1:n_cresc_abn) {
      abnormals <- data.frame(position = which(cresc >= positive_growth_threshold |
                                                 cresc_abs < negative_growth_threshold),
                              cresc=cresc[which(cresc >= positive_growth_threshold |
                                                  cresc_abs < negative_growth_threshold)])
      # print("abnormals")
      # print(abnormals)
      # begin with the census with the highest DBH change

      # ab <- which.max(abs(cresc))
      ab <- abnormals[which.max(abs(abnormals$cresc)), "position"]
      # print("ab")
      # print(ab)
      # check if this census is truly abnormal
      # if (abs(cresc[ab]) >= positive_growth_threshold | cresc_abs[ab] < negative_growth_threshold) {
      # print(i)
      # print(n_cresc_abn)
      # print(cresc)
      # print(abnormals)
      if(length(ab)!= 1){
        if(length(ab)== 0){
          # print("solved!")
          next
        }
        else stop("erreur: length(ab) > 1")
      }
      # print(length(ab))
        if (cresc[ab] >= positive_growth_threshold |
            cresc_abs[ab] <= negative_growth_threshold) {
        # values surrounding ab
        surround = c(ab - 2, ab - 1, ab + 1, ab + 2)
        # that have a meaning (no NAs or 0 values)
        surround = surround[surround > 0 &
                              surround <= length(cresc)]

        # mean DBH change around ab
        meancresc = max(mean(cresc[surround], na.rm = TRUE), 0)

        # moment of max and min DBH changes around ab (including ab, that should be one of the 2)
        sourround_ab = sort(c(surround, ab))
        up = sourround_ab[which.max(cresc[sourround_ab])]
        down = sourround_ab[which.min(cresc[sourround_ab])]

        if (length(surround) > 0) {
          first <- min(up, down) + 1
          last <- max(up, down)
          # print("down")
          # print(down)
          # print("up")
          # print(up)
          # print("size_corr[up]")
          # print(size_corr[up]/pi)
          # print("size_corr[up+1]")
          # print(size_corr[up+1]/pi)
          # print("size_corr[down]")
          # print(size_corr[down]/pi)
          # print("size_corr[down+1]")
          # print(size_corr[down+1]/pi)
          # print("time[down+1]")
          # print(time[down+1])
          # print("time[up]")
          # print(time[up])

          # 1st case : excessive increase/decrease offset by a similar decrease in dbh, plus 5cm/yr
          # is there a value that could compensate the excessive DBH change?
          # check if removing those values would solve the problem (ie cresc < 5 & cresc_abs > -2 )
          if (isTRUE(down > up & cresc[up] * cresc[down] < 0 &
                     # first an increase and then a decrease in DBH
                     (size_corr[down + 1] - size_corr[up]) / (time[down + 1] - time[up])  < positive_growth_threshold &
                     size_corr[down + 1] - size_corr[up] > negative_growth_threshold)){
            is.na(size_corr) <- first:last
            # code_corsav <- code_corr
            # print(is.factor(code_corsav))
            for(c in first:last){
              codetemp <- as.character(ifelse(code_corr[c] == "0",
                                              "p_incr",
                                              paste(code_corr[c], "p_incr",sep = "+")))
              # if(any(is.na(code_corr))) {print(c("O0ZDF", code_corr));print(is.factor(code_corr));print(is.factor(code_corsav))}

              code_corr[c] <- codetemp
            }

          }
          #Add an "else" here because the conditions are supposed to be mutually exclusive for many aspects.
          #e.g. I do not see how up>down & down<up would be true. Moreover, I suppose some unexpected behaviors to pop from the double test that I used to do here.

          #see for example: the first condition passes and the size_corr corresponding to "ab" is set to NA
          # because of ponctual increase then decrease outlyer being spotted. If the test is rerun then,
          # NAs are tested and might cause weird stuff to happen.
          else if (isTRUE(up > down & cresc[up] * cresc[down] < 0 &
                     # first an decrease and then a increase in DBH
                     (size_corr[up + 1] - size_corr[down]) / (time[up + 1] - time[down])  < positive_growth_threshold &
                     size_corr[up + 1] - size_corr[down] > (2*negative_growth_threshold/3))) { #tag dirtyhack
            ##IMPORTANT: The "return to normal" is highly sensitive. On the line above, if just negative_growth_threshold is the reference to which compare
            #the difference of sizes, then it causes linear junction of the series instead of readjustment of the second series. THIS IS SERIOUS STUFF
            # correction: abnormal values are deleted and will be replaced later on (see missing)
            is.na(size_corr) <- first:last
            for(c in first:last){
              codetemp <- as.character(ifelse(code_corr[c] == "0",
                                 "p_decr",
                                 paste(code_corr[c], "p_decr",sep = "+")))
              code_corr[c] <- codetemp
            }
          }
          else {
            # print("up > down & cresc[up] * cresc[down] < 0")
            # print(up > down & cresc[up] * cresc[down] < 0)
            # print(c(up,down,cresc[up],cresc[down]))
            # print("(size_corr[up + 1] - size_corr[down]) / (time[up + 1] - time[down])  < positive_growth_threshold")
            # print((size_corr[up + 1] - size_corr[down]) / (time[up + 1] - time[down])  < positive_growth_threshold)
            # print(c(size_corr[up+1], size_corr[down],time[up+1], time[down]))
            # print("size_corr[up + 1] - size_corr[down] >negative_growth_threshold")
            # print(size_corr[up + 1] - size_corr[down] > negative_growth_threshold)

          # 2nd case: abnormal DBH change with no return to initial values
          # we trust the set of measurements with more values
          # if they are the same size, then we trust the last one
          # ladders?

          #NOTA: I think a scope argument could be added to prevent the cases where several estimated measures
          #form an outlyer series of more than 2 points, and could cause realignement of the other serie(s) with it because
          #of undetection of the "return". But this could also cause bad behaviors. Looks like manual correction of these tricky cases is more appropriate.

          # CHECK SIZE OR SIZE CORR?
          #answer: size_corr, because it integrates the POM changes

            # delta <- (size_corr[ab + 1] - size_corr[ab])
            delta <- cresc_abs[ab]
            # print(c(delta, (size_corr[ab + 1] - size_corr[ab])))
            # print(c(delta, size_corr[ab+1], size_corr[ab]))
            # print(c(ab, size_corr))
            # print(j)
            # print(cresc)
            if(!ignore_POM){
              if ((sum(!is.na(size_corr[1:ab])) > sum(!is.na(size_corr))/2)) { # | isTRUE(ladder[ab] == 0 & ladder[ab+1] == 1)
                # size_corr[(ab + 1):length(size_corr)] <-
                #   size_corr[(ab + 1):length(size_corr)] - cresc_abs[which.max(abs(cresc))] + meancresc *
                #   diff(time)[ab]

                #correction
                size_corr[(ab + 1):length(size_corr)] <-
                  size_corr[(ab + 1):length(size_corr)] - delta + (meancresc * diff(time)[ab])


                #tag correction with code_corr
                if(delta > 0){
                  for (c in (ab+1):length(size_corr)) {
                    codetemp <- as.character(ifelse(
                      code_corr[c] == "0",
                      "def_incr_rp",
                      paste(code_corr[c], "def_incr_rp", sep = "+")
                    ))
                    code_corr[c] <- codetemp
                  }
                }
                else if(delta < 0){
                  for (c in (ab+1):length(size_corr)) {
                  codetemp <- as.character(ifelse(
                    code_corr[c] == "0",
                    "def_decr_rp",
                    paste(code_corr[c], "def_decr_rp", sep = "+")
                  ))
                  code_corr[c] <- codetemp
                }

              }
              }
              else {

                #correction
                size_corr[1:ab] <-
                  size_corr[1:ab] + delta - (meancresc * diff(time)[ab])

                #tag correction with code_corr
                for (c in 1:ab) {
                  if(delta > 0){
                    codetemp <- as.character(ifelse(
                      code_corr[c] == "0",
                      "def_incr_R_recent",
                      paste(code_corr[c], "def_incr_rl", sep = "+")
                    ))
                  }
                  else if(delta < 0){
                    codetemp <- as.character(ifelse(
                      code_corr[c] == "0",
                      "def_decr_R_recent",
                      paste(code_corr[c], "def_decr_rl", sep = "+")
                    ))
                  }
                  code_corr[c] <- codetemp
                }
                # for(c in 1:ab){
                #   codetemp <- as.character(ifelse(code_corr[c] == "0",
                #                                   "def",
                #                                   paste(code_corr[c], "def",sep = "+")))
                # print(codetemp)
                # if(is.na(codetemp)){
                #   print(c("codecor",code_corr,"c",c,"ab",ab,"length",length(size_corr),length(code_corr)))
                # }
                # print(class(codetemp))
                # if(isFALSE(class(codetemp))) print(c(codetemp,c))
                # code_corr[c] <- codetemp
              }
            }
            else if(isTRUE(ignore_POM)){
              if(delta < 0){

                # Correction: the latter series is realigned on the older one because we suppose the anomaly is due to unreported POM shift
                size_corr[(ab + 1):length(size_corr)] <-
                  size_corr[(ab + 1):length(size_corr)] - delta + (meancresc * diff(time)[ab])

                for (c in (ab+1):length(size_corr)){
                  codetemp <- as.character(ifelse(
                    code_corr[c] == "0",
                    "def_decr_rp",
                    paste(code_corr[c], "def_decr_rp", sep = "+")
                  ))
                }
              }
              else if(delta > 0){

                # Correction: The shortest series is realigned on the largest one or the more recent if of equal sizes
                if ((sum(!is.na(size_corr[1:ab])) > sum(!is.na(size_corr))/2)){
                  size_corr[(ab + 1):length(size_corr)] <-
                    size_corr[(ab + 1):length(size_corr)] - delta + (meancresc * diff(time)[ab])

                  for (c in (ab+1):length(size_corr)){
                    codetemp <- as.character(ifelse(
                      code_corr[c] == "0",
                      "def_incr_rp",
                      paste(code_corr[c], "def_incr_rp", sep = "+")
                    ))
                  }

                }
                else{
                  size_corr[1:ab] <-
                    size_corr[1:ab] + delta - (meancresc * diff(time)[ab])

                  for (c in (ab+1):length(size_corr)){
                    codetemp <- as.character(ifelse(
                      code_corr[c] == "0",
                      "def_incr_rl",
                      paste(code_corr[c], "def_incr_rl", sep = "+")
                    ))
                  }
                }

              }
            }


            }
          }
        }

        # cresc_abs: absolute annual diameter increment
        # print("reinitializing cresc")
        cresc <- rep(0, length(size_corr) - 1)
        cresc_abs <- rep(0, length(size_corr) - 1)
        if (sum(!is.na(size_corr)) > 1) {
          # print("sum(!is.na(size_corr)) > 1)")
          cresc[which(!is.na(size_corr))[-1] - 1] <-
            diff(size_corr[!is.na(size_corr)]) / diff(time[!is.na(size_corr)])
          cresc_abs[which(!is.na(size_corr))[-1] - 1] <- diff(size_corr[!is.na(size_corr)])
        }
        # print("cresc updated")
        # print(cresc)
      }
  }
  # TAG TODO : add code corr
  return(data.frame("size_corr" = size_corr, "code_corr" = as.character(code_corr) ))
}







.correct_POM_changes <- function(size,
                                 size_corr,
                                 code_corr,
                                 cresc,
                                 time,
                                 POM,
                                 default_POM,
                                 positive_growth_threshold,
                                 negative_growth_threshold,
                                 i){
  ignore_negative_POM_changes = F
  # code_corr <- rep(0, length(size_corr))
  # Account for explicit POM changes

  if(POM[1] != default_POM){
    ####  WHAT ??? ###
    # How do we convert to dbh #
    warning(paste0("tree ",
                   i,
                   " is first measured with a POM equal to ",
                   POM[1],
                   ", thus translation-based corrections do not give a diameter at breast height for this tree."))
  }

  if(anyNA(POM)){
    POM <- .fill_na_POM(POM)
  }

  if(! all(POM == POM[1])){

    POM_diff <- diff(POM)
    if(!ignore_negative_POM_changes &
       any(POM_diff < 0)){
      msg <- paste0("It seems that you have negative POM changes ",
                    "for individual ",i,
                    " which is supposedly erroneous or abnormal.",
                    " If this is normal, run again with argument ignore_negative_pom_changes set to TRUE")
      stop(msg)
    }

    shifts <- which(!is.na(POM_diff) & POM_diff != 0)
    for(s in shifts){
      existing <- c(s-2,s-1,s+1,s+2)
      existing <- existing[existing > 0 & existing <= length(cresc)]
      existing <- existing[cresc[existing] > negative_growth_threshold &
        cresc[existing] < positive_growth_threshold] #Because we don't want to use outlyers to compute expected growth...
      meancresc <- max(mean(cresc[existing], na.rm=TRUE), 0)
      correction <- max(size_corr[s]-size_corr[s+1],0) + (meancresc*(time[s+1]-time[s]))
      size_corr[(s+1):length(size_corr)] <- (size_corr[(s+1):length(size_corr)]) + correction
      code_corr[(s+1):length(code_corr)] <- ifelse(code_corr[(s+1):length(code_corr)] == 0,
                                                   "POM",
                                                   paste(code_corr[(s+1):length(code_corr)], "POM", sep = "+"))
    }
  }
  res <- data.frame("size_corr" = size_corr, "code_corr"=as.character(code_corr))
  return(res)
}

.replace_missing <- function(size_corr, time,status){

  # tree <- tree[order(time),c("time","size","status")] # in case data is not ordered yet
  missing <- which(is.na(size_corr) & !is.na(status) & status == 1) # indices of the missing values
  present <- !is.na(size_corr) # to simplify the code written hereafter

  corrected_values <- rep(NA, length(size_corr))

  # correct each value - apply replaced by for: faster and also clearer.
  for(i in missing){
    if(i < min(which(present))){
      size_val <- size_corr[which(present)[1:min(2, sum(present))]]
      time_val <- time[which(!is.na(size_corr))[1:min(2, sum(present))]]
    }
    else if(i > max(which(present))){
      size_val <- size_corr[which(present)[(sum(present)-1):sum(present)]]
      time_val <- time[which(present)[(sum(present)-1):sum(present)]]

      size_val <- size_val[!is.na(size_val)]
      time_val <- time_val[!is.na(size_val)] ## Something weird here in C's original code
    }
    else{
      size_val <- size_corr[c(max(which(!is.na(size_corr[1:(i-1)]))), i+min(which(!is.na(size_corr[(i+1):length(size_corr)]))))]
      time_val <- time[c(max(which(!is.na(size_corr[1:(i-1)]))), i+min(which(!is.na(size_corr[(i+1):length(size_corr)]))))]
    }

    reg <- stats::lm(size_val ~ time_val)$coef
    corrected_values[i] <- reg[1] + reg[2]*time[i]

    if (sum(!is.na(time_val))==1) {
      corrected_values[i] <- size_val
    }
  }
  size_corr[is.na(size_corr)] <- corrected_values[is.na(size_corr)]
  return(size_corr)
}


.fill_na_POM <- function(POM){ # TO CHECK
  if(any(is.na(POM))){
    if(all(is.na(POM))){
      POM <- rep(1.3, length(POM))
    }
    else{
      while(any(is.na(POM))){
        index <- which.max(is.na(POM))
        # print(index);print(length(POM))
        if(index == 1){
          POM[1:(which.max(!is.na(POM))[-1])] <- POM[(which.max(!is.na(POM)))]
        }
        else if(index == length(POM)){

          POM[index] <- POM[index-1]
        }
      }
    }

  }
}

# @param data Data.frame, no default. Forest inventory in the form of a long-format time series - one line is a measure for one individual during one census time.
# @param size_col Character. The name of the column containing tree size measurements - diameter or circumference at breast height
# @param time_col Character. The name of the column containing census year
# @param status_col Character. The name of the column containing tree vital status - 0=dead; 1=alive.
# @param id_col Character. The name of the column containing trees unique ids
# @param POM_col Character. The name of the column containing trees POM
# @param positive_growth_threshold Numeric. Upper threshold over which an annual DIAMETER growth is considered abnormal. Defaults to 5 cm.
# @param negative_growth_threshold Numeric. Lower threshold under which a negative annual DIAMETER growth is considered abnormal. Defaults to -2 cm.
# @param default_POM Numeric. POM normally used in the forest inventory- defaults to the internationa convention of breast height, 1.3
