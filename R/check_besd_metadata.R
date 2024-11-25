#' Check BeSD-TI analysis-related globals, datasets and variables
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#'
#' @return Log entries; errors if conditions not met
#'
#' @export
#'
#' @examples
#' check_besd_metadata()

# check_besd_metadata R version 1.00 - Biostat Global Consulting - 2024-07-23

# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-23  1.00      Caitlin Clary   Original R version adapted from v1.11 of
#                                       check_RI_analysis_metadata in vcqiR
# 2024-11-12  1.01      Caitlin Clary   Add COVID-19 analysis checks
# *******************************************************************************

check_besd_metadata <- function(VCP = "check_besd_metadata",
                                       analysis){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  # Check the generic analysis-related globals
  check_besd_analysis_metadata()

  # Check the CM dataset
  if (besd_object_exists("CM_DATASET")){
    # check_CM_metadata()
  } else {

    besd_log_comment(
      VCP, 3, "Comment",
      "Cluster Metadata (CM) dataset not defined. When CM is not specified, BeSD-TI assumes that any relevant design variables (e.g. stratum IDs, cluster IDs, weights) are present in the main analysis dataset."
    )
  }

  if (DATA_FOLDER %in% OUTPUT_FOLDER){
    errormsgs <- "DATA_FOLDER and OUTPUT_FOLDER cannot be the same. Please change one path."
    besd_log_comment(VCP, 1, "Error",
                     "DATA_FOLDER and OUTPUT_FOLDER cannot be the same. Please change one path.")

    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  exitflag <- 0
  errormsgs <- NULL

  if (MAKE_TEMPLATE_REPORT == 1){

    # Initiate object for tracking report inputs
    assign("BESDTI_REPORT_INPUTS", NULL, envir = .GlobalEnv)

    # Start some lists used for report compilation (not currently using)
    # besd_global(REPORT_INDICATOR_LIST, NULL)
    # besd_global(REPORT_INDICATOR_TITLE_LIST, NULL)

    if (EXPORT_TO_EXCEL != 1){
      newerror <- "To make a template report (MAKE_TEMPLATE_REPORT = 1), BeSD-TI requires that the EXPORT_TO_EXCEL option be turned on. Please set besd_global(EXPORT_TO_EXCEL, 1) in the control program or turn the MAKE_TEMPLATE_REPORT option off."

      errormsgs <- c(errormsgs, newerror)

      exitflag <- 1
      besd_log_comment(VCP, 1, "Error", newerror)
    }

    if (MAKE_PLOTS != 1){
      newerror <- "To make a template report (MAKE_TEMPLATE_REPORT = 1), BeSD-TI requires that the MAKE_PLOTS option be turned on. Please set besd_global(MAKE_PLOTS, 1) in the control program or turn the MAKE_TEMPLATE_REPORT option off."

      errormsgs <- c(errormsgs, newerror)

      exitflag <- 1
      besd_log_comment(VCP, 1, "Error", newerror)
    }
  }

  # Child vaccination analysis checks ----
  if (analysis == "child"){
    # Add logic: checks for child vaccination BeSD analysis

    if (!besd_object_exists("CH_DATASET")){
      errormsgs <- c(errormsgs,
                     "Please set CH_DATASET.")
      exitflag <- 1
      besd_log_comment(VCP, 1, "Error", "Please set CH_DATASET.")
    } else {

      # Check that child dataset exists
      ch_data_file <- paste0(DATA_FOLDER, "/", CH_DATASET)
      if (file.exists(ch_data_file)){
        file.copy(from = ch_data_file, to = OUTPUT_FOLDER, overwrite = TRUE)

        besd_global(TEMP_DATASETS,
                    c(TEMP_DATASETS, CH_DATASET))
      }

      if (!file.exists(ch_data_file)){
        errormsgs <- c(
          errormsgs,
          paste0("The file defined by global macros DATA_FOLDER/CH_DATASET (",
                 ch_data_file, ") does not exist"))

        exitflag <- 1
        besd_log_comment(VCP, 1, "Error",
                         paste0("Child dataset specified ( ",
                                ch_data_file, ") does not exist"))
      } else {

        # Read the child dataset
        dat <- besd_read(ch_data_file)

        if (is.data.frame(dat) == FALSE){
          errormsgs <- c(
            errormsgs,
            paste0("The file defined by global macros DATA_FOLDER/CH_DATASET (",
                   ch_data_file, ") is not in a valid format"))

          exitflag <- 1
          besd_log_comment(
            VCP, 1, "Error",
            paste0("Child dataset (", ch_data_file, ") is not in a valid format"))

        } else {

          # Check existence and type of required variables

          numeric_varlist <- c(
            "Clusternum",
            "CHI_intent", "CHI_confb", "CHI_normf", "CHI_where", "CHI_afford")

          string_varlist <- c("Districtname", "PUID", "Housenum")

          no_missing_varlist <- c("Districtname", "Clusternum", "Housenum", "PUID")

          varlist <- unique(c(numeric_varlist, string_varlist))

          for(v in seq_along(varlist)){
            if (varlist[v] %in% names(dat)){

              # If the variable exists, confirm the variable is not missing and
              # has the correct variable type

              var_v <- get(varlist[v], dat)

              if (varlist[v] %in% numeric_varlist){

                if (all(!class(var_v) %in% c("numeric", "double", "integer"))){
                  errormsgs <- c(
                    errormsgs,
                    paste0(numeric_varlist[v], " needs to be a numeric variable in the child dataset."))
                  exitflag <- 1
                  besd_log_comment(
                    VCP, 1, "Error",
                    paste0(numeric_varlist[v], " needs to be a numeric variable in the child dataset."))
                }
              }

              if (varlist[v] %in% no_missing_varlist){
                if (any(var_v == "" | is.na(var_v))){
                  errormsgs <- c(errormsgs,
                                 paste0("The variable ", varlist[v],
                                        " in the child dataset cannot have missing values."))
                  exitflag <- 1
                  besd_log_comment(
                    VCP, 1, "Error",
                    paste0("The variable ", varlist[v],
                           " in the child dataset cannot have a missing value."))
                }
              }

            } else {
              errormsgs <- c(
                errormsgs,
                paste0("The variable ", varlist[v],
                       " does not exist in the child dataset and is required to run BeSD-TI"))
              exitflag <- 1
              besd_log_comment(
                VCP, 1, "Error",
                paste0("The variable ", varlist[v],
                       " does not exist in the child dataset and is required to run BeSD-TI"))
            }
          } # end varlist loop

          # Check values of the core indicator variables
          if (any(!dat$CHI_intent %in% c(1, 2, 3, NA))){
            errormsgs <- c(
              errormsgs,
                "The CHI_intent variable has invalid values. Valid values are 1, 2, 3, and missing (NA)."
              )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The CHI_intent variable has invalid values. Valid values are 1, 2, 3, and missing (NA)."
            )
          } # end CHI_intent check

          if (any(!dat$CHI_confb %in% c(1, 2, 3, 4, NA))){
            errormsgs <- c(
              errormsgs,
              "The CHI_confb variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The CHI_confb variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )
          } # end CHI_confb check

          if (any(!dat$CHI_normf %in% c(0, 1, NA))){
            errormsgs <- c(
              errormsgs,
              "The CHI_normf variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The CHI_normf variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )
          } # end CHI_normf check

          if (any(!dat$CHI_where %in% c(0, 1, NA))){
            errormsgs <- c(
              errormsgs,
              "The CHI_where variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The CHI_where variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )
          } # end CHI_where check

          if (any(!dat$CHI_afford %in% c(1, 2, 3, 4, NA))){
            errormsgs <- c(
              errormsgs,
              "The CHI_afford variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The CHI_afford variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )
          } # end CHI_afford check
        } # end else{} when child dataset in valid format
      } # end else{} when child dataset exists
    } # end else{} when CH_DATASET is set
  } # end if analysis = child

  # COVID-19 analysis checks ----

  if (analysis == "covid"){

    if (!besd_object_exists("COV_SURVEY_RESPONDENTS")){
      errormsgs <- c(
        errormsgs,
        "Please define the respondent type for this survey by setting COV_SURVEY_RESPONDENTS")

      exitflag <- 1
      besd_log_comment(
        VCP, 1, "Error",
        "Please define the respondent type for this survey by setting COV_SURVEY_RESPONDENTS")
    } else {

      if (!stringr::str_to_lower(COV_SURVEY_RESPONDENTS) %in% c("adults", "health workers")){
        errormsgs <- c(
          errormsgs,
          "The value of COV_SURVEY_RESPONDENTS is not valid; accepted values are 'Adults' or 'Health Workers'")

        exitflag <- 1
        besd_log_comment(
          VCP, 1, "Error",
          "The value of COV_SURVEY_RESPONDENTS is not valid; accepted values are 'Adults' or 'Health Workers'")
      }

    }

    if (!besd_object_exists("COV_DATASET")){
      errormsgs <- c(errormsgs,
                     "Please set COV_DATASET")
      exitflag <- 1
      besd_log_comment(VCP, 1, "Error", "Please set COV_DATASET")
    } else {

      # Check that COVID-19 survey dataset exists
      cv_data_file <- paste0(DATA_FOLDER, "/", COV_DATASET)
      if (file.exists(cv_data_file)){
        file.copy(from = cv_data_file, to = OUTPUT_FOLDER, overwrite = TRUE)

        besd_global(TEMP_DATASETS,
                    c(TEMP_DATASETS, COV_DATASET))
      }

      if (!file.exists(cv_data_file)){
        errormsgs <- c(
          errormsgs,
          paste0("The file defined by global macros DATA_FOLDER/COV_DATASET (",
                 cv_data_file, ") does not exist"))

        exitflag <- 1
        besd_log_comment(VCP, 1, "Error",
                         paste0("COVID-19 dataset specified ( ",
                                cv_data_file, ") does not exist"))
      } else {

        # Read the COVID-19 dataset
        dat <- besd_read(cv_data_file)

        if (is.data.frame(dat) == FALSE){
          errormsgs <- c(
            errormsgs,
            paste0("The file defined by global macros DATA_FOLDER/COV_DATASET (",
                   cv_data_file, ") is not in a valid format"))

          exitflag <- 1
          besd_log_comment(
            VCP, 1, "Error",
            paste0("COVID-19 dataset (", cv_data_file, ") is not in a valid format"))

        } else {

          # Check existence and type of required variables

          numeric_varlist <- c(
            "Clusternum",
            "COV_intent", "COV_confb", "COV_normf", "COV_where", "COV_afford")

          string_varlist <- c("Districtname", "PUID", "Housenum")

          no_missing_varlist <- c("Districtname", "Clusternum", "Housenum", "PUID")

          varlist <- unique(c(numeric_varlist, string_varlist))

          for(v in seq_along(varlist)){
            if (varlist[v] %in% names(dat)){

              # If the variable exists, confirm the variable is not missing and
              # has the correct variable type

              var_v <- get(varlist[v], dat)

              if (varlist[v] %in% numeric_varlist){

                if (all(!class(var_v) %in% c("numeric", "double", "integer"))){
                  errormsgs <- c(
                    errormsgs,
                    paste0(numeric_varlist[v], " needs to be a numeric variable in the COVID-19 dataset."))
                  exitflag <- 1
                  besd_log_comment(
                    VCP, 1, "Error",
                    paste0(numeric_varlist[v], " needs to be a numeric variable in the COVID-19 dataset."))
                }
              }

              if (varlist[v] %in% no_missing_varlist){
                if (any(var_v == "" | is.na(var_v))){
                  errormsgs <- c(errormsgs,
                                 paste0("The variable ", varlist[v],
                                        " in the COVID-19 dataset cannot have missing values."))
                  exitflag <- 1
                  besd_log_comment(
                    VCP, 1, "Error",
                    paste0("The variable ", varlist[v],
                           " in the COVID-19 dataset cannot have a missing value."))
                }
              }

            } else {
              errormsgs <- c(
                errormsgs,
                paste0("Variable ", varlist[v],
                       " does not exist in the COVID-19 dataset and is required to run BeSD-TI"))
              exitflag <- 1
              besd_log_comment(
                VCP, 1, "Error",
                paste0("The variable ", varlist[v],
                       " does not exist in the COVID-19 dataset and is required to run BeSD-TI"))
            }
          } # end varlist loop

          # Check values of the core indicator variables

          # COV_intent: 0 = no, 1 = yes, 2 = not sure, 3 = already vx
          if (any(!dat$COV_intent %in% c(0, 1, 2, 3, NA))){
            errormsgs <- c(
              errormsgs,
              "The COV_intent variable has invalid values. Valid values are 0, 1, 2, 3, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The COV_intent variable has invalid values. Valid values are 0, 1, 2, 3, and missing (NA)."
            )
          } # end COV_intent check

          if (any(!dat$COV_confb %in% c(1, 2, 3, 4, NA))){
            errormsgs <- c(
              errormsgs,
              "The COV_confb variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The COV_confb variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )
          } # end COV_confb check

          if (any(!dat$COV_normf %in% c(0, 1, NA))){
            errormsgs <- c(
              errormsgs,
              "The COV_normf variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The COV_normf variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )
          } # end COV_normf check

          if (any(!dat$COV_where %in% c(0, 1, NA))){
            errormsgs <- c(
              errormsgs,
              "The COV_where variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The COV_where variable has invalid values. Valid values are 0, 1, and missing (NA)."
            )
          } # end COV_where check

          if (any(!dat$COV_afford %in% c(1, 2, 3, 4, NA))){
            errormsgs <- c(
              errormsgs,
              "The COV_afford variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )

            exitflag <- 1
            besd_log_comment(
              VCP, 1, "Error",
              "The COV_afford variable has invalid values. Valid values are 1, 2, 3, 4, and missing (NA)."
            )
          } # end COV_afford check
        } # end else if COV_DATASET was read in
      } # end else if COV_DATASET (file read/variable checks)
    } # end else if COV_DATASET exists (file copy)
  }

  if (exitflag == 1){
    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}

