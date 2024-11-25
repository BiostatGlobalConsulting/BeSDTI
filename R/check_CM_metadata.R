#' Check existence, format, and contents of the cluster metadata (CM) dataset
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @return Errors and/or warnings if conditions not met
#'
#' @import dplyr
#'

# check_CM_metadata R version 1.01 - Biostat Global Consulting - 2024-07-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-23  1.00      Caitlin Clary   Original R version adapted from v1.04 of
#                                       check_VCQI_CM_metadata in vcqiR
# *******************************************************************************

check_CM_metadata <- function(VCP = "check_CM_metadata"){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  if (besd_object_exists("CM_DATASET")) {
    if (file.exists(paste0(DATA_FOLDER, "/", CM_DATASET))){
      # Read CM dataset
      CM <- besd_read(paste0(DATA_FOLDER, "/", CM_DATASET))

      # Check if the file is in valid format
      if (is.data.frame(CM) == FALSE){
        errormsgs <- c(
          errormsgs,
          paste0("The file defined by global macros DATA_FOLDER/CM_DATASET (",
                 paste0(DATA_FOLDER, "/", CM_DATASET), ") is not in valid format"))

        exitflag <- 1
        besd_log_comment(VCP, 1, "Error",
                         paste0("CM dataset (", DATA_FOLDER, "/",
                                CM_DATASET, ") is not in valid format"))
      } else {
        # Determine which psweight variable is required
        if (exists("CH_DATASET")) {
          psw <- "psweight_ch"
        } else if (exists("COVID_DATASET")) {
          psw <- "psweight_cov"
        } else {
          psw <- NULL
        }

        # Variable checks

        cm_vars <- c("stratum_id", "stratum_name",
                     "cluster_id", "cluster_name", psw)
        for(i in seq_along(cm_vars)) {
          if (cm_vars[i] %in% names(CM)) {
            # Check variable format
            if (cm_vars[i] %in% c("stratum_name", "cluster_name")) {
              if (is.character(get(cm_vars[i], CM)) == FALSE) {
                warningmsgs <- c(warningmsgs,
                                 paste0(cm_vars[i]," should be a character variable in the CM dataset."))
                besd_log_comment(VCP, 2, "Warning",
                                 paste0(cm_vars[i]," should be a character variable in the CM dataset."))
              }
            } else {
              if (is.numeric(get(cm_vars[i], CM)) == FALSE) {
                warningmsgs <- c(warningmsgs,
                                 paste0(cm_vars[i]," should be a numeric variable in the CM dataset."))
                besd_log_comment(VCP, 2, "Warning",
                                 paste0(cm_vars[i]," should be a numeric variable in the CM dataset."))
              }
            }

            # # Check for missing values
            # if (cm_vars[i] %in% c("province_id", "urban_cluster")) {
            #   if (sum(is.na(get(cm_vars[i], CM))) > 0) {
            #     warningmsgs <- c(
            #       warningmsgs,
            #       paste0(cm_vars[i], " should not have a missing value in the CM dataset."))
            #
            #     besd_log_comment(VCP,2,"Warning",paste0(cm_vars[i]," should not have a missing value in the CM dataset."))
            #   }
            # }

          } else {
            # Error if variable is not present in CM

            errormsgs <- c(
              errormsgs,
              paste0("Variable ", cm_vars[i], " does not exist in the CM dataset and is required to run BeSD-TI"))
            besd_log_comment(VCP, 1, "Error",
                             paste0("Variable ", cm_vars[i], " does not exist in the CM dataset and is required to run BeSD-TI"))

            exitflag <- 1
          }
        } # end variable check loop

        # If province_id is in the CM dataset, confirm that every row with the
        # same value of stratum_id has the same value of province_id
        if ("stratum_id" %in% names(CM) & "province_id" %in% names(CM)){
          prov_test <- CM %>%
            select(stratum_id, province_id) %>%
            unique() %>% group_by(stratum_id) %>%
            summarize(n = n())
          if (max(prov_test$n) > 1) {
            errormsgs <- c(
              errormsgs,
              "The CM dataset has a problem. At least one value of stratum_id has more than one value of province_id. Edit the CM dataset so that each value of stratum_id is associated with a single value of province_id.")
            besd_log_comment(VCP, 1, "Error", "The CM dataset has a problem. At least one value of stratum_id has more than one value of province_id. Edit the CM dataset so that each value of stratum_id is associated with a single value of province_id.")
            exitflag <- 1
          }
        }

      }
    } else {
      errormsgs <- c(errormsgs, paste0("The file defined by ", DATA_FOLDER, "/", CM_DATASET, " does not exist."))
      besd_log_comment(VCP, 1, "Error", paste0("The file defined by ", DATA_FOLDER, "/", CM_DATASET, " does not exist."))
      exitflag <- 1
    } # End error message when file doesn't exist
  } else {
    errormsgs <- c(
      errormsgs,
      "Please set CM_DATASET in the control program.")
    besd_log_comment(VCP, 1, "Error", "Please set CM_DATASET in the control program.")
    exitflag <- 1
  } # End error message when CM_DATASET not defined


  if (!is.null(warningmsgs)){
    warning(warningmsgs)
  }

  if (exitflag == 1){
    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}


