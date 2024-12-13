#' Response to multiple-choice question with only one response
#'
#' @param VCP VCQI current program name to be logged, default to be the function name
#' @param database_id Custom tag for output file names (optional)
#' @param cleanup Clean up DESC_02 global settings after run
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' BESD_DESC_02()

# BESD_DESC_02 R version 1.00 - Biostat Global Consulting - 2024-08-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-08  1.00      Caitlin Clary   Original R version adapted from v1.00 of
#                                       DESC_02 in vcqiR
# *******************************************************************************

# Notes - introducing database_id concept in order to save core indicator
# DESC_02 databases in a clearly named way, e.g. DESC_02_1_intent,
# DESC_02_1_confb

BESD_DESC_02 <- function(VCP = "BESD_DESC_02",
                         database_id = NA,
                         cleanup = FALSE){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  if (besd_object_exists("DESC_02_VARIABLES")){
    print(paste0("Calculating ", VCP, " for ", DESC_02_VARIABLES, "..."))
  } else{
    print(paste0("Calculating ", VCP, " for BESD_DESC_02 ..."))
  }

  print("Checking global macros")
  BESD_DESC_02_00GC()

  if (PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    BESD_DESC_02_01PP(database_id = database_id)
  }

  if (GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    BESD_DESC_02_03DV(database_id = database_id)
  }

  if (GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    BESD_DESC_02_04GO(database_id = database_id)
  }

  if (EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    BESD_DESC_02_05TOST(database_id = database_id)
  }

  # Clear out globals if specified
  if (cleanup == TRUE){
    rm(list = c("DESC_02_DATASET", "DESC_02_VARIABLES",
                "DESC_02_WEIGHTED", "DESC_02_DENOMINATOR",
                "DESC_02_TO_TITLE", "DESC_02_TO_SUBTITLE",
                "DESC_02_N_LABEL", "DESC_02_NTWD_LABEL"),
       envir = .GlobalEnv) %>% suppressWarnings()

    if (besd_object_exists("DESC_02_N_RELABEL_LEVELS")){
      for (i in 1:DESC_02_N_RELABEL_LEVELS){
        rm(list = c(
          paste0("DESC_02_RELABEL_LEVEL_", i),
          paste0("DESC_02_RELABEL_LABEL_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    if (besd_object_exists("DESC_02_N_SUBTOTALS")){
      for (i in 1:DESC_02_N_SUBTOTALS){
        rm(list = c(
          paste0("DESC_02_SUBTOTAL_LEVELS_", i),
          paste0("DESC_02_SUBTOTAL_LABEL_", i),
          paste0("DESC_02_SUBTOTAL_LIST_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    for (i in 1:100){
      rm(list = c(paste0("DESC_02_TO_FOOTNOTE_", i)),
         envir = .GlobalEnv) %>% suppressWarnings()
    }

    rm(list = c("DESC_02_N_SUBTOTALS",
                "DESC_02_N_RELABEL_LEVELS",
                "DESC_02_SHOW_SUBTOTALS_ONLY",
                "DESC_02_LIST_N_BEFORE_PCT",
                "DESC_02_LIST_NWTD_BEFORE_PCT",
                "DESC_02_SUPPRESS_CI_OUTPUT"),
       envir = .GlobalEnv)%>% suppressWarnings()

  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}
