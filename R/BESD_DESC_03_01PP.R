#' Pre-process dataset for DESC_03
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @return A dataset (DESC_03_<ANALYSIS_COUNTER>_<DESC_03_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# BESD_DESC_03_01PP R version 1.00 - Biostat Global Consulting - 2024-09-09
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-09-09  1.00      Caitlin Clary   Original R version adapted from v1.01 of
#                                       DESC_03_01PP in vcqiR
# *******************************************************************************


BESD_DESC_03_01PP <- function(VCP = "BESD_DESC_03_01PP",
                              database_id = NA){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  dat <- besd_read(paste0(OUTPUT_FOLDER,"/",DESC_03_DATASET))

  #* The user may call this measure several times with different combinations
  #* of inputs, so track a counter so each dataset gets saved for later
  #* examination, if necessary

  if (besd_object_exists("DESC_03_COUNTER")){
    DESC_03_COUNTER = DESC_03_COUNTER + 1
    assign("DESC_03_COUNTER", DESC_03_COUNTER, envir = .GlobalEnv)
  }

  if (!besd_object_exists("DESC_03_COUNTER")){
    assign("DESC_03_COUNTER", 1, envir = .GlobalEnv)
  }

  # dat <- dat %>% select(level1id,level2id,level3id,stratumid,clusterid,respid,
  #                       HH02,HH04,psweight,all_of(OUTPUT_VARLIST),all_of(DESC_03_VARIABLES),
  #                       all_of(BESDTI_PASS_THRU_VARLIST))

  dat <- dat %>%
    select(
      Districtname, Clusternum, clusterid, respid, psweight,
      all_of(OUTPUT_VARLIST),
      all_of(DESC_03_VARIABLES),
      all_of(BESDTI_PASS_THRU_VARLIST)
    )

  if (!is.na(database_id)){
    temp_filestub <- paste0("DESC_03_", ANALYSIS_COUNTER,
                            "_", database_id, ".rds")
  } else {
    temp_filestub <- paste0("DESC_03_", ANALYSIS_COUNTER,
                            "_", DESC_03_COUNTER, ".rds")
  }

  saveRDS(dat, file = paste0(OUTPUT_FOLDER, "/", temp_filestub))


  if (!besd_object_exists("DESC_03_TEMP_DATASETS")){
    DESC_03_TEMP_DATASETS <- NULL
  }

  besd_global(DESC_03_TEMP_DATASETS,
              c(DESC_03_TEMP_DATASETS, temp_filestub))

  rm(temp_filestub)

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
