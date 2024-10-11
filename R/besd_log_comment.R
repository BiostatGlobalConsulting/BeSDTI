#' Write comment to BeSD-TI program progress log
#'
#' @param program The program this comment is associated with
#' @param level The level of this comment (1 = high level, e.g. errors, 2 = second-order, e.g. warnings, 3 = general comments, e.g. about files being saved; 4 = comments on properties of datasets, 5 = debugging detail, e.g. program flow comments)
#' @param keyword The category this comment belongs to, e.g. Error, Warning, Comment, Data, or Flow
#' @param comment The comment to be written
#'
#' @import readr
#'
#' @export
#'
#' @examples
#' besd_log_comment("Program 1", 1, "Error", "Required value X not defined")

# besd_log_comment R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.04)
# *******************************************************************************

besd_log_comment <- function(program, level, keyword, comment){
  # Note: besd_open_log() checks that VCQI_OUTPUT_FOLDER and VCQI_ANALYSIS_NAME exist
  # and creates VCQI_LOGFILE_NAME

  # If there isn't an open log file, open one now
  if(besd_object_exists("BESDTI_LOG_OPEN") == FALSE){
    besd_global(BESDTI_LOG_OPEN, 0)
  }

  if(BESDTI_LOG_OPEN != 1){
    besd_open_log()
  }

  if(besd_object_exists("OUTPUT_FOLDER") & besd_object_exists("LOGFILE_NAME")){
    if(!file.exists(
      paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".csv"))){
      besd_open_log()
    }}

  # Post comment to log file
  readr::write_csv(data.frame(
    as.character(Sys.Date()),
    as.character(format(Sys.time(), format="%H:%M:%S")),
    program, level, keyword, comment
  ),
  file = paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".csv"),
  append = TRUE)

}
