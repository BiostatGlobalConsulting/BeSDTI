#' Open a clean BeSD-TI log file
#'
#' @return A CSV file in OUTPUT_FOLDER
#'
#' @import openxlsx
#' @import readr

 # besd_open_log R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.03)
# *******************************************************************************

# Note: this program is called from besd_log_comment

# If the user has not established the required global variables then exit. We do
# not call the program to exit gracefully here, because that, too requires these
# global macros to be in place...this is a hard stop very early in the process
# of the run if these basics are not in place.

besd_open_log <- function(){
  if(besd_object_exists("BeSDTI_LOGOPEN") == TRUE){
    if(get("BeSDTI_LOGOPEN", envir = globalenv()) == 1){
      stop("You are attempting to open a BeSD-TI log file, but the BeSDTI_LOGOPEN value indicates that one is already open.")
    }
  }

  if(besd_object_exists("OUTPUT_FOLDER") == FALSE){
    stop("Define the OUTPUT_FOLDER before attempting to open a log file.")
  }

  if(besd_object_exists("ANALYSIS_NAME") == FALSE){
    stop("Define the ANALYSIS_NAME before attempting to open a log file.")
  }

  assign("LOGFILE_NAME", paste0("BeSD-TI_", ANALYSIS_NAME, "_LOG"), envir = .GlobalEnv)

  # Export placeholder text to Excel tabular output file

  TO_wb <- createWorkbook()
  addWorksheet(TO_wb, sheetName = "Log")

  # TO DO review the instructions to open/view the log - in Stata the process is to type besd_cleanup() - the current process is a functional placeholder
  writeData(TO_wb, "Log", data.frame(
    `Placeholder Log Text` = c(
      "This text is a placeholder.",
      "If BeSD-TI exits in a clean manner then this text will disappear.",
      "If BeSD-TI has halted and this text is in the worksheet, you might find an informative log in the R log dataset.",
      paste0("If that happens, navigate to your OUTPUT_FOLDER (", OUTPUT_FOLDER, ") and look for the log file named ", LOGFILE_NAME, ".csv"),
      "Open this BeSD-TI log and scroll to the bottom to discover clues as to what went wrong." #,
      # "If that happens, go to the R command line and type the following line:",
      # 'log <- read.csv(paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".csv"))',
      # "",
      # "Now the BeSD-TI log will be loaded into R as a dataset called 'log'. You can type View(log) in the R console to view the dataset and scroll to the bottom to discover clues as to what went wrong.",
      # "Contact GetVCQIHelp@biostatglobal.com if you have questions. If possible, attach your log file to the e-mail.")
    )
  )
  )

  openxlsx::saveWorkbook(
    TO_wb,
    file = paste0(OUTPUT_FOLDER, "/", ANALYSIS_NAME, "_TO.xlsx"),
    overwrite = TRUE)

  # Open log file
  besd_log <- data.frame(
    date = Sys.Date(),
    time = format(Sys.time(), format="%H:%M:%S"),
    program = "BESDTI_LOG_OPEN",
    level = 3,
    entry_type = "c(return)",
    entry = "The following comments document characteristics of the computer that is running BeSD-TI."
  )

  besd_log[2,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "BESDTI_LOG_OPEN", 3, "c(return)",
                    R.version.string)

  besd_log[3,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "BESDTI_LOG_OPEN", 3, "c(return)",
                    paste0("OS: ", Sys.info()[1]))

  besd_log[4,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "BESDTI_LOG_OPEN", 3, "c(return)",
                    paste0("Release: ", Sys.info()[2]))

  besd_log[5,] <- c(as.character(Sys.Date()),
                    as.character(format(Sys.time(), format="%H:%M:%S")),
                    "BESDTI_LOG_OPEN", 3, "c(return)",
                    paste0("Machine type: ", Sys.info()[5]))

  readr::write_csv(besd_log,
                   file = paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".csv"),
                   append = FALSE)

  assign("BESDTI_LOG_OPEN", 1, envir = .GlobalEnv)
}
