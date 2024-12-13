#' Exit BeSD-TI program at the end of a run or after an error
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param halt_message Error text to print when BeSD-TI is stopped mid-run
#'
#' @return Clean exit from BeSD-TI programs
#'
#' @importFrom stringr str_detect
#' @import openxlsx
#' @importFrom rlang abort

# besd_halt_immediately R version 1.00 - Biostat Global Consulting - 2024-09-06
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-09-06  1.00      Caitlin Clary   Original R version adapted from v1.07 of
#                                       vcqi_halt_immediately in vcqiR
#
# ******************************************************************************

# Note: AGGREGATE_DATABASES logic is not implemented because DESC
# databases are not aggregated in our other software and BeSD-TI only makes DESC
# databases - currently commented out

besd_halt_immediately <- function(
    VCP = "besd_halt_immediately",
    halt_message = NA
){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  # if (CHECK_INSTEAD_OF_RUN != 1){

    # **************************************************************************
    # If the user made organ pipe plots and saved accompanying datasets, augment
    # those datasets with the cluster names

    # TO DO add function
    # if (besd_object_value("MAKE_PLOTS", 1) &
    #    besd_object_value("MAKE_OP_PLOTS", 1) &
    #    besd_object_value("SAVE_OP_PLOT_DATA", 1)){
    #   add_HH_vars_to_opplot_datasets()
    # }

    # If the user has specified to keep the temp databases, run program to append
    # all databases into one database

    if (besd_object_value("DELETE_DATABASES_AT_END", 0) &
       besd_object_value("AGGREGATE_DATABASES", 1) &
       !besd_object_value("GENERATE_DATABASES", 0)){
      # AGGREGATE_DATABASES()
    }

    #  If user specified to delete databases put a comment in the log
    if (besd_object_value("DELETE_DATABASES_AT_END", 1)){
      besd_log_comment(VCP, 3, "Comment",
                       "User has specified that BeSD-TI databases should all be deleted.")
    }

    # if (besd_object_value("DELETE_DATABASES_AT_END", 1) |
    #    (besd_object_value("DELETE_DATABASES_AT_END", 0) &
    #     besd_object_value("AGGREGATE_DATABASES", 1))){

  if (besd_object_value("DELETE_DATABASES_AT_END", 1)){

      # Note: this will erase all files that end in _database.dta in the output
      # folder...even if those files were generated by an earlier BeSD-TI run. This
      # makes it important to direct the output of each run to its own output
      # folder

      if (besd_object_exists("BESDTI_DATABASES")){
        for(f in seq_along(BESDTI_DATABASES)){
          besd_log_comment(VCP, 3, "Comment",
                           paste0("Erasing ", BESDTI_DATABASES[f], ".rds"))
          if (stringr::str_detect(BESDTI_DATABASES[f], ".rds")){
            file.remove(paste0(OUTPUT_FOLDER, "/", BESDTI_DATABASES[f]))
          } else {
            file.remove(paste0(OUTPUT_FOLDER, "/", BESDTI_DATABASES[f], ".rds"))
          }
        }
      }
    }

    # Clean up after report template programs
    if (MAKE_TEMPLATE_REPORT == 1){
      unlink(paste0(OUTPUT_FOLDER, "/Report Inputs"), recursive = TRUE)
      temp_rmd <- list.files(paste0(OUTPUT_FOLDER, "/Report_Template"))
      temp_rmd <- temp_rmd[stringr::str_detect(temp_rmd, ".Rmd")]
      for (tr in seq_along(temp_rmd)){
        file.remove(paste0(OUTPUT_FOLDER, "/Report_Template/", temp_rmd[tr]))
      }
    }

    # **************************************************************************
    # Delete BeSD-TI temp datasets if the user requests it. Most of the measures
    # clean up after themselves, but there are some temporary datasets that
    # persist across measures; clean those up here.

    # TO DO - update RI references

    if (besd_object_value("DELETE_TEMP_DATASETS", 1)){

      if (!besd_object_exists("TEMP_DATASETS")){
        TEMP_DATASETS <- NULL
      }

      datasetlist <- TEMP_DATASETS

      for (d in seq_along(datasetlist)) {
        besd_log_comment(VCP,3,"Cleanup",
                         paste0("Erasing temp dataset ", datasetlist[d]))
        if (stringr::str_detect(datasetlist[d], ".rds")) {
          file.remove(paste0(OUTPUT_FOLDER, "/", datasetlist[d]))
        } else {
          file.remove(paste0(OUTPUT_FOLDER, "/", datasetlist[d], ".rds"))
        }
      } # end d loop

      # Temp datasets by program:
      programs <- c("DESC_02", "DESC_03")

      # Note 2022-10-27: some RI_QUAL_09 datasets are logged in BESDTI_DATABASES
      # and in RI_QUAL_09_TEMP_DATASETS - they are removed with BESDTI_DATABASES
      # earlier in this program, so trigger warnings when the code below
      # attempts to remove them again. Added if (file.exists) calls below to
      # prevent these warnings.

      for(p in seq_along(programs)){
        if (besd_object_exists(paste0(programs[p], "_TEMP_DATASETS"))){
          tempset <- get(paste0(programs[p], "_TEMP_DATASETS"), envir = globalenv())

          if (!is.null(tempset) & length(tempset) > 0){
            for (d in seq_along(tempset)){
              besd_log_comment(VCP, 3, "Cleanup",
                               paste0("Erasing temp dataset ", tempset[d]))

              if (stringr::str_detect(tempset[d], ".rds")){
                if (file.exists(paste0(OUTPUT_FOLDER, "/", tempset[d]))){
                  file.remove(paste0(OUTPUT_FOLDER, "/", tempset[d]))}
              } else {
                if (file.exists(paste0(OUTPUT_FOLDER, "/", tempset[d],".rds"))){
                  file.remove(paste0(OUTPUT_FOLDER, "/", tempset[d],".rds"))}
              }
            } # end d loop
          }

        }
      } # end p loop
    }

    # ****************************************************************************
    # Let the user know (in console and logfile) if BeSD-TI is exiting prematurely
    if (besd_object_value("BESDTI_ERROR", 1)){
      message("BeSD-TI is exiting prematurely because of an error.")
      besd_log_comment(VCP, 1, "Error", "BeSD-TI is exiting prematurely because of an error.")
    }

    # ****************************************************************************
    # Clean up and close log file

    besd_log_comment(VCP, 3, "Comment", "Closing and exporting the log...")

     # Note: need to use this assign process, not besd_global
    assign("BESDTI_LOGOPEN", 0, envir = globalenv())

    vhi_log <- read.csv(paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".csv"))

    # Backup process in case the log file doesn't have appropriate header
    if (any(names(vhi_log) == "level") == FALSE |
       any(names(vhi_log) == "entry_type") == FALSE){
      firstrow <- names(vhi_log)
      firstrow <- ifelse(substr(firstrow, 1, 1) == "X", str_replace(firstrow, "X", ""), firstrow)

      firstrow <- data.frame(
        date = str_replace_all(firstrow[1], fixed("."), "-"),
        time = str_replace_all(firstrow[2], fixed("."), ":"),
        program = firstrow[3],
        level = as.numeric(firstrow[4]),
        entry_type = firstrow[5],
        entry = firstrow[6]
      ) %>%
        mutate(date = as.character(date), time = as.character(time),
               program = as.character(program), level = as.numeric(level),
               entry_type = as.character(entry_type), entry = as.character(entry))

      names(vhi_log) <- c("date", "time", "program", "level", "entry_type", "entry")

      vhi_log <- vhi_log %>%
        mutate(date = as.character(date), time = as.character(time),
               program = as.character(program), level = as.numeric(level),
               entry_type = as.character(entry_type), entry = as.character(entry))

      vhi_log <- bind_rows(firstrow, vhi_log)
    }

    n_logrows <- nrow(vhi_log)
    n_errors <- filter(vhi_log, level %in% 1) %>% nrow()
    n_warnings <- filter(vhi_log, level %in% 2 & entry_type == "Warning") %>% nrow()

    vhi_log <- vhi_log %>%
      mutate(sequence = 1:n()) %>%
      select(sequence, everything()) %>%
      arrange(level, sequence)

    # Rename variables for export to the first row of the log
    names(vhi_log) <- c("Log Sequence", "Date", "Time", "Program", "Level",
                        "Log Entry Type", "Log Entry")

    vhi_wb <- createWorkbook()
    addWorksheet(vhi_wb, "Log")

    errorStyle <- createStyle(bgFill = "#FFC7CE")
    warningStyle <- createStyle(bgFill = "#FFEB9C")
    headerStyle <- createStyle(fgFill = "#D1D1D1")
    centerStyle <- createStyle(halign = "center") ##TO DO double check this gives what we want

    # Format error rows
    conditionalFormatting(
      wb = vhi_wb, sheet = "Log",
      cols = 1:ncol(vhi_log), rows = 2:nrow(vhi_log),
      rule = '$F2="Error"', style = errorStyle
    )

    # Format warning rows
    conditionalFormatting(
      wb = vhi_wb, sheet = "Log",
      cols = 1:ncol(vhi_log), rows = 2:nrow(vhi_log),
      rule = '$F2="Warning"', style = warningStyle
    )

    writeData(vhi_wb, sheet = "Log", vhi_log)

    # Set the widths of the log columns to hold the text
    # expand the column width to hold the text (max allowable width via mata = 255)
    # setColWidths(vhi_wb, sheet = "Log", cols = 1, widths =  12)
    # setColWidths(vhi_wb, sheet = "Log", cols = 2, widths =  12)
    # setColWidths(vhi_wb, sheet = "Log", cols = 3, widths =   9)
    # setColWidths(vhi_wb, sheet = "Log", cols = 4, widths =  30)
    # setColWidths(vhi_wb, sheet = "Log", cols = 5, widths =   6)
    # setColWidths(vhi_wb, sheet = "Log", cols = 6, widths =  15)
    # setColWidths(vhi_wb, sheet = "Log", cols = 7, widths = 225)

    setColWidths(vhi_wb, sheet = "Log",
                 cols = c(1, 2, 3, 4, 5, 6, 7),
                 widths = c(12, 12, 9, 30, 6, 15, 225))

    # Align cell
    addStyle(wb = vhi_wb, sheet = "Log",
      cols = 1, rows = 1:(nrow(vhi_log)+1),
      style = centerStyle)

    addStyle(wb = vhi_wb, sheet = "Log",
      cols = 5, rows = 1:(nrow(vhi_log)+1),
      style = centerStyle)

    # Format headers
    addStyle(wb = vhi_wb, sheet = "Log",
             cols = 1:7, rows = 1,
             style = headerStyle)

    saveWorkbook(
      vhi_wb,
      file = paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".xlsx"),
      overwrite = TRUE)

    # Replace Log tab in TO file - testing
    to <- openxlsx::loadWorkbook(paste0(OUTPUT_FOLDER, "/", ANALYSIS_NAME, "_TO.xlsx"))

    # Backup process in case Log tab isn't available
    if (any(names(to) == "Log") == FALSE){
      addWorksheet(to, "Log")
      worksheetOrder(to) <- c(length(names(to)), (1:(length(names(to)) - 1)))
    }

    # Format error rows
    conditionalFormatting(
      wb = to, sheet = "Log",
      cols = 1:ncol(vhi_log), rows = 2:nrow(vhi_log),
      rule = '$F2="Error"', style = errorStyle
    )

    # Format warning rows
    conditionalFormatting(
      wb = to, sheet = "Log",
      cols = 1:ncol(vhi_log), rows = 2:nrow(vhi_log),
      rule = '$F2="Warning"', style = warningStyle
    )

    writeData(to, sheet = "Log", vhi_log)

    setColWidths(to, sheet = "Log",
                 cols = c(1, 2, 3, 4, 5, 6, 7),
                 widths = c(12, 12, 9, 30, 6, 15, 225))

    # Align cell
    addStyle(wb = to, sheet = "Log",
             cols = 1, rows = 1:(nrow(vhi_log)+1),
             style = centerStyle)

    addStyle(wb = to, sheet = "Log",
             cols = 5, rows = 1:(nrow(vhi_log)+1),
             style = centerStyle)

    # Format headers
    addStyle(wb = to, sheet = "Log",
             cols = 1:ncol(vhi_log), rows = 1,
             style = headerStyle)

    saveWorkbook(to, paste0(OUTPUT_FOLDER, "/", ANALYSIS_NAME, "_TO.xlsx"),
                 overwrite = TRUE)

    # Delete the .csv version of the log
    file.remove(paste0(OUTPUT_FOLDER, "/", LOGFILE_NAME, ".csv"))

    # Report n-1 if one of the errors is 'BeSD-TI is exiting prematurely' because that's not a separate error
    if (besd_object_value("BESDTI_ERROR", 1)){
      print(paste0("BeSD-TI ran with ", (n_errors -1), " error messages and ",
                   n_warnings, " warnings."))
    } else {
      print(paste0("BeSD-TI ran with ", n_errors," error messages and ",
                   n_warnings," warnings."))
    }

    if ((n_errors+n_warnings) > 0){
      print(paste0("See the Log worksheet in ", OUTPUT_FOLDER, "/",
                   LOGFILE_NAME, ".xlsx for more information"))
    }

    # Billboard
    cat(paste(
      r"(  =====================================================================   )",
      r"(     BEHAVIOURAL AND SOCIAL DRIVERS OF VACCINATION: TOOLS FOR IMPACT      )",
      r"(  =====================================================================   )",
      r"(                    ____      _____ ____      __________                  )",
      r"(                   / __ )___ / ___// __ \    /_  __/  _/                  )",
      r"(                  / __  / _ \\__ \/ / / /_____/ /  / /                    )",
      r"(                 / /_/ /  __/__/ / /_/ /_____/ / _/ /                     )",
      r"(                /_____/\___/____/_____/     /_/ /___/                     )",
      r"(  =====================================================================   )",
      r"(                      (C) WORLD HEALTH ORGANIZATION                       )",
      r"(  =====================================================================   )",
      r"()",
      sep = "\n"
    ))


    # ****************************************************************************
    # Exit with error code, if appropriate

    if (besd_object_value("BESDTI_ERROR", 1)){
      cleanup_BeSDTI_globals()

      if (all(is.na(halt_message))){halt_message <- ""} # message must be a character vector

      rlang::abort(message = halt_message,
                   call = NULL)
    } else {
      cleanup_BeSDTI_globals()
    }

  # } # end CHECK_INSTEAD_OF_RUN if()

}
