#' Check language choice and language file for BeSD
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @return A global that records the language choice and errors and/or warnings if conditions not met
#'
#' @import stringr
#' @import openxlsx
#'
#' @export
#'
#' @examples
#' besd_multi_lingual_strings()

# besd_multi_lingual_strings R version 1.00 - Biostat Global Consulting - 2024-08-09
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-09  1.00      Caitlin Clary   Original R version adapted from v1.02 of
#                                       vcqi_multi_lingual_strings in vcqiR
# *******************************************************************************

# TO DO - handler if besd_read returns an error rather than a data frame?
# TO DO - better error messages when selected language has missing values?

besd_multi_lingual_strings <- function(VCP = "besd_multi_lingual_strings"){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  # besd_global(LANGUAGE_FILE, "default")

  if (!besd_object_exists("LANGUAGE_FILE")){
    language_file <- default_multilingual
    lang_default <- TRUE
  } else {
    lang_temp <- get("LANGUAGE_FILE", envir = .GlobalEnv)
    if (stringr::str_to_lower(lang_temp) %in% "default") {
      language_file <- default_multilingual
      lang_default <- TRUE
    } else {
      # Add logic to load user-supplied file; error if file not found

      if (file.exists(lang_temp)){
        language_file <- besd_read(lang_temp)
        lang_default <- FALSE
      } else {
        errormsgs <- c(
          errormsgs,
          paste0("The multilingual string file specified in the global LANGUAGE_FILE (", LANGUAGE_FILE, ") could not be found. Update the file path or set this global to 'default' to use the package default multilingual strings.")
        )
        lang_default <- TRUE
      }
    }
  }

  if (!besd_object_exists("OUTPUT_LANGUAGE")){
    language <- "ENGLISH"
  } else {

    language <- get("OUTPUT_LANGUAGE", envir = .GlobalEnv)
    language <- str_to_upper(language)

    if (language == "EN"){
      language <- "ENGLISH"
    } else if (language == "ES"){
      language <- "SPANISH"
    } else if (language == "FR"){
      language <- "FRENCH"
    } else if (language == "PT"){
      language <- "PORTUGUESE"
    }
  }

  if (lang_default == FALSE) {
    language_options <- stringr::str_to_upper(names(language_file)[2:ncol(language_file)])
  } else {
    language_options <- c("ENGLISH", "SPANISH", "FRENCH", "PORTUGUESE")
  }

  if (!language %in% language_options){

    # Update error message

    if (lang_default %in% TRUE){
      errormsgs <- c(
        errormsgs,
        paste0("Global OUTPUT_LANGUAGE is set to an invalid value (", OUTPUT_LANGUAGE,
               "). Valid values (when using the default BeSD-TI multilingual strings file) are English, EN, Spanish, SP, French, FR, Portuguese, or PT."))
      besd_log_comment(
        VCP, 1, "Error",
        paste0("Global OUTPUT_LANGUAGE is set to an invalid value (", OUTPUT_LANGUAGE,
               "). Valid values (when using the default BeSD-TI multilingual strings file) are English, EN, Spanish, SP, French, FR, Portuguese, or PT."))
      exitflag <- 1
    } else {
      errormsgs <- c(
        errormsgs,
        paste0("Global OUTPUT_LANGUAGE is set to an invalid value (", OUTPUT_LANGUAGE,
               "). The languages available in the user-supplied multilingual strings file (", LANGUAGE_FILE, ") are ",
               paste(language_options, collapse = ", "), "."))

      besd_log_comment(
        VCP, 1, "Error",
        paste0("Global OUTPUT_LANGUAGE is set to an invalid value (", OUTPUT_LANGUAGE,
               "). The languages available in the user-supplied multilingual strings file (", LANGUAGE_FILE, ") are ",
               paste(language_options, collapse = ", "), "."))
      exitflag <- 1
    }
  } else {

    # if (file.exists(paste0(DATA_FOLDER,"/Multi-Lingual Phrases - En Fr Es Pt.xlsx"))){
    #   dat <- openxlsx::read.xlsx(xlsxFile = paste0(DATA_FOLDER,"/Multi-Lingual Phrases - En Fr Es Pt.xlsx"),
    #                              sheet = "Latest version")
    # } else {
    #   errormsgs <- c(errormsgs,paste0("Multi-Lingual file not found in ", DATA_FOLDER))
    #   besd_log_comment(VCP,1,"Error", paste0("Multi-Lingual file not found in ", DATA_FOLDER))
    #   exitflag <- 1
    # }

    if (exitflag == 1){
      besd_global(BeSD_ERROR, 1)
      besd_halt_immediately(halt_message = errormsgs)
    }

    dat <- language_file
    names(dat) <- str_to_upper(names(dat))

    var <- get(language,dat)
    var <- haven::zap_label(var)
    var <- haven::zap_labels(var)

    if ("character" %in% class(var)){
      if (any(is.na(var)| var == "")){
        errormsgs <- c(errormsgs, paste0("Variable ", language, " is missing values. Populate and rerun."))
        besd_log_comment(VCP, 1, "Error", paste0("Variable ", language, " is missing values. Populate and rerun."))
        exitflag <- 1
      }
    } else if (any(is.na(var))){
      errormsgs <- c(errormsgs, paste0("Variable ", language, " is missing values. Populate and rerun."))
      besd_log_comment(VCP, 1, "Error", paste0("Variable ", language, " is missing values. Populate and rerun."))
      exitflag <- 1
    }

  }

  if(exitflag == 1){
    besd_global(RUN_ERROR, 1)
    besd_halt_immediately(halt_message = errormsgs)
  } else {
    assign("language_use", language, envir = .GlobalEnv)
    assign("language_file", language_file, envir = .GlobalEnv)
  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}
