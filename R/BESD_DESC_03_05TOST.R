#' Export datasets to Excel for DESC_03
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @return Sheet(s) in tabular output Excel file in OUTPUT_FOLDER
#'
#' @import stringr

# BESD_DESC_03_05TOST R version 1.00 - Biostat Global Consulting - 2024-08-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-08  1.00      Caitlin Clary   Original R version adapted from v1.03 of
#                                       DESC_03_05TOST in vcqiR
# *******************************************************************************


BESD_DESC_03_05TOST <- function(VCP = "BESD_DESC_03_05TOST",
                                database_id = NA){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  if (str_to_upper(DESC_03_WEIGHTED) == "YES"){
    besd_global(DESC_03_TO_FOOTNOTE_1,
                language_string(language_use = language_use, str = "OS_344"))
                # Abbreviations: CI=Confidence Interval

    besd_global(DESC_03_TO_FOOTNOTE_2,
                language_string(language_use = language_use, str = "OS_248"))
                # Respondents could select more than one response to this question.

    besd_global(DESC_03_TO_FOOTNOTE_3,
                language_string(language_use = language_use, str = "OS_51"))
                # Note: This measure is a population estimate that incorporates survey weights.
                # The CI is calculated with software that take the complex survey design into account.
  }

  if (str_to_upper(DESC_03_WEIGHTED) == "NO"){
    besd_global(DESC_03_TO_FOOTNOTE_1,
                language_string(language_use = language_use, str = "OS_52"))
                # Note: This measure is an unweighted summary of proportions from the survey sample.

    besd_global(DESC_03_TO_FOOTNOTE_2,
                language_string(language_use = language_use, str = "OS_248"))
                # Respondents could select more than one response to this question.

    if (str_to_upper(DESC_03_DENOMINATOR) == "ALL"){
      besd_global(DESC_03_TO_FOOTNOTE_3,
                  language_string(language_use = language_use, str = "OS_16"))
                  # Denominator (N) is the total number of respondents.
    }
    if (str_to_upper(DESC_03_DENOMINATOR) == "RESPONDED"){
      besd_global(DESC_03_TO_FOOTNOTE_3,
                  language_string(language_use = language_use, str = "OS_15"))
                  # Denominator (N) is limited to respondents who answered the question.
    }
  }

  vid <- DESC_03_COUNTER

  set_back_to_blank <- 0
  if (!besd_object_exists("DESC_03_TO_TITLE")){set_back_to_blank <- 1}

  if (is.na(database_id)){
    zpc <- DESC_03_COUNTER
    if (DESC_03_COUNTER < 10){
      zpc <- paste0("0", zpc) # zero pad the desc_03 counter
    }
  }

  rm(list = c("TO_DESC_03", "TO_DESC_03_columnlabel",
              "TO_DESC_03_formatnum", "TO_DESC_03_colformat"), envir = .GlobalEnv) %>%
    suppressWarnings()

  # dat <- besd_read(
  #   paste0(OUTPUT_FOLDER, "/DESC_03_", ANALYSIS_COUNTER, "_", zpc, "_", vid, "_database.rds"))

  if (is.na(database_id)){
    dat <- besd_read(paste0(OUTPUT_FOLDER, "/DESC_03_", ANALYSIS_COUNTER, "_",
                            zpc, "_", vid, "_database.rds"))

  } else {
    dat <- besd_read(paste0(OUTPUT_FOLDER, "/DESC_03_", ANALYSIS_COUNTER, "_",
                            database_id, "_", vid, "_database.rds"))
  }

  # If table title isn't specified, use variable label (default set in GO)
  if (set_back_to_blank %in% 1){
    if (!is.null(attributes(dat$outcome)$label)){
      besd_global(DESC_03_TO_TITLE, attributes(dat$outcome)$label)
    }
  }

  if ("nwtd" %in% names(dat)){
    wtd <- 1
  } else {
    wtd <- 0
  }

  # We need to tweak the database first
  for (i in seq_along(DESC_03_VORDER)) {
    # datname <- paste0(OUTPUT_FOLDER, "/DESC_03_", ANALYSIS_COUNTER, "_", zpc,
    #                   "_", vid, "_database.rds")
    #
    # db <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", zpc, "_", vid, "_database")

    if (is.na(database_id)){
      datname <- paste0(OUTPUT_FOLDER, "/DESC_03_", ANALYSIS_COUNTER, "_",
                        zpc, "_", vid, "_database.rds")

      db <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", zpc, "_", vid, "_database")
    } else {
      datname <- paste0(OUTPUT_FOLDER, "/DESC_03_", ANALYSIS_COUNTER, "_",
                        database_id, "_", vid, "_database.rds")

      db <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", database_id, "_", vid, "_database")
    }

    format_desc0203_database(
      i = DESC_03_VORDER[i], db = db,
      wtd = wtd, indicator = "DESC_03", datname = datname, vid = vid)
  } # end of DESC_03_VORDER i loop

  for (i in seq_along(DESC_03_VORDER)) {

    if (is.na(database_id)){
      tempname <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", zpc, "_",
                         vid, "_database_", DESC_03_VORDER[i], ".rds")
    } else {
      tempname <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", database_id, "_",
                         vid, "_database_", DESC_03_VORDER[i], ".rds")
    }

    if (besd_object_exists("DESC_03_LIST_N_BEFORE_PCT")){
      if (str_to_upper(DESC_03_LIST_N_BEFORE_PCT) == "YES"){
        ni <- get(paste0("n", DESC_03_VORDER[i]), envir = .GlobalEnv)
        make_table_column(
          tablename = "TO_DESC_03",
          dbfilename = tempname,
          variable = "n", replacevar = NA, noannotate = TRUE,
          label = ni)
      }
    }

    if (besd_object_exists("DESC_03_LIST_NWTD_BEFORE_PCT")){
      if (wtd == 1 & str_to_upper(DESC_03_LIST_NWTD_BEFORE_PCT) == "YES"){
        nwtdi <- get(paste0("nwtd", DESC_03_VORDER[i]), envir = .GlobalEnv)
        make_table_column(
          tablename = "TO_DESC_03",
          dbfilename = tempname,
          variable = "nwtd", replacevar = NA, noannotate = TRUE,
          label = nwtdi)
      }
    }

    pcti <- get(paste0("pct", DESC_03_VORDER[i]), envir = .GlobalEnv)
    make_table_column(
      tablename = "TO_DESC_03",
      dbfilename = tempname,
      variable = "estimate", replacevar = NA, noannotate = TRUE,
      label = paste0(pcti, " ", language_string(language_use = language_use, str = "OS_1"))) #(%)

    if (wtd == 1 & suppress_cis != 1){
      make_table_column(
        tablename = "TO_DESC_03",
        dbfilename = tempname,
        variable = "ci", replacevar = NA, noannotate = TRUE,
        label = language_string(language_use = language_use, str = "OS_4")) #95% CI (%)
    }

    file.remove(paste0(OUTPUT_FOLDER, "/", tempname))

  } # end of DESC_03_VORDER i loop

  # Now we want to do it overall

  if (is.na(database_id)){
    tempname <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", zpc, "_", vid, "_database.rds")
  } else {
    tempname <- paste0("DESC_03_", ANALYSIS_COUNTER, "_", database_id, "_", vid, "_database.rds")
  }

  make_table_column(
    tablename = "TO_DESC_03",
    dbfilename = tempname,
    variable = "n", replacevar = NA, noannotate = TRUE, label = DESC_03_N_LABEL)

  if (wtd == 1){
    make_table_column(
      tablename = "TO_DESC_03",
      dbfilename = tempname,
      variable = "nwtd", replacevar = NA, noannotate = TRUE, label = DESC_03_NWTD_LABEL)
  }

  sheetname <- paste0("DESC_03_", DESC_03_COUNTER, "_", DESC_03_SHORT_TITLE)
  sheet1 <- paste0(sheetname, " ", ANALYSIS_COUNTER)
  if(str_count(sheet1) > 31){
    e <- 31-str_count(paste0(" ", ANALYSIS_COUNTER))
    sheetname <- substr(sheetname, 1, e)
  }

  export_table_to_excel(indicator = "DESC_03", sheet = sheetname, brief = FALSE)

  if (MAKE_TEMPLATE_REPORT %in% 1){

    BESDTI_REPORT_INPUTS <- get("BESDTI_REPORT_INPUTS", envir = .GlobalEnv)

    getnotes <- TRUE
    tempnotes <- NULL
    i <- 1
    while(getnotes == TRUE){
      if (besd_object_exists(paste0("DESC_03_TO_FOOTNOTE_", i))){
        tempnotes <- c(tempnotes, get(paste0("DESC_03_TO_FOOTNOTE_", i), envir = .GlobalEnv))
        i <- i + 1
      } else {
        getnotes <- FALSE
      }
    } # end while

    if (!is.null(tempnotes)){
      tempnotes <- paste(tempnotes, collapse = "  \n")
    } else {
      tempnotes <- NA
    }

    BESDTI_REPORT_INPUTS$notes[nrow(BESDTI_REPORT_INPUTS)] <- tempnotes

    assign("BESDTI_REPORT_INPUTS", BESDTI_REPORT_INPUTS, envir = .GlobalEnv)

    RT_DESC_03 <- TO_DESC_03

    colnames <- get(paste0("TO_DESC_03", "_columnlabel"), envir = .GlobalEnv)

    # TO DO eventually: add if () override to exclude this indicator

    if (!dir.exists(paste0(OUTPUT_FOLDER, "/Report Inputs"))){
      dir.create(paste0(OUTPUT_FOLDER, "/Report Inputs"))
    }

    if (is.na(database_id)){
      rt_db_name <- paste0(OUTPUT_FOLDER, "/Report Inputs/DESC_03_", ANALYSIS_COUNTER, "_",
                           zpc, "_", vid, "_rt.rds")

    } else {
      rt_db_name <- paste0(OUTPUT_FOLDER, "/Report Inputs/DESC_03_", ANALYSIS_COUNTER, "_",
                           database_id, "_", vid, "_rt.rds")
    }

    RT_DESC_03 <- mutate(RT_DESC_03,
                         title = DESC_03_TO_TITLE,
                         variable = paste(DESC_03_VARIABLES, collapse = ", "),
                         database_id = database_id,
                         analysis_counter = ANALYSIS_COUNTER) %>%
      left_join(., select(output_layout, order, label, rowtype), by = c("name" = "label"))

    # Rename columns after mutating and joining, in case of duplicate colnames
    names(RT_DESC_03) <- c("outputid", "name", colnames,
                           "title", "variable", "database_id", "analysis_counter",
                           "order", "rowtype")

    saveRDS(RT_DESC_03, rt_db_name)
    # besd_global(REPORT_INDICATOR_LIST, c(REPORT_INDICATOR_LIST, rt_db_name))
    # besd_global(REPORT_INDICATOR_TITLE_LIST, c(REPORT_INDICATOR_TITLE_LIST, DESC_03_TO_TITLE))
  }

  rm(list = c("TO_DESC_03", "TO_DESC_03_columnlabel", "TO_DESC_03_formatnum",
              "TO_DESC_03_colformat"), envir = .GlobalEnv) %>%
    suppressWarnings()

  rm(list = c(paste0("DESC_03_labels_", DESC_03_COUNTER)),
     envir = .GlobalEnv) %>%
    suppressWarnings()

  # If title global was blank, we used variable labels or names - return global
  # to blank before exiting
  if (set_back_to_blank %in% 1){
    besd_global(DESC_03_TO_TITLE, NA)
  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
