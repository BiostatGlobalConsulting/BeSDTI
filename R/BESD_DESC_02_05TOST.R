#' Export datasets to Excel for BESD_DESC_02
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param database_id Tag for file names
#'
#' @return Sheet(s) in tabular output Excel file in OUTPUT_FOLDER
#'
#' @import stringr

# BESD_DESC_02_05TOST R version 1.00 - Biostat Global Consulting - 2024-08-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-08  1.00      Caitlin Clary   Original R version adapted from v1.03 of
#                                       DESC_02_05TOST in vcqiR
# *******************************************************************************


BESD_DESC_02_05TOST <- function(VCP = "BESD_DESC_02_05TOST",
                                database_id = NA){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  if (str_to_upper(DESC_02_WEIGHTED) == "YES"){
    besd_global(DESC_02_TO_FOOTNOTE_1,
                language_string(language_use = language_use, str = "OS_344"))
                # Abbreviations: CI=Confidence Interval

    besd_global(DESC_02_TO_FOOTNOTE_2,
                language_string(language_use = language_use, str = "OS_247"))
                # Respondents could only select one response to this question.

    besd_global(DESC_02_TO_FOOTNOTE_3,
                language_string(language_use = language_use, str = "OS_51"))
                # Note: This measure is a population estimate that incorporates survey weights.,
                # The CI is calculated with software that take the complex survey design into account.
  }

  if (str_to_upper(DESC_02_WEIGHTED) == "NO"){
    besd_global(DESC_02_TO_FOOTNOTE_1,
                language_string(language_use = language_use, str = "OS_52"))
                # Note: This measure is an unweighted summary of proportions from the survey sample.

    besd_global(DESC_02_TO_FOOTNOTE_2,
                language_string(language_use = language_use, str = "OS_247"))
                # Respondents could only select one response to this question.

    if (str_to_upper(DESC_02_DENOMINATOR) == "ALL"){
      besd_global(DESC_02_TO_FOOTNOTE_3,
                  language_string(language_use = language_use, str = "OS_16"))
                  # Denominator (N) is the total number of respondents.
    }

    if (str_to_upper(DESC_02_DENOMINATOR) == "RESPONDED"){
      besd_global(DESC_02_TO_FOOTNOTE_3,
                  language_string(language_use = language_use, str = "OS_15"))
                  # Denominator (N) is limited to respondents who answered the question.
    }
  }

  vid <- 1

  if (is.na(database_id)){
    zpc <- DESC_02_COUNTER
    if (DESC_02_COUNTER < 10){
      zpc <- paste0("0", zpc) # zero pad the desc_02 counter
    }
  }

  for (d in seq_along(DESC_02_VARIABLES)){
    rm(list = c("TO_DESC_02", "TO_DESC_02_columnlabel",
                "TO_DESC_02_formatnum", "TO_DESC_02_colformat"),
       envir = .GlobalEnv) %>% suppressWarnings()

    if (is.na(database_id)){
      dat <- besd_read(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_",
                              zpc, "_", vid, "_database.rds"))

    } else {
      dat <- besd_read(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_",
                              database_id, "_", vid, "_database.rds"))
    }



    if ("nwtd" %in% names(dat)){
      wtd <- 1
    } else {
      wtd <- 0
    }

    # We need to tweak the database first
    for (i in seq_along(DESC_02_VORDER)) {

      if (is.na(database_id)){
        datname <- paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_",
                          zpc, "_", vid, "_database.rds")

        db <- paste0("DESC_02_", ANALYSIS_COUNTER, "_", zpc, "_", vid, "_database")
      } else {
        datname <- paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_",
                          database_id, "_", vid, "_database.rds")

        db <- paste0("DESC_02_", ANALYSIS_COUNTER, "_", database_id, "_", vid, "_database")
      }


			format_desc0203_database(
			  i = DESC_02_VORDER[i], db = db,
			  wtd = wtd, indicator = "DESC_02", datname = datname, vid = vid)
    } # end of DESC_02_VORDER i loop

    for (i in seq_along(DESC_02_VORDER)) {

      if (is.na(database_id)){
        tempname <- paste0("DESC_02_", ANALYSIS_COUNTER, "_", zpc, "_",
                           vid, "_database_", DESC_02_VORDER[i], ".rds")
      } else {
        tempname <- paste0("DESC_02_", ANALYSIS_COUNTER, "_", database_id, "_",
                           vid, "_database_", DESC_02_VORDER[i], ".rds")
      }

      if (besd_object_exists("DESC_02_LIST_N_BEFORE_PCT")){
        if (str_to_upper(DESC_02_LIST_N_BEFORE_PCT) == "YES"){
          ni <- get(paste0("n", DESC_02_VORDER[i]), envir = .GlobalEnv)
          make_table_column(
            tablename = "TO_DESC_02",
            # dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER, "_", zpc, "_",
            #                     vid, "_database_", DESC_02_VORDER[i], ".rds"),
            dbfilename = tempname,
            variable = "n", replacevar = NA, noannotate = TRUE,
            label = ni)
        }
      }

      if (besd_object_exists("DESC_02_LIST_NWTD_BEFORE_PCT")){
        if (wtd == 1 & str_to_upper(DESC_02_LIST_NWTD_BEFORE_PCT) == "YES"){
          nwtdi <- get(paste0("nwtd", DESC_02_VORDER[i]), envir = .GlobalEnv)
          make_table_column(
            tablename = "TO_DESC_02",
            # dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER, "_", zpc, "_",
            #                     vid, "_database_", DESC_02_VORDER[i], ".rds"),
            dbfilename = tempname,
            variable = "nwtd", replacevar = NA, noannotate = TRUE,
            label = nwtdi)
        }
      }

      pcti <- get(paste0("pct",DESC_02_VORDER[i]), envir = .GlobalEnv)
      make_table_column(
        tablename = "TO_DESC_02",
        # dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc, "_",
        #                     vid, "_database_", DESC_02_VORDER[i] ,".rds"),
        dbfilename = tempname,
        variable = "estimate", replacevar = NA, noannotate = TRUE,
        label = paste0(pcti, " ", language_string(language_use = language_use, str = "OS_1"))) #(%)

      if (wtd == 1 & suppress_cis != 1){
        make_table_column(
          tablename = "TO_DESC_02",
          # dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc, "_",
          #                     vid, "_database_", DESC_02_VORDER[i], ".rds"),
          dbfilename = tempname,
          variable = "ci", replacevar = NA, noannotate = TRUE,
          label = language_string(language_use = language_use, str = "OS_4")) # 95% CI (%)
      }

      # file.remove(
      #   paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_", zpc,
      #          "_", vid, "_database_", DESC_02_VORDER[i], ".rds"))

      file.remove(paste0(OUTPUT_FOLDER, "/", tempname))

    } # end of DESC_02_VORDER i loop

    # Now we want to do it overall

    if (is.na(database_id)){
      tempname <- paste0("DESC_02_", ANALYSIS_COUNTER, "_", zpc, "_", vid, "_database.rds")
    } else {
      tempname <- paste0("DESC_02_", ANALYSIS_COUNTER, "_", database_id, "_", vid, "_database.rds")
    }

    make_table_column(
      tablename = "TO_DESC_02",
      # dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER, "_", zpc,"_",vid, "_database.rds"),
      dbfilename = tempname,
      variable = "n", replacevar = NA, noannotate = TRUE, label = DESC_02_N_LABEL)

    if (wtd == 1){
      make_table_column(
        tablename = "TO_DESC_02",
        # dbfilename = paste0("DESC_02_", ANALYSIS_COUNTER,"_", zpc, "_", vid, "_database.rds"),
        dbfilename = tempname,
        variable = "nwtd", replacevar = NA, noannotate = TRUE, label = DESC_02_NWTD_LABEL)
    }

    sheetname <- paste0("DESC_02_", DESC_02_COUNTER, "_", DESC_02_VARIABLES[d])
    sheet1 <- paste0(sheetname, " ", ANALYSIS_COUNTER)
    if (str_count(sheet1) > 31){
      e <- 31-str_count(paste0(" ", ANALYSIS_COUNTER))
      sheetname <- substr(sheetname, 1, e)
    }

    export_table_to_excel(indicator = "DESC_02", sheet = sheetname, brief = FALSE)

    # TESTING - saving TO_DESC_02 to report inputs folder

    if (MAKE_TEMPLATE_REPORT %in% 1){

      RT_DESC_02 <- TO_DESC_02

      colnames <- get(paste0("TO_DESC_02", "_columnlabel"), envir = .GlobalEnv)

      # TO DO eventually: add if () override to exclude this indicator

      if (!dir.exists(paste0(OUTPUT_FOLDER, "/Report Inputs"))){
        dir.create(paste0(OUTPUT_FOLDER, "/Report Inputs"))
      }

      if (is.na(database_id)){
        rt_db_name <- paste0(OUTPUT_FOLDER, "/Report Inputs/DESC_02_", ANALYSIS_COUNTER, "_",
                             zpc, "_", vid, "_rt.rds")

      } else {
        rt_db_name <- paste0(OUTPUT_FOLDER, "/Report Inputs/DESC_02_", ANALYSIS_COUNTER, "_",
                             database_id, "_", vid, "_rt.rds")
      }

      RT_DESC_02 <- mutate(RT_DESC_02,
                           title = DESC_02_TO_TITLE,
                           variable = paste(DESC_02_VARIABLES, collapse = ", "),
                           database_id = database_id,
                           analysis_counter = ANALYSIS_COUNTER) %>%
        left_join(., select(output_layout, order, label, rowtype), by = c("name" = "label"))

      # Rename columns after mutating and joining, in case of duplicate colnames
      names(RT_DESC_02) <- c("outputid", "name", colnames,
                             "title", "variable", "database_id", "analysis_counter",
                             "order", "rowtype")

      saveRDS(RT_DESC_02, rt_db_name)
      # besd_global(REPORT_INDICATOR_LIST, c(REPORT_INDICATOR_LIST, rt_db_name))
      # besd_global(REPORT_INDICATOR_TITLE_LIST, c(REPORT_INDICATOR_TITLE_LIST, DESC_02_TO_TITLE))
    }

    vid = vid + 1

    rm(list = c(
      "TO_DESC_02", "TO_DESC_02_columnlabel",
      "TO_DESC_02_formatnum", "TO_DESC_02_colformat"),
      envir = .GlobalEnv) %>% suppressWarnings()

  } # end of DESC_02_VARIABLES d loop

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
