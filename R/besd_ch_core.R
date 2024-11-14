#' Calculate core BeSD indicators for a child vaccination analysis
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#' @import ggplot2
#'
#' @return Log entries; errors if conditions not met
#'
#' @export
#'
#' @examples
#' besd_ch_core()

# besd_ch_core R version 1.00 - Biostat Global Consulting - 2024-07-23

# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-23  1.00      Caitlin Clary   Original R version
# *******************************************************************************

# TO DO - use relabel options to bring in multilingual strings??
# TO DO - add logic re: generating plots, tables

besd_ch_core <- function(VCP = "besd_ch_core",
                         cleanup = TRUE){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  # Check globals ----

  exitflag <- 0
  errormsgs <- NULL

  ch_data_file <- "CH_dv.rds"

  if (!file.exists(paste0(OUTPUT_FOLDER, "/", ch_data_file))){
    errormsgs <- c(
      errormsgs,
      paste0(paste0(OUTPUT_FOLDER, "/", ch_data_file), " does not exist. Did you run gen_ch_dv?"))

    besd_log_comment(
      VCP, 1, "Error",
      paste0(paste0(OUTPUT_FOLDER, "/", ch_data_file), " does not exist. Did you run gen_ch_dv?"))

    exitflag <- 1
  }

  if (!besd_object_exists("BESD_CORE_WEIGHTED")){
    errormsgs <- c(
      errormsgs,
      "BESD_CORE_WEIGHTED must be specified."
    )

    besd_log_comment(
      VCP, 1, "Error",
      "BESD_CORE_WEIGHTED must be specified.")

    exitflag <- 1
  }

  if (!besd_object_exists("BESD_CORE_DENOMINATOR")){
    errormsgs <- c(
      errormsgs,
      "BESD_CORE_DENOMINATOR must be specified."
    )

    besd_log_comment(
      VCP, 1, "Error",
      "BESD_CORE_DENOMINATOR must be specified.")

    exitflag <- 1
  }

  if (besd_object_exists("BESD_CORE_DENOMINATOR") &
      besd_object_exists("BESD_CORE_WEIGHTED")){

    # Confirm BESD_CORE_DENOMINATOR is valid - if BESD_CORE_WEIGHTED is YES,
    # then BESD_CORE_DENOMINATOR must be ALL
    if (str_to_upper(BESD_CORE_WEIGHTED) %in% "YES" &
        !str_to_upper(BESD_CORE_DENOMINATOR) %in% "ALL"){

      errormsgs <- c(
        errormsgs,
        paste0("BESD_CORE_DENOMINATOR must be ALL if BESD_CORE_WEIGHTED is YES. The current value is ",
               BESD_CORE_DENOMINATOR))

      besd_log_comment(
        VCP, 1, "Error",
        paste0("BESD_CORE_DENOMINATOR must be ALL if BESD_CORE_WEIGHTED is YES. The current value is ",
               BESD_CORE_DENOMINATOR))
      exitflag <- 1

    } else {

      # If BESD_CORE_WEIGHTED is NO, BESD_CORE_DENOMINATOR can be ALL or RESPONDED
      if (!str_to_upper(BESD_CORE_DENOMINATOR) %in% c("ALL", "RESPONDED")){

        errormsgs <- c(
          errormsgs,
          paste0("BESD_CORE_DENOMINATOR must be ALL or RESPONDED. The current value is ",
                 BESD_CORE_DENOMINATOR))

        besd_log_comment(
          VCP, 1, "Error",
          paste0("BESD_CORE_DENOMINATOR must be ALL or RESPONDED.The current value is ",
                 BESD_CORE_DENOMINATOR))
        exitflag <- 1
      }

      # Gently remind the user that missing values are not tabulated when the denominator is RESPONDED
      if (str_to_upper(BESD_CORE_DENOMINATOR) %in% "RESPONDED"){
        besd_log_comment(
          VCP, 3, "Comment",
          "BESD_CORE_DENOMINATOR is RESPONDED so missing values will not be tabulated.")
      }
    } # End else()
  }

  if (!besd_object_exists("BESD_CORE_TABLE_STRUCTURE")){
    errormsgs <- c(
      errormsgs,
      "BESD_CORE_TABLE_STRUCTURE must be specified."
    )

    besd_log_comment(
      VCP, 1, "Error",
      "BESD_CORE_TABLE_STRUCTURE must be specified.")

    exitflag <- 1
  } else {
    BESD_CORE_TABLE_STRUCTURE <- as.numeric(BESD_CORE_TABLE_STRUCTURE)

    if (!BESD_CORE_TABLE_STRUCTURE %in% c(1, 2, 3)){
      errormsgs <- c(
        errormsgs,
        paste0("BESD_CORE_TABLE_STRUCTURE must be 1, 2, or 3. The current value is ",
               BESD_CORE_TABLE_STRUCTURE))

      besd_log_comment(
        VCP, 1, "Error",
        paste0("BESD_CORE_TABLE_STRUCTURE must be 1, 2, or 3. The current value is ",
               BESD_CORE_TABLE_STRUCTURE))

      exitflag <- 1
    }
  }

  # if (!besd_object_exists("BESD_CORE_PLOT_STRUCTURE")){
  #   errormsgs <- c(
  #     errormsgs,
  #     "BESD_CORE_PLOT_STRUCTURE must be specified."
  #   )
  #
  #   besd_log_comment(
  #     VCP, 1, "Error",
  #     "BESD_CORE_PLOT_STRUCTURE must be specified.")
  #
  #   exitflag <- 1
  # } else {
  #   BESD_CORE_PLOT_STRUCTURE <- as.numeric(BESD_CORE_PLOT_STRUCTURE)
  #
  #   if (!BESD_CORE_PLOT_STRUCTURE %in% c(1, 2)){
  #     errormsgs <- c(
  #       errormsgs,
  #       paste0("BESD_CORE_PLOT_STRUCTURE must be 1 or 2. The current value is ",
  #              BESD_CORE_PLOT_STRUCTURE))
  #
  #     besd_log_comment(
  #       VCP, 1, "Error",
  #       paste0("BESD_CORE_PLOT_STRUCTURE must be 1 or 2. The current value is ",
  #              BESD_CORE_PLOT_STRUCTURE))
  #
  #     exitflag <- 1
  #   }
  # }

  # TO DO - consider if multiple stratifiers should be supported here. Perhaps
  # one plot per stratifier??

  # Clean up core plot stratifier object
  if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){
    BESD_CORE_PLOT_STRATIFIER <- BESD_CORE_PLOT_STRATIFIER[!is.na(BESD_CORE_PLOT_STRATIFIER) & !is.null(BESD_CORE_PLOT_STRATIFIER) & !BESD_CORE_PLOT_STRATIFIER == ""]
  }

  if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){

    if (length(BESD_CORE_PLOT_STRATIFIER) > 1){
      errormsgs <- c(
        errormsgs,
        "Only one variable can be specified as BESD_CORE_PLOT_STRATIFIER")

      besd_log_comment(
        VCP, 1, "Error",
        "Only one variable can be specified as BESD_CORE_PLOT_STRATIFIER")

      exitflag <- 1
    } else {

      if (file.exists(paste0(OUTPUT_FOLDER, "/", ch_data_file))){
        tempdat <- readRDS(paste0(OUTPUT_FOLDER, "/", ch_data_file))

        for (g in seq_along(BESD_CORE_PLOT_STRATIFIER)){
          if (!BESD_CORE_PLOT_STRATIFIER[g] %in% names(tempdat)){
            errormsgs <- c(
              errormsgs,
              paste0("The variable ", BESD_CORE_PLOT_STRATIFIER[g],
                     " provided in global macro BESD_CORE_PLOT_STRATIFIER does not exist ",
                     "in the dataset."))

            besd_log_comment(
              VCP, 1, "Error",
              paste0("The variable ", BESD_CORE_PLOT_STRATIFIER[g],
                     " provided in global macro BESD_CORE_PLOT_STRATIFIER does not exist ",
                     "in the dataset."))
            exitflag <- 1
          }
        } # End of BESD_CORE_PLOT_STRATIFIER g loop
      } # End if ch_data_file exists
    }
  } # End core plot stratifier check

  if (besd_object_exists("BESD_CORE_PLOT_COLORS")){

    if (any(besd_check_color(BESD_CORE_PLOT_COLORS) == FALSE)){

      problem_colors <- paste(
        BESD_CORE_PLOT_COLORS[besd_check_color(BESD_CORE_PLOT_COLORS) == FALSE],
        collapse = ", ")

      errormsgs <- c(
        errormsgs,
        paste0("One or more colors specified in BESD_CORE_PLOT_COLORS is not valid. ",
               "The invalid value(s) are: ", problem_colors, "."))

      besd_log_comment(
        VCP, 1, "Error",
        paste0("One or more colors specified in BESD_CORE_PLOT_COLORS is not valid. ",
               "The invalid value(s) are: ", problem_colors, "."))

      exitflag <- 1
    }

    # TO DO add check that length = 1 when stratifier is undefined OR length =
    # number of unique (non-NA??) values in stratifier

  } # End color check

  if (exitflag == 1){
    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Calculate indicators -----

  # BESD_CORE_TABLE_STRUCTURE
  # 1 = show dichotomized responses in tables
  # 2 = show all response options to core indicators in tables
  # 3 = show all response options *with subtotals* for dichotomized categories

  ## Intent indicator ----
  besd_global(DESC_02_DATASET, ch_data_file)
  besd_global(DESC_02_WEIGHTED, BESD_CORE_WEIGHTED)
  besd_global(DESC_02_DENOMINATOR, BESD_CORE_DENOMINATOR)

  besd_global(DESC_02_VARIABLES, "CHI_intent")

  # Dichotomized or with subtotals: set subtotal options
  if (BESD_CORE_TABLE_STRUCTURE %in% c(1, 3)){
    besd_global(DESC_02_N_SUBTOTALS, 2)

    besd_global(DESC_02_SUBTOTAL_LEVELS_1, c(1, 2))
    besd_global(DESC_02_SUBTOTAL_LEVELS_2, 3)
  }

  # Dichotomized: show subtotals only
  if (BESD_CORE_TABLE_STRUCTURE %in% 1){
    besd_global(DESC_02_SUBTOTAL_LABEL_1, language_string(
      language_use = language_use, str = "OS_B32")) # "None or some of vaccines in schedule"
    besd_global(DESC_02_SUBTOTAL_LABEL_2, language_string(
      language_use = language_use, str = "OS_B33")) # "All vaccines in schedule"

    besd_global(DESC_02_SHOW_SUBTOTALS_ONLY, "yes")
  }

  # Subtotals: set placement of subtotal columns
  if (BESD_CORE_TABLE_STRUCTURE %in% 3){
    # besd_global(DESC_02_SUBTOTAL_LABEL_1, "Subtotal: None or some of vaccines in schedule")
    # besd_global(DESC_02_SUBTOTAL_LABEL_2, "Subtotal: All vaccines in schedule")

    besd_global(DESC_02_SUBTOTAL_LABEL_1, paste0(
      # "Subtotal: "
      language_string(language_use = language_use,
                      str = "OS_B31"), ": ",
      # "None or some of vaccines in schedule"
      language_string(language_use = language_use,
                      str = "OS_B32")))

    besd_global(DESC_02_SUBTOTAL_LABEL_2, paste0(
      # "Subtotal: "
      language_string(language_use = language_use,
                      str = "OS_B31"), ": ",
      # "All vaccines in schedule"
      language_string(language_use = language_use,
                      str = "OS_B33")))

    besd_global(DESC_02_SUBTOTAL_LIST_1, "after 2")
    besd_global(DESC_02_SUBTOTAL_LIST_2, "after 3")
  }

  besd_global(DESC_02_TO_TITLE,
              language_string(language_use = language_use, str = "OS_B6"))
  besd_global(DESC_02_TO_SUBTITLE, NA)
  # Footnotes?

  BESD_DESC_02(cleanup = TRUE, database_id = "intent")

  ## ConfB indicator ----
  besd_global(DESC_02_DATASET, ch_data_file)
  besd_global(DESC_02_WEIGHTED, BESD_CORE_WEIGHTED)
  besd_global(DESC_02_DENOMINATOR, BESD_CORE_DENOMINATOR)

  besd_global(DESC_02_VARIABLES, "CHI_confb")

  # Dichotomized or with subtotals: set subtotal options
  if (BESD_CORE_TABLE_STRUCTURE %in% c(1, 3)){
    besd_global(DESC_02_N_SUBTOTALS, 2)

    besd_global(DESC_02_SUBTOTAL_LEVELS_1, c(1, 2))
    besd_global(DESC_02_SUBTOTAL_LEVELS_2, c(3, 4))
  }

  # Dichotomized: show subtotals only
  if (BESD_CORE_TABLE_STRUCTURE %in% 1){
    # "Not at all or a little important"
    besd_global(DESC_02_SUBTOTAL_LABEL_1,
                language_string(language_use = language_use,
                                str = "OS_B34"))
    # "Moderately or very important"
    besd_global(DESC_02_SUBTOTAL_LABEL_2,
                language_string(language_use = language_use,
                                str = "OS_B35"))

    besd_global(DESC_02_SHOW_SUBTOTALS_ONLY, "yes")
  }

  # Subtotals: set placement of subtotal columns
  if (BESD_CORE_TABLE_STRUCTURE %in% 3){

    # besd_global(DESC_02_SUBTOTAL_LABEL_1, "Subtotal: Not at all or a little important")
    # besd_global(DESC_02_SUBTOTAL_LABEL_2, "Subtotal: Moderately or very important")

    besd_global(DESC_02_SUBTOTAL_LABEL_1,
                paste0(
                  # "Subtotal: "
                  language_string(language_use = language_use,
                                  str = "OS_B31"), ": ",
                  # "Not at all or a little important"
                  language_string(language_use = language_use,
                                str = "OS_B34")))

    besd_global(DESC_02_SUBTOTAL_LABEL_2,
                paste0(
                  # "Subtotal: "
                  language_string(language_use = language_use,
                                  str = "OS_B31"), ": ",
                  # "Moderately or very important"
                  language_string(language_use = language_use,
                                str = "OS_B35")))

    besd_global(DESC_02_SUBTOTAL_LIST_1, "after 2")
    besd_global(DESC_02_SUBTOTAL_LIST_2, "after 4")
  }

  besd_global(DESC_02_TO_TITLE,
              language_string(language_use = language_use, str = "OS_B7"))
  besd_global(DESC_02_TO_SUBTITLE, NA)
  # Footnotes?

  BESD_DESC_02(cleanup = TRUE, database_id = "confb")

  ## NormF indicator ----
  # NOTE - there are only two responses to this question, so
  # BESD_CORE_TABLE_STRUCTURE does not apply here

  besd_global(DESC_02_DATASET, ch_data_file)
  besd_global(DESC_02_WEIGHTED, BESD_CORE_WEIGHTED)
  besd_global(DESC_02_DENOMINATOR, BESD_CORE_DENOMINATOR)

  besd_global(DESC_02_VARIABLES, "CHI_normf")

  besd_global(DESC_02_TO_TITLE,
              language_string(language_use = language_use, str = "OS_B8"))
  besd_global(DESC_02_TO_SUBTITLE, NA)
  # Footnotes?

  BESD_DESC_02(cleanup = TRUE, database_id = "normf")

  ## Where indicator ----
  # NOTE - there are only two responses to this question, so
  # BESD_CORE_TABLE_STRUCTURE does not apply here

  besd_global(DESC_02_DATASET, ch_data_file)
  besd_global(DESC_02_WEIGHTED, BESD_CORE_WEIGHTED)
  besd_global(DESC_02_DENOMINATOR, BESD_CORE_DENOMINATOR)

  besd_global(DESC_02_VARIABLES, "CHI_where")
  besd_global(DESC_02_TO_TITLE,
              language_string(language_use = language_use, str = "OS_B9"))
  besd_global(DESC_02_TO_SUBTITLE, NA)
  # Footnotes?

  BESD_DESC_02(cleanup = TRUE, database_id = "where")

  ## Afford indicator ----
  besd_global(DESC_02_DATASET, ch_data_file)
  besd_global(DESC_02_WEIGHTED, BESD_CORE_WEIGHTED)
  besd_global(DESC_02_DENOMINATOR, BESD_CORE_DENOMINATOR)

  besd_global(DESC_02_VARIABLES, "CHI_afford")

  # Dichotomized or with subtotals: set subtotal options
  if (BESD_CORE_TABLE_STRUCTURE %in% c(1, 3)){
    besd_global(DESC_02_N_SUBTOTALS, 2)

    besd_global(DESC_02_SUBTOTAL_LEVELS_1, c(1, 2))
    besd_global(DESC_02_SUBTOTAL_LEVELS_2, c(3, 4))


  }

  # Dichotomized: show subtotals only
  if (BESD_CORE_TABLE_STRUCTURE %in% 1){
    # "Not at all or a little easy"
    besd_global(DESC_02_SUBTOTAL_LABEL_1,
                language_string(language_use = language_use,
                                str = "OS_B36"))
    # "Moderately or very easy"
    besd_global(DESC_02_SUBTOTAL_LABEL_2,
                language_string(language_use = language_use,
                                str = "OS_B37"))

    besd_global(DESC_02_SHOW_SUBTOTALS_ONLY, "yes")
  }

  # Subtotals: set placement of subtotal columns
  if (BESD_CORE_TABLE_STRUCTURE %in% 3){

    besd_global(DESC_02_SUBTOTAL_LABEL_1,
                paste0(
                  # "Subtotal: "
                  language_string(language_use = language_use,
                                  str = "OS_B31"), ": ",
                  # "Not at all or a little easy"
                  language_string(language_use = language_use,
                                  str = "OS_B36")))

    besd_global(DESC_02_SUBTOTAL_LABEL_2,
                paste0(
                  # "Subtotal: "
                  language_string(language_use = language_use,
                                  str = "OS_B31"), ": ",
                  # "Moderately or very easy"
                  language_string(language_use = language_use,
                                  str = "OS_B37")))

    besd_global(DESC_02_SUBTOTAL_LIST_1, "after 2")
    besd_global(DESC_02_SUBTOTAL_LIST_2, "after 4")
  }


  besd_global(DESC_02_TO_TITLE,
              language_string(language_use = language_use, str = "OS_B10"))
  besd_global(DESC_02_TO_SUBTITLE, NA)
  # Footnotes?

  BESD_DESC_02(cleanup = TRUE, database_id = "afford")

  # Compile and save output database with all five indicators

  db_intent <- readRDS(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER,
                              "_", "intent", ".rds"))
  db_confb <- readRDS(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER,
                             "_", "confb", ".rds"))
  db_normf <- readRDS(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER,
                             "_", "normf", ".rds"))
  db_where <- readRDS(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER,
                             "_", "where", ".rds"))
  db_afford <- readRDS(paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER,
                              "_", "afford", ".rds"))

  # Rename and label intent variables
  db_intent <- db_intent %>%
    rename("CHI_intent_desc02_1" = "desc02_1_1", # none
           "CHI_intent_desc02_2" = "desc02_1_2", # some
           "CHI_intent_desc02_3" = "desc02_1_3"  # all
           ) %>%
    mutate(CHI_intent_binary = case_when(
      CHI_intent == 1 ~ 0,
      CHI_intent == 2 ~ 0,
      CHI_intent == 3 ~ 1,
      TRUE ~ NA
    ))

  db_intent$CHI_intent_binary <- haven::labelled(
    db_intent$CHI_intent_binary,
    label = language_string(language_use = language_use, str = "OS_B26"))

  # Rename and label confb variables
  db_confb <- db_confb %>%
    rename("CHI_confb_desc02_1" = "desc02_1_1", # not at all important
           "CHI_confb_desc02_2" = "desc02_1_2", # a little important
           "CHI_confb_desc02_3" = "desc02_1_3", # moderately important
           "CHI_confb_desc02_4" = "desc02_1_4"  # very important
    ) %>%
    mutate(CHI_confb_binary = case_when(
      CHI_confb == 1 ~ 0,
      CHI_confb == 2 ~ 0,
      CHI_confb == 3 ~ 1,
      CHI_confb == 4 ~ 1,
      TRUE ~ NA
    ))

  db_confb$CHI_confb_binary <- haven::labelled(
    db_confb$CHI_confb_binary,
    label = language_string(language_use = language_use, str = "OS_B27"))

  # Rename and label normf variables

  db_normf <- db_normf %>%
    rename("CHI_normf_desc02_1" = "desc02_1_1", # no
           "CHI_normf_desc02_2" = "desc02_1_2"  # yes
    ) %>%
    mutate(CHI_normf_binary = case_when(
      CHI_normf == 0 ~ 0,
      CHI_normf == 1 ~ 1,
      TRUE ~ NA
    ))

  db_normf$CHI_normf_binary <- haven::labelled(
    db_normf$CHI_normf_binary,
    label = language_string(language_use = language_use, str = "OS_B28"))

  # Rename and label where variables

  db_where <- db_where %>%
    rename("CHI_where_desc02_1" = "desc02_1_1", # no
           "CHI_where_desc02_2" = "desc02_1_2"  # yes
    ) %>%
    mutate(CHI_where_binary = case_when(
      CHI_where == 0 ~ 0,
      CHI_where == 1 ~ 1,
      TRUE ~ NA
    ))

  db_where$CHI_where_binary <- haven::labelled(
    db_where$CHI_where_binary,
    label = language_string(language_use = language_use, str = "OS_B29"))

  # Rename and label afford variables

  db_afford <- db_afford %>%
    rename("CHI_afford_desc02_1" = "desc02_1_1", # not at all easy
           "CHI_afford_desc02_2" = "desc02_1_2", # a little easy
           "CHI_afford_desc02_3" = "desc02_1_3", # moderately easy
           "CHI_afford_desc02_4" = "desc02_1_4"  # very easy
    ) %>%
    mutate(CHI_afford_binary = case_when(
      CHI_afford == 1 ~ 0,
      CHI_afford == 2 ~ 0,
      CHI_afford == 3 ~ 1,
      CHI_afford == 4 ~ 1,
      TRUE ~ NA
    ))

  db_afford$CHI_afford_binary <- haven::labelled(
    db_afford$CHI_afford_binary,
    label = language_string(language_use = language_use, str = "OS_B30"))

  db_out <- left_join(db_intent, db_confb) %>%
    left_join(., db_normf) %>%
    left_join(., db_where) %>%
    left_join(., db_afford)

  if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){
    vars <- c("respid", OUTPUT_VARLIST, BESD_CORE_PLOT_STRATIFIER)
  } else {
    vars <- c("respid", OUTPUT_VARLIST)
  }

  # TO DO join stratifiers?
  tempstrat <- readRDS(paste0(OUTPUT_FOLDER, "/", ch_data_file)) %>%
    select(
      all_of(vars)
    )

  db_out <- db_out %>% left_join(., tempstrat)

  saveRDS(db_out,
          paste0(OUTPUT_FOLDER, "/BESD_CH_CORE_", ANALYSIS_COUNTER, ".rds"))

  # TO DO add this dataset to TEMP_DATASETS

  # if (MAKE_BAR_PLOTS == 1){
    besd_core_plot(analysis = "child")
  # }

    # Add logic to include the core outputs in BESDTI_REPORT_INPUTS
    if (MAKE_TEMPLATE_REPORT == 1){

      # Copy core plot to report inputs folder
      plot_name <- paste0("BeSD_Core_", "Child_", ANALYSIS_COUNTER, ".png")

      file.copy(
        from = paste0(OUTPUT_FOLDER, "/Plots_CORE/", plot_name),
        to = paste0(OUTPUT_FOLDER, "/Report Inputs/", plot_name)
      )

      if (!besd_object_exists("BESD_CORE_PLOT_STRATIFIER_LABEL")){
        BESD_CORE_PLOT_STRATIFIER_LABEL <- NA
      }

      temp_report_row <- data.frame(
        indicator = "besd_ch_core",
        variable = NA,
        label = NA,
        title = "BeSD Core Indicator Plot",
        database_id = NA,
        database_path = NA,
        plot_path = paste0(OUTPUT_FOLDER, "/Report Inputs/", plot_name),
        stratifier = BESD_CORE_PLOT_STRATIFIER_LABEL,
        analysis_counter = ANALYSIS_COUNTER
      )

      rm(list = ls(
        pattern = "^BESD_CORE_PLOT_STRATIFIER_LABEL", envir = .GlobalEnv),
        envir = .GlobalEnv)

      BESDTI_REPORT_INPUTS <- bind_rows(temp_report_row,
                                        BESDTI_REPORT_INPUTS)


      BESDTI_REPORT_INPUTS <- BESDTI_REPORT_INPUTS %>%
        mutate(tempsort = row_number()) %>%
        arrange(analysis_counter, tempsort) %>%
        select(-tempsort)

      assign("BESDTI_REPORT_INPUTS", BESDTI_REPORT_INPUTS, envir = .GlobalEnv)
    }


    # Clean out core indicator globals
    if (cleanup == TRUE){
      rm(list = c(
        "BESD_CORE_WEIGHTED",
        "BESD_CORE_DENOMINATOR",
        "BESD_CORE_TABLE_STRUCTURE",
        "BESD_CORE_PLOT_STRATIFIER",
        "BESD_CORE_PLOT_COLORS"
      ), envir = .GlobalEnv) %>% suppressWarnings()
    }

    besd_log_comment(VCP, 5, "Flow", "Exiting")
}


