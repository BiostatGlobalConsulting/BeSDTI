#' Clean up BeSD-TI globals
#'
#' @return Remove BeSD-TI global values from global environment
#' @export

# cleanup_BeSDTI_globals R version 1.00 - Biostat Global Consulting - 2024-08-20
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-20  1.00      Caitlin Clary   Original R version
# *******************************************************************************

cleanup_BeSDTI_globals <- function(){

  # Remove individual VCQI objects:
  rm(list = c(

    "DATA_FOLDER",
    "OUTPUT_FOLDER",
    "ANALYSIS_NAME",
    "BESDTI_LOG_OPEN",
    "LOGFILE_NAME",
    "VCP",
    "CH_DATASET",
    "CM_DATASET",
    "COVID_DATASET",
    "OUTPUT_LAYOUT",
    "OUTPUT_VARLIST",
    "SVYDESIGN_SYNTAX",
    "CI_METHOD",
    "EXPORT_TO_EXCEL",
    "NUM_DECIMAL_DIGITS",
    "MAKE_TEMPLATE_REPORT",
    "MAKE_PLOTS",
    "PLOT_OUTCOMES_IN_TABLE_ORDER",
    "MAKE_BAR_PLOTS",
    "BAR_PLOT_CITEXT",
    "DOUBLE_BAR_PLOT_CITEXT",
    "MAKE_UW_PLOTS",
    "MAKE_OP_PLOTS",
    "SAVE_OP_PLOT_DATA",
    "SAVE_IW_PLOT_DATA",
    "SAVE_UW_PLOT_DATA",
    "DELETE_BESDTI_DATABASES_AT_END",
    "AGGREGATE_DATABASES",
    "DELETE_TEMP_BESDTI_DATASETS",
    "MAKE_AUGMENTED_DATASET",
    "OUTPUT_LANGUAGE",
    "LANGUAGE_FILE", "language_file", "language_use",
    "TITLE_CUTOFF",
    "FOOTNOTE_CUTOFF",
    "TEMP_DATASETS",
    "ANALYSIS_COUNTER",

    "bold_left",
    "bold_right",
    "col_header",
    "italic_left",
    "italic_left_indented3",
    "italic_right",
    "regular_left",
    "regular_right",
    "table_footnote",
    "table_header",
    "table_subtitle",
    "table_title",
    "shaded_left",
    "shaded_right",
    "use_basic_fmtids",
    "output_layout",
    "suppress_cis",

    "CHECK_INSTEAD_OF_RUN",
    "DELETE_DATABASES_AT_END",
    "DELETE_TEMP_DATASETS",
    "GENERATE_DATABASES",
    "GENERATE_DVS",
    "PREPROCESS_DATA",
    "SAVE_BAR_PLOT_DATA"


  ), envir = .GlobalEnv) %>% suppressWarnings()

  # Remove BeSD-TI objects by pattern:
  rm(list = ls(pattern = '^BESDTI_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^UWPLOT_ANNOTATE_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC02_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^DESC_03_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^TO_DESC_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^pct', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^BESD_CORE_', envir = .GlobalEnv), envir = .GlobalEnv)
  rm(list = ls(pattern = '^REPORT', envir = .GlobalEnv), envir = .GlobalEnv)


  # rm(list = ls(pattern = '^n', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^nwtd', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_COVG_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_COVG_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_COVG_03_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_DOSES_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_COVG_04_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_CONT_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_CONT_01B_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_QUAL_01_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_QUAL_02_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_QUAL_07B_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_QUAL_08_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^RI_QUAL_09_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = "SHIFTFROM_", envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = "SHIFTTO_", envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '_TEMP_DATASETS$', envir = .GlobalEnv), envir = .GlobalEnv)
  #
  # rm(list = ls(pattern = '^SHIFTWITHIN_', envir = .GlobalEnv), envir = .GlobalEnv)
  # rm(list = ls(pattern = '^DROPDUP_', envir = .GlobalEnv), envir = .GlobalEnv)

}
