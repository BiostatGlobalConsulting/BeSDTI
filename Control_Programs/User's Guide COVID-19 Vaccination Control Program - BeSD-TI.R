# BeSD-TI Control Program - COVID-19 Vaccination - Version 1.00 - 2024-11-13
# Biostat Global Consulting

# Behavioral and Social Drivers of Vaccination: Tools for Impact (BeSD-TI)
# control program to analyze data from a COVID-19 vaccination BeSD survey

# Change log

# Date        Version Number    Name              What Changed
# 2024-11-13  1.00              Caitlin Clary     Original version

# *************************************************
# Code Block A                 (Do not change) ----

# Load the BeSD-TI package
library(BeSDTI, attach.required = TRUE)

# Start with clear memory
cleanup_BeSDTI_globals()

# *************************************************
# Code Block B               (User may change) ----

DATA_FOLDER <- "Q:/WHO - BeSD support/CBC Working Folder/BeSD Test Data"

OUTPUT_FOLDER <- "Q:/WHO - BeSD support/CBC Working Folder/BeSD COVID Test Output"

ANALYSIS_NAME <- "COVID_Vx_Test"

# *************************************************
# Code Block C                 (Do not change) ----

setwd(DATA_FOLDER)

# Start with a clean, empty Excel file for tabulated output (TO)
unlink(paste0(OUTPUT_FOLDER, "/", ANALYSIS_NAME, "_TO.xlsx"), force = TRUE)

# Give the current program a name, for logging purposes
VCP <- "BeSD-TI_COVID_Control_Program"

# Open the log and put a comment in it
besd_log_comment(VCP, 3, "Comment", "Run begins...log opened...")

# Document the global macros that were defined before the log opened
besd_log_global(DATA_FOLDER)
besd_log_global(OUTPUT_FOLDER)
besd_log_global(ANALYSIS_NAME)

# Write an entry in the log file documenting the package version
besd_log_comment(VCP, 3, "Package",
                 paste0("BeSD-TI package version ", utils::packageVersion("BeSDTI")))

# *************************************************
# Code Block D               (User may change) ----

# Specify dataset names and important metadata. Datasets should be saved in
# DATA_FOLDER. File names should include file extensions; accepted file types:
# .rds, .dta, .csv

# Name of dataset that holds COVID-19 vaccination BeSD data
besd_global(COV_DATASET, "BeSD_Covid_Data_Faux_2024-12-13.rds")

# Name of dataset that holds cluster metadata (optional)
besd_global(CM_DATASET, "BeSD_CM_COVID_Data_Faux_2024-12-13.rds")

# Surveys about the BeSD of COVID-19 vaccination are sometimes administered to
# health workers and sometimes administered to a general population of adults.
# Specify the respondent type for this survey:
besd_global(COV_SURVEY_RESPONDENTS, "Health Workers") # Valid values: 'Health Workers' or 'Adults'

# Output layout parameters

# The output layout parameters determine the geographic and/or demographic
# strata for which results are displayed in tabular output and plots. Users may
# specify a single stratifier (like urban/rural) or a set of several stratifiers
# (like urban/rural and sex and household wealth). Any variable used as a
# stratifier must be listed in OUTPUT_VARLIST. If you wish to summarize output
# for the entire survey without any stratification, OUTPUT_VARLIST may be left
# undefined.

# If OUTPUT_VARLIST is populated and OUTPUT_LAYOUT is not defined, BeSD-TI will
# generate a default layout for tables and figures. That layout file will be
# saved in the OUTPUT_FOLDER.

# The user may create their own OUTPUT_LAYOUT file defining the conditions,
# preferred order, and row labels for the output strata and point to that layout
# file in the control program, e.g.
# besd_global(OUTPUT_LAYOUT, "Q:/My_BeSD-TI_Output/my_output_layout.rds").
# See the BeSD-TI User's Guide for more details on creating a layout file.

besd_global(OUTPUT_VARLIST, c("Districtname", "Area"))

# besd_global(OUTPUT_LAYOUT, "")

# User specifies survey::svydesign syntax to describe the sample. The data
# argument in survey::svydesign should *not* be specified here.
besd_global(SVYDESIGN_SYNTAX,
            list(ids = ~clusterid, # Created from Clusternum
                 strata = ~Districtname,
                 weights = ~psweight,
                 fpc = NULL,
                 nest = FALSE))

# User specifies the method for calculating confidence intervals
# Valid choices are "Logit", "Wilson", "Jeffreys" or "Clopper"; our default recommendation is "Wilson"
besd_global(CI_METHOD, "Wilson")

# Specify whether the code should export to excel, or not (usually 1)
besd_global(EXPORT_TO_EXCEL, 1)

# User specifies the number of digits after the decimal place in coverage outcomes
besd_global(NUM_DECIMAL_DIGITS, 1)

# Specify whether a report template should be made using the output of this
# BeSD-TI run. Set to 1 for yes.
besd_global(MAKE_TEMPLATE_REPORT, 1)

# Specify whether the code should make plots, or not (usually 1)
# MAKE_PLOTS must be 1 for any plots to be made
besd_global(MAKE_PLOTS, 1)

# Set PLOT_OUTCOMES_IN_TABLE_ORDER to 1 if you want inchworm and unweighted
# plots to list strata in the same order as the tables; otherwise the strata
# will be sorted by the outcome and shown in bottom-to-top order of increasing
# indicator performance
besd_global(PLOT_OUTCOMES_IN_TABLE_ORDER, 0)

# Make bar plots? Set to 1 for yes
besd_global(MAKE_BAR_PLOTS, 1)

# Text at right side of bar plots
# 1 1-sided 95% LCB | Point Estimate | 1-sided 95% UCB
# 2 Point Estimate (2-sided 95% Confidence Interval)  [THIS IS THE DEFAULT]
# 3 Point Estimate (2-sided 95% Confidence Interval) (0, 1-sided 95% UCB]
# 4 Point Estimate (2-sided 95% Confidence Interval) [1-sided 95% UCB, 100)
# 5 Point Estimate (2-sided 95% CI) (0, 1-sided 95% UCB] [1-sided 95% LCB, 100)
besd_global(BAR_PLOT_CITEXT, 2)

# Text at right side of double bar plots
# 1 (default) means show both point estimates
# 2 means show both point estimates and both 2-sided 95% CIs
# 3 means do not show any text
besd_global(DOUBLE_BAR_PLOT_CITEXT, 1)

# Make unweighted sample proportion plots? Set to 1 for yes.
besd_global(MAKE_UW_PLOTS, 1)

# Annotate text in the unweighted plot for small sample sizes? Set 1 for yes.
besd_global(UWPLOT_ANNOTATE_LOW_MED, 0)
# Add square brackets around N < UWPLOT_ANNOTATE_LOW_N; default is 25
besd_global(UWPLOT_ANNOTATE_LOW_N, NA)
# Add parentheses around N < UWPLOT_ANNOTATE_MED_N; default is 50
besd_global(UWPLOT_ANNOTATE_MED_N, NA)

# Make organ pipe plots? Set to 1 for yes.
besd_global(MAKE_OP_PLOTS, 1)

# Save the data underlying each organ pipe plot?  Set to 1 for yes.
# Recall that organ pipe plots do not include many quantitative details
# and do not list the cluster id for any of the bars.
#
# If this option is turned on, (set to 1) then the organ pipe plot program
# will save a dataset in the Plots_OP folder for each plot.  The dataset will
# list the cluster id for each bar in the plot along with its height and width.
# This makes it possible to identify which cluster id goes with which bar in
# the plot and to understand the quantitative details of each bar.
besd_global(SAVE_OP_PLOT_DATA, 1)

# Save the data underlying inchworm plots/bar plots? Set to 1 for yes.
# If this option is turned on, inchworm and barplot programs will save a dataset
# in the Plots_IW_UW folder that makes it possible to understand the quantitative
# details of each plot component and can be used to recreate the plot.
besd_global(SAVE_BAR_PLOT_DATA, 1)

# Save the data underlying unweighted plots? Set to 1 for yes.
# If this option is turned on, unweighted plot programs will save a dataset
# in the Plots_IW_UW folder that makes it possible to understand the quantitative
# details of each plot component and can be used to recreate the plot.
besd_global(SAVE_UW_PLOT_DATA, 1)

# Specify whether the code should save BeSD-TI output databases
#
# WARNING!! If this macro is set to 1, BeSD-TI will delete ALL files that
# end in _database.rds in the OUTPUT_FOLDER at the end of the run
# If you want to save the databases, change the value to 0.
# (Usually 1)
besd_global(DELETE_DATABASES_AT_END, 1)

# If you wish to aggregate files that end
# in _database.rds into a single dataset, set the
# DELETE_DATABASES_AT_END option to 0 and set the
# AGGREGATE_DATABASES option to 1.
besd_global(AGGREGATE_DATABASES, 1)

# Specify whether the code should delete intermediate datasets
# at the end of the analysis (Usually 1)
# If you wish to keep them for additional analysis or debugging,
# set the option to 0.
besd_global(DELETE_TEMP_DATASETS, 1)

# Specify the language for table and figure text. In the default BeSD-TI
# language file, the options are ENGLISH, SPANISH, FRENCH, or PORTUGUESE. Users
# can modify this file to add their own language(s) - for instructions on adding
# a language, see the BeSD-TI User's Guide.

besd_global(OUTPUT_LANGUAGE, "English")

# Specify the file to use when populating table and figure text. Omit this
# global or set it to "default" to use the standard BeSD-TI language file, or
# specify a file path if using a user-modified file.

besd_global(LANGUAGE_FILE, "default")

# Specify the row character cut off length for plot titles - meaning if a title
# is > ### characters it will be split onto the next line(s). Set to NA if
# titles should not be wrapped to additional lines.
besd_global(TITLE_CUTOFF, NA)

# Specify the row character cut off length for plot footnotes - meaning if a
# footnote is > ### characters it will be split onto the next line(s). Set to NA
# if footnotes should not be wrapped to additional lines.
besd_global(FOOTNOTE_CUTOFF, NA)

besd_global(CHECK_INSTEAD_OF_RUN, 0)

# *************************************************
# Code Block E                 (Do not change) ----

# ............................................................
# Check the user's metadata for completeness and correctness
# ............................................................

# Establish an empty object before starting to record temp dataset list
TEMP_DATASETS <- NULL

check_besd_metadata(analysis = "covid")

# ............................................................
# Establish unique IDs
# ............................................................

# The name of the dataset coming out of the ID step is COV_with_ids
establish_unique_COV_ids()

# ............................................................
# Generate derived variables for COVID-19 BeSD analysis
# ............................................................
gen_cov_dv()

# *************************************************
# Code Block F               (User may change) ----

# Calculate BeSD-TI indicators requested by the user

# First, set the analysis counter. This is a counter that is used to name
# datasets. It is usually set to 1 but the user might change it if requesting
# repeat analyses with differing parameters - see the User's Guide

besd_global(ANALYSIS_COUNTER, 1)

# --------------------------------------------------------------------------
# If compiling a template report, set options before calculating indicators
# --------------------------------------------------------------------------

if (MAKE_TEMPLATE_REPORT == 1){

  # Define a title for the report template
  besd_global(REPORT_TITLE, "BeSD Report: Harmonia 2024")

}

# --------------------------------------------------------------------------
# Summarize BeSD core indicators for COVID-19 vaccination
# --------------------------------------------------------------------------

besd_global(BESD_CORE_WEIGHTED, "NO")
besd_global(BESD_CORE_DENOMINATOR, "RESPONDED")

# Define how core outcomes should be reported in tables
# 1 = show dichotomized responses in tables
# 2 = show all response options to core indicators in tables
# 3 = show all response options *with subtotals* for dichotomized categories
besd_global(BESD_CORE_TABLE_STRUCTURE, 1)

# Should bar plots show separate bars for respondents in different categories,
# e.g. male vs. female? If so, define the variable for stratification here.
besd_global(BESD_CORE_PLOT_STRATIFIER, "Area")

# What color(s) should bars in the bar plot be? Leave this global undefined (NA
# or NULL) to use default colors, provide one bar color if
# BESD_CORE_PLOT_STRATIFIER is undefined, or provide colors for each level of
# the variable specified in BESD_CORE_PLOT_STRATIFIER.
besd_global(BESD_CORE_PLOT_COLORS, NA)

# If desired, specify a caption to add to the core indicator bar plot
# besd_global(BESD_CORE_PLOT_CAPTION, "")

# Add headers to the core indicator bar plot specifying which construct each
# core indicator is measuring? Set to 1 to show headers (recommended)
besd_global(BESD_CORE_PLOT_SHOW_HEADERS, 1)

# Suppress results in core plot when N is under a specified threshold?
besd_global(BESD_CORE_PLOT_SUPPRESS_LOW_N, 25)

besd_covid_core(cleanup = TRUE)

# --------------------------------------------------------------------------
# Summarize responses to some multiple-choice questions using DESC_02 & DESC_03
# --------------------------------------------------------------------------

# Standard BeSD question:
# How concerned are you about getting COVID-19?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_risks")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B58"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Have you received a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_vaccs")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B59"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# How safe do you think a COVID-19 vaccine is for you?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_confs")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B62"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Do you think that getting a COVID-19 vaccine will allow you to see your family and friends again?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_seefr")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B63"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# How much do you trust the health workers who would give you a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_confh")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B64"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Do you think most adults you know will get a COVID-19 vaccine if it is recommended to them?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_normp")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B65"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Do you think most of the people you work with will get a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_normw")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B66"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))


# ..............................................................................
# Standard BeSD question:
# Do you think your religious leaders want you to get a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_normr")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B68"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Do you think other community leaders want you to get a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_normc")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B69"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Has a health worker recommended you get a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_rechcw")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B70"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Have you ever been contacted about being due for a COVID-19 vaccine?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_recall")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B71"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# If it was time for you to get a COVID-19 vaccine, would you need permission to go and get it?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_travel")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B72"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Do you know where to go to get a COVID-19 vaccine for yourself?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_where")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B73"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# Is a COVID-19 vaccine available for you to get at your place of work?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_onsite")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B74"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# How easy is it to get a COVID-19 vaccine for yourself?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_access")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE, language_string(language_use = language_use, str = "OS_B75"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# What makes it hard for you to get a COVID-19 vaccine?

besd_global(DESC_03_DATASET, "COV_with_ids.rds")
besd_global(DESC_03_SHORT_TITLE, "Vx_Services_Access")
besd_global(DESC_03_VARIABLES,
            c("COV_lowacnoth", "COV_lowacavail", "COV_lowachard",
              "COV_lowacsite", "COV_lowacopen", "COV_lowacwait",
              "COV_lowacwork", "COV_lowacaway", "COV_lowacelse"))

besd_global(DESC_03_WEIGHTED, "YES")
besd_global(DESC_03_DENOMINATOR, "ALL")
besd_global(DESC_03_SELECTED_VALUE, 1)

besd_global(DESC_03_TO_TITLE,
            language_string(language_use = language_use, str = "OS_B77"))
besd_global(DESC_03_TO_SUBTITLE, NA)

# Relabel COV_lowacelse to simply say "Other"
besd_global(DESC_03_N_RELABEL_LEVELS, 1)
besd_global(DESC_03_RELABEL_LEVEL_1, "COV_lowacelse")
besd_global(DESC_03_RELABEL_LABEL_1,
            language_string(language_use = language_use, str = "OS_304"))

BESD_DESC_03(cleanup = TRUE, database_id = "lowac")

# ..............................................................................
# Standard BeSD question:
# How satisfied are you with COVID-19 vaccination services?
besd_global(DESC_02_DATASET, "COV_with_ids.rds")
besd_global(DESC_02_VARIABLES, "COV_satis")
besd_global(DESC_02_WEIGHTED, "NO")
besd_global(DESC_02_DENOMINATOR, "RESPONDED")

besd_global(DESC_02_TO_TITLE,
            language_string(language_use = language_use, str = "OS_B78"))

besd_global(DESC_02_TO_SUBTITLE, NA) # No subtitle

# Remember that DESC_02 automatically assigns three footnotes, so if you
# want to include another, start with the number 4.
# We are not using it here, but clear it out in case it was used earlier.
besd_global(DESC_02_TO_FOOTNOTE_4, NA)
BESD_DESC_02(cleanup = TRUE,
             database_id = stringr::str_remove(DESC_02_VARIABLES, "COV_"))

# ..............................................................................
# Standard BeSD question:
# What is not satisfactory about the vaccination services?

besd_global(DESC_03_DATASET, "COV_with_ids.rds")
besd_global(DESC_03_SHORT_TITLE, "Vx_Unsatisfactory")
besd_global(DESC_03_VARIABLES,
            c("COV_qualinoth", "COV_qualiavail", "COV_qualiopen",
              "COV_qualiwait", "COV_qualiclean", "COV_qualipoor",
              "COV_qualirespect", "COV_qualitime", "COV_qualielse"))

besd_global(DESC_03_WEIGHTED, "YES")
besd_global(DESC_03_DENOMINATOR, "ALL")
besd_global(DESC_03_SELECTED_VALUE, 1)

besd_global(DESC_03_TO_TITLE,
            language_string(language_use = language_use, str = "OS_B79"))
besd_global(DESC_03_TO_SUBTITLE, NA)

# Relabel COV_qualielse to simply say "Other"
besd_global(DESC_03_N_RELABEL_LEVELS, 1)
besd_global(DESC_03_RELABEL_LEVEL_1, "COV_qualielse")
besd_global(DESC_03_RELABEL_LABEL_1,
            language_string(language_use = language_use, str = "OS_304"))

BESD_DESC_03(cleanup = TRUE, database_id = "quali")

# ..............................................................................
# Compile report

if (MAKE_TEMPLATE_REPORT == 1){
  besd_report(analysis = "covid")
}

# *************************************************
# Code Block G                 (Do not change) ----
#
# Exit gracefully

# 1. Close the datasets that hold the results of hypothesis tests and put them
#    into the output spreadsheet
# 2. Close the log file and put it into the output spreadsheet
# 3. Clean up extra files and global values
# 4. Send a message to the screen if there are warnings or errors in the log

besd_cleanup()
