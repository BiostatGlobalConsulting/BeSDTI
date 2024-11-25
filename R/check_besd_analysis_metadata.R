#' Check BeSD-TI analysis metadata for missing information and format problems
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @import dplyr
#' @import stringr
#'
#' @return Default run settings, default level4 layout file if needed, errors and/or warnings if conditions not met
#'
#' @export
#'
#' @examples
#' check_besd_analysis_metadata()
#'
# check_besd_analysis_metadata R version 1.00 - Biostat Global Consulting - 2024-07-23
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-23  1.00      Caitlin Clary   Original R version adapted from v1.18 of
#                                       check_analysis_metadata in vcqiR
# 2024-09-18  1.01      Caitlin Clary   Add MAKE_TEMPLATE_REPORT checks
# *******************************************************************************

# Note: sections of this program that check FMTID are not implemented (2022-10-07)
# Note: see formatID placeholder comment (2022-10-27)

check_besd_analysis_metadata <- function(VCP = "check_besd_analysis_metadata"){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  exitflag <- 0
  errormsgs <- NULL
  warningmsgs <- NULL

  programlist <- c("DATA_FOLDER", "OUTPUT_FOLDER", "ANALYSIS_NAME")

  for(i in 1:length(programlist)){
    if (besd_object_exists(programlist[i]) == FALSE){
      errormsgs <- c(errormsgs, paste0("Please define ", programlist[i], " in the control program."))

      besd_log_comment(VCP, 1, "Error",
                       paste0("Please define ", programlist[i], " in the control program."))
      exitflag <- 1
    }
  }

  if (exitflag == 1){
    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Establish the labels for tables and figures using the language requested
  # by the user. If they did not specify a language, the program defaults
  # to English
  besd_multi_lingual_strings()

  # check to see if the TITLE and FOOTNOTE CUTOFF globals are set
  if (!besd_object_exists("TITLE_CUTOFF")){
    besd_global(TITLE_CUTOFF, 40)
  }
  if (!besd_object_exists("FOOTNOTE_CUTOFF")){
    besd_global(FOOTNOTE_CUTOFF, 100)
  }

  # Check global BESDTI_PASS_THRU_VARLIST
  # First assign BESDTI_PASS_THRU_VARLIST to NULL if not defined/empty/NA
  if (!besd_object_exists("BESDTI_PASS_THRU_VARLIST")){
    besd_global(BESDTI_PASS_THRU_VARLIST, NULL)
  } else {
    for (i in seq_along(BESDTI_PASS_THRU_VARLIST)){
      if (is.na(BESDTI_PASS_THRU_VARLIST[i]) | str_trim(BESDTI_PASS_THRU_VARLIST[i]) == ""){
        besd_global(BESDTI_PASS_THRU_VARLIST, BESDTI_PASS_THRU_VARLIST[-i])
      }
    }
  }


  if (!is.null(BESDTI_PASS_THRU_VARLIST)){
    if (besd_object_exists("DATA_FOLDER")){
      if (file.exists(paste0(DATA_FOLDER, "/", CH_DATASET))){
        chdat <- besd_read(paste0(DATA_FOLDER, "/", CH_DATASET))
        for(i in seq_along(BESDTI_PASS_THRU_VARLIST)){
          if (!BESDTI_PASS_THRU_VARLIST[i] %in% names(chdat)){
            errormsgs <- c(
              errormsgs,
              paste0("The global BESDTI_PASS_THRU_VARLIST includes ",
                     BESDTI_PASS_THRU_VARLIST[i],
                     ", but it is not a variable in CH_DATASET."))

            besd_log_comment(
              VCP, 1, "Error",
              paste0("The global BESDTI_PASS_THRU_VARLIST includes ",
                     BESDTI_PASS_THRU_VARLIST[i],
                     ", but it is not a variable in CH_DATASET."))
            exitflag <- 1
          }
        } # end of BESDTI_PASS_THRU_VARLIST i loop
      }
    }
  }

  # This syntax should be set in Block E, set as default if user failed to specify
  if (besd_object_exists("SVYDESIGN_SYNTAX") == FALSE){
    # besd_global(SVYDESIGN_SYNTAX,
    #             list(ids = ~clusterid,
    #                  weights = ~psweight,
    #                  strata = ~stratumid,
    #                  fpc = NULL, nest = FALSE))

    besd_global(SVYDESIGN_SYNTAX,
                list(ids = ~Clusternum,
                     weights = ~psweight,
                     strata = ~Districtname,
                     fpc = NULL, nest = FALSE))

  }

  # 2024/05/07 Add options for fpc and nest
  if (! "fpc" %in% names(SVYDESIGN_SYNTAX)){
    besd_global(SVYDESIGN_SYNTAX, c(SVYDESIGN_SYNTAX, list(fpc = NULL)))
  }

  if (! "nest" %in% names(SVYDESIGN_SYNTAX)){
    besd_global(SVYDESIGN_SYNTAX, c(SVYDESIGN_SYNTAX, list(nest = FALSE)))
  }

  ## Output layout (level4) checks ----

  besd_log_global(OUTPUT_VARLIST)
  besd_log_global(OUTPUT_LAYOUT)

  # If the user provides a level4 set_layout they must supply the set_varlist
  if (besd_object_exists("OUTPUT_LAYOUT") & !besd_object_exists("OUTPUT_VARLIST")){
    exitflag <- 1
    errormsgs <- c(errormsgs, "If you specify OUTPUT_LAYOUT then you must also specify OUTPUT_VARLIST.")
    besd_log_comment(VCP, 1, "Error",  "If you specify OUTPUT_LAYOUT then you must also specify OUTPUT_VARLIST.")
  }

  # Check variables provided in the level 4 varlist
  if (besd_object_exists("OUTPUT_VARLIST")){

    # Placeholder for default layout dataset
    layout_temp <- NULL

    for(v in seq_along(OUTPUT_VARLIST)){

      print(paste0("Looking for ", OUTPUT_VARLIST[v]))
      found <- FALSE

      # We don't know which dataset holds each stratifier...most will be in the
      # CH/COVID datasets but some might be in CM or another, so loop over the
      # datasets and keep going until we find it.

      # Note that it may be convenient sometimes to use a stratifier that
      # is constructed DURING the BeSD-TI run and therefore does not exist
      # in the datasets in the DATA_FOLDER. To accommodate this option,
      # we include an item in the search list named TEMP_STRATHOLDER.
      # This global can point to a file in the OUTPUT_FOLDER. Note,
      # for instance, how it is used in the indicator named SIA_COVG_04.

      # Define datasets to look for
      besd_datasets <- c("CH_DATASET", "COV_DATASET", "CM_DATASET")

      # Restrict to datasets that are defined and exist in the data folder

      # For most datasets, look in DATA_FOLDER
      besd_datasets1 <- lapply(seq_along(besd_datasets), function(x){
        if (besd_object_exists(besd_datasets[x])){
          if (file.exists(paste0(DATA_FOLDER, "/", get(besd_datasets[x], envir = globalenv())))){
            besd_datasets[x]
          }
        }}) %>% do.call(c, .)

      # For other datasets, a full path is defined
      besd_datasets2 <- lapply(seq_along(besd_datasets), function(x){
        if (besd_object_exists(besd_datasets[x])){
          if (file.exists(get(besd_datasets[x], envir = globalenv()))){
            besd_datasets[x]
          }
        }}) %>% do.call(c, .)

      besd_datasets2 <- besd_datasets2[!besd_datasets2 %in% besd_datasets1]

      besd_datasets <- c(besd_datasets1, besd_datasets2)

      # Add TEMP_STRATHOLDER if appropriate
      if (besd_object_exists("TEMP_STRATHOLDER")){
        if (file.exists(paste0(OUTPUT_FOLDER, "/", TEMP_STRATHOLDER))){
          besd_datasets <- c(besd_datasets, TEMP_STRATHOLDER)
        }} else {TEMP_STRATHOLDER <- ""}

      for(d in seq_along(besd_datasets)){

        # For datasets other than TEMP_STRATHOLDER...
        if (besd_datasets[d] != TEMP_STRATHOLDER){

          # Read from DATA_FOLDER when appropriate
          if (besd_datasets[d] %in% besd_datasets1){

            temppath <- paste0(DATA_FOLDER, "/",
                               get(besd_datasets[d], envir = global_env()))

            tempdat <- besd_read(temppath)

            # Otherwise read using the full path
          } else {
            temppath <- get(besd_datasets[d], envir = global_env())

            tempdat <- besd_read(temppath)
          }
          # Read TEMP_STRATHOLDER from OUTPUT_FOLDER
        } else {

          temppath <- paste0(OUTPUT_FOLDER, "/",
                             get(besd_datasets[d], envir = global_env()))

          tempdat <- besd_read(temppath)
        }

        varhere <- any(names(tempdat) == OUTPUT_VARLIST[v])

        if (varhere == TRUE){
          print(paste0("Found ", OUTPUT_VARLIST[v], " in ",
                       besd_datasets[d], "..."))

          dat <- besd_read(temppath)

          dvar <- get(OUTPUT_VARLIST[v], dat)

          # If the variable has a label in the input dataset, use that as the
          # variable label, otherwise use the variable name
          variable_label <- ifelse(!is.null(attributes(dvar)$label),
                                   attributes(dvar)$label, OUTPUT_VARLIST[v])

          # if (is.labelled(dvar)){
          if (!is.null(attributes(dvar)$labels)){
            valuesandlabels <- data.frame(
              val = attr(dvar, "labels"),
              lab = names(attr(dvar, "labels"))
            )
          } else {
            valuesandlabels <- data.frame(
              val = sort(unique(dvar)),
              lab = sort(unique(dvar))
            )
          }

          variable_values <- valuesandlabels$val
          variable_value_labels <- valuesandlabels$lab
          nrows <- length(variable_values) + 1

          layout_rows <- data.frame(
            order = rep(NA, nrows),
            label = c(variable_label, rep(NA, nrows-1)),
            condition = rep(NA, nrows),
            rowtype = c("LABEL_ONLY", rep("DATA_ROW", nrows-1))
          )

          # if (is.labelled(dvar)){
          if (!is.null(attributes(dvar)$labels)){
            layout_rows$label[2:nrows] <- names(attr(dvar, "labels"))
            layout_rows$value_todrop <- c(NA, attr(dvar, "labels"))
          } else {
            layout_rows$label[2:nrows] <- sort(unique(dvar))
            layout_rows$value_todrop <- c(NA, sort(unique(dvar)))
          }

          if (is.numeric(dvar)){

            layout_rows <- layout_rows %>%
              mutate(
                condition = ifelse(
                  rowtype %in% "DATA_ROW",
                  paste0(OUTPUT_VARLIST[v], ' == ',  value_todrop), NA),
              ) %>% select(-value_todrop)

          } else {

            layout_rows <- layout_rows %>%
              mutate(
                condition = ifelse(
                  rowtype %in% "DATA_ROW",
                  paste0(OUTPUT_VARLIST[v], ' == ', '"', value_todrop, '"'), NA),
              ) %>% select(-value_todrop)
          }

          layout_temp <- bind_rows(layout_temp, layout_rows)

          # Stop loop here once the variable has been found
          found <- TRUE
          break
        } # end varhere = TRUE

      } # End d loop (searching through datasets)

      if (found == FALSE){
        exitflag <- 1

        errormsgs <- c(
          errormsgs,
          paste0(
            "User asked for variable ", OUTPUT_VARLIST[v],
            " in OUTPUT_VARLIST but that variable doesn't seem to appear in ",
            paste(besd_datasets, collapse = ", ")))

        besd_log_comment(VCP, 1, "Error", paste0(
          "User asked for variable ", OUTPUT_VARLIST[v],
          " in OUTPUT_VARLIST but that variable doesn't seem to appear in ", paste(besd_datasets, collapse = ", ")))
      }

    } # End v loop (looping through variables)

    # If OUTPUT_LAYOUT wasn't provided, save default layout here:
    if (exitflag != 1  & !besd_object_exists("OUTPUT_LAYOUT")){

      # Add format columns with default settings
      layout_temp <- layout_temp %>% mutate(order = 1:n(),
                                            fmtid_for_first_column_r = "regular_left",
                                            fmtid_for_other_columns_r = "regular_right",
                                            outlinecolor1_r = "#0000ff",
                                            outlinecolor2_r = "lightgrey",
                                            bar_fillcolor1_r = "#2b92be",
                                            bar_fillcolor2_r = "lightgrey",
                                            shadecolor1_r = NA,
                                            shadecolor2_r = NA,
                                            addline = NA) %>%
        mutate(fmtid_for_other_columns_r = ifelse(!rowtype %in% "DATA_ROW", NA, fmtid_for_other_columns_r),
               outlinecolor1_r = ifelse(!rowtype %in% "DATA_ROW", NA, outlinecolor1_r),
               bar_fillcolor2_r = ifelse(!rowtype %in% "DATA_ROW", NA, bar_fillcolor2_r),
               bar_fillcolor1_r = ifelse(!rowtype %in% "DATA_ROW", NA, bar_fillcolor1_r),
               bar_fillcolor2_r = ifelse(!rowtype %in% "DATA_ROW", NA, bar_fillcolor2_r))

      saveRDS(layout_temp, file = paste0(OUTPUT_FOLDER, "/OUTPUT_LAYOUT_automatic.rds"))
      besd_global(OUTPUT_LAYOUT,
                  paste0(OUTPUT_FOLDER, "/OUTPUT_LAYOUT_automatic.rds"))
    }

  } # end if varlist supplied but not layout

  if (besd_object_exists("OUTPUT_LAYOUT")){
    if (file.exists(OUTPUT_LAYOUT)){

      output_layout <- besd_read(OUTPUT_LAYOUT)

      varerror <- FALSE
      if (any(names(output_layout) == "order") == FALSE){varerror <- TRUE} else if (
        is.numeric(output_layout$order) == FALSE){varerror <- TRUE}
      if (sum(c("label", "condition", "rowtype") %in% names(output_layout)) < 3){
        varerror <- TRUE
      } else if (
        is.character(output_layout$label) == FALSE | is.character(output_layout$condition) == FALSE | is.character(output_layout$rowtype) == FALSE
      ){varerror <- TRUE}

      if (varerror == TRUE){
        exitflag <- 1
        errormsgs <- c(errormsgs, "OUTPUT_LAYOUT must contain a numeric variable named order and string variables named rowtype, label and condition")
        besd_log_comment(VCP, 1, "Error", "OUTPUT_LAYOUT must contain a numeric variable named order and string variables named rowtype, label and condition")
      }

      #2023/01/10: add new part to check level4 customized plot related settings

      # Check valid inputs for color columns
      color_cols <- c("outlinecolor1_r", "outlinecolor2_r",
                      "bar_fillcolor1_r", "bar_fillcolor2_r",
                      "shadecolor1_r", "shadecolor2_r")

      for(cc in seq_along(color_cols)){
        if (color_cols[cc] %in% names(output_layout)){
           test <- get(color_cols[cc], output_layout)
           test <- test[!is.na(test)]
           test <- test[test != ""]

           if (any(besd_check_color(test) != TRUE)){
             exitflag <- 1
             errormsgs <- c(
               errormsgs,
               paste0(
                 "In the OUTPUT_LAYOUT file, the column ", color_cols[cc], " has invalid values. ",
                 "Valid values are R color names from colors(), valid hex codes, or NA."
               ))

             besd_log_comment(
               VCP, 1, "Error",
               paste0(
                 "In the OUTPUT_LAYOUT file, the column ", color_cols[cc], " has invalid values. ",
                 "Valid values are R color names from colors(), valid hex codes, or NA."
               ))
           }
        }
      }

      # Check if all inputs are valid for addline
      if ("addline" %in% names(output_layout)) {
        addline <- get("addline", output_layout)
        if (any(!addline %in% c("below", "above", "both", "", NA))) {
          exitflag <- 1
          errormsgs <-c(errormsgs, "In the OUTPUT_LAYOUT file, the column addline has invalid values. Valid values are: below, above, both or NA.")
          besd_log_comment(VCP, 1, "Error", "In the OUTPUT_LAYOUT file, the column addline has invalid values. Valid values are: below, above, both or NA.")
        }
      } # end of checking addline

      #2023-02-07 add: check cell format
      assign("use_basic_fmtids", 1, envir = .GlobalEnv)
      # Import the default cell styles
      besd_basic_fmtids()

      if (besd_object_exists("FMTIDS")){
        # First check if the file exists
        if (file.exists(FMTIDS)){
          # Second check if all the styles created are R acceptable
          # Try to source the user-provided file
          sourcetest <- try(source(FMTIDS), silent = TRUE)
          # Check if there were any errors triggered by the user-provided file
          # If there were errors, add a warning message to the log
          # If no errors, source the user-provided file
          if (inherits(sourcetest, "try-error")){
            exitflag <- 1
            #TO DO: update error message and comment
            errormsgs <-c(errormsgs, paste0("FMTIDS file", FMTIDS, " contains invalid cell style. Please check the file"))
            besd_log_comment(VCP,1,"Error",paste0("FMTIDS file", FMTIDS, " contains invalid cell style. Please check the file"))

          } else {
            # Import the customized formats
            assign("use_basic_fmtids", 0, envir = .GlobalEnv)
            source(file = FMTIDS)

            # Check if there is repeated id names if columns are defined.
            defaultid <- c("bold_left", "bold_right", "regular_left", "regular_right", "shaded_left", "shaded_right", "italic_left", "italic_right")

            firstin <- 0
            otherin <- 0
            repeat_ids <- NULL
            if ("fmtid_for_first_column_r" %in% names(output_layout)){
              firstin <- 1
              for (l in 1:nrow(output_layout)){
                firstid <- output_layout$fmtid_for_first_column_r[l]
                # Check if there is repeated id
                if (firstid %in% defaultid){
                  repeat_ids <- c(repeat_ids," ", firstid)
                }

                # Check if there is not defined id
                if (firstid != "" & !is.na(firstid) & !is.null(firstid)){
                  if (!exists(firstid, envir = .GlobalEnv)){
                    output_layout$fmtid_for_first_column_r[l] <- "regular_left"
                    warningmsgs <-c(warningmsgs,paste0("OUTPUT_LAYOUT contains the fmtid ", firstid,
                                                       " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_left"))
                    besd_log_comment(VCP,2,"Warning",paste0("OUTPUT_LAYOUT contains the fmtid ", firstid,
                                                            " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_left"))
                  }
                }
              } #end of l loop
            } #end of if column defined

            if ("fmtid_for_other_columns_r" %in% names(output_layout)){
              otherin <- 1
              for (l in 1:nrow(output_layout)){
                otherid <- output_layout$fmtid_for_other_columns_r[l]
                # Check if there is repeated id
                if (otherid %in% defaultid){
                  repeat_ids <- c(repeat_ids," ", otherid)
                }
                # Check if there is not defined id
                if (otherid != "" & !is.na(otherid) & !is.null(otherid)){
                  if (!exists(otherid, envir = .GlobalEnv)){
                    output_layout$fmtid_for_other_columns_r[l] <- "regular_right"
                    warningmsgs <-c(warningmsgs,paste0("OUTPUT_LAYOUT contains the fmtid ", otherid,
                                                       " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_right"))
                    besd_log_comment(VCP,2,"Warning",paste0("OUTPUT_LAYOUT contains the fmtid ", otherid,
                                                            " which is not defined in the FMTIDS file and cannot be used. It will be replaced with the default value of regular_right"))
                  }
                }
              } #end of l loop
            }#end of if column defined

            # If there's repeated ids, send a message
            if (!is.null(repeat_ids)){
              repeat_ids <- str_flatten(repeat_ids)
              warningmsgs <-c(warningmsgs,paste0("The .R file provided in global FMTIDS contains the default fmtid name(s):", repeat_ids,
                                                 ". The customized fmtid settings will be used over the default values in besd_basic_fmtids function",
                                                 " which means that the foramt(s) might be applied somewehre else too."))
              besd_log_comment(VCP,2,"Warning",paste0("The .R file provided in global FMTIDS contains the default fmtid name(s):", repeat_ids,
                                                      ". The customized fmtid settings will be used over the default values in besd_basic_fmtids function",
                                                      " which means that the foramt(s) might be applied somewehre else too."))

            }

            # If columns not defined, send a message
            if (firstin == 0 & otherin == 0){
              assign("use_basic_fmtids",1, envir = .GlobalEnv)
              warningmsgs <- c(warningmsgs,
                             "Columns fmtid_for_first_column_r or fmtid_for_other_columns_r was not provided in OUTPUT_LAYOUT; BeSD-TI will have access to the default formats from function besd_basic_fmtids")
              besd_log_comment(VCP,2,"Warning",
                               "Columns fmtid_for_first_column_r or fmtid_for_other_columns_r was not provided in OUTPUT_LAYOUT; BeSD-TI will have access to the default formats from function besd_basic_fmtids")
            }

          } #end successfully imported customized formats.

        } else {
          #File path is not valid
          warningmsgs <-c(warningmsgs,paste0("File containing the format ids for OUTPUT_LAYOUT provided in global FMTIDS does not exist: ", FMTIDS))
          besd_log_comment(VCP,2,"Warning",paste0("File containing the format ids for OUTPUT_LAYOUT provided in global FMTIDS does not exist: ", FMTIDS))

          #change fmtid_for_first_column_r abd fmtid_for_other_columns_r to empty if exist
          if ("fmtid_for_first_column_r" %in% names(output_layout)){
            output_layout <- output_layout %>% mutate(fmtid_for_first_column_r = NA)
          }

          if ("fmtid_for_other_columns_r" %in% names(output_layout)){
            output_layout <- output_layout %>% mutate(fmtid_for_other_columns_r = NA)
          }

        }
      } else {

        notempty <- 0
        if ("fmtid_for_first_column_r" %in% names(output_layout)){
          if (!(all(output_layout$fmtid_for_first_column_r == "" |
                    is.na(output_layout$fmtid_for_first_column_r) |
                    is.null(output_layout$fmtid_for_first_column_r) |
                    output_layout$fmtid_for_first_column_r %in%
                    c("regular_left","regular_right","shaded_left","shaded_right",
                      "italic_left","italic_right","bold_left","bold_right","italic_left_indented3")))){
            notempty <- 1
          }
        }

        if ("fmtid_for_other_columns_r" %in% names(output_layout)){
          if (!(all(output_layout$fmtid_for_other_columns_r == "" |
                    is.na(output_layout$fmtid_for_other_columns_r) |
                    is.null(output_layout$fmtid_for_other_columns_r) |
                    output_layout$fmtid_for_other_columns_r %in%
                    c("regular_left","regular_right","shaded_left","shaded_right",
                      "italic_left","italic_right","bold_left","bold_right","italic_left_indented3")))){
            notempty <- 1
          }
        }

        # If the user does not define the file but fmtid_for_first_column_r or
        # fmtid_for_other_columns_r is not empty error out
        if (notempty == 1){
          exitflag <- 1

          errormsgs <- c(
            errormsgs,
            "FMTIDS file is not defined but customized format is defined for at least one row.")

          besd_log_comment(
            VCP, 1, "Error",
            "FMTIDS file is not defined but customized format is defined for at least one row.")
        }

        # If the user does not define the file but fmtid_for_first_column_r or
        # fmtid_for_other_columns_r is not defined or empty, give a message.
        if (notempty == 0){
          warningmsgs <- c(
            warningmsgs,
            "A separate user-defined file with format ids for OUTPUT_LAYOUT was not provided; BeSD-TI will have access to the default formats from function besd_basic_fmtids")

          besd_log_comment(
            VCP, 2, "Warning",
            "A separate user-defined file with format ids for OUTPUT_LAYOUT was not provided; BeSD-TI will have access to the default formats from function besd_basic_fmtids")

          if (!(all(output_layout$fmtid_for_other_columns_r == "" |
                    is.na(output_layout$fmtid_for_other_columns_r) |
                    is.null(output_layout$fmtid_for_other_columns_r) |
                    output_layout$fmtid_for_other_columns_r %in% "regular_left"))) {
            assign("use_basic_fmtids", 0, envir = .GlobalEnv)
          }

          if (!(all(output_layout$fmtid_for_other_columns_r == "" |
                    is.na(output_layout$fmtid_for_other_columns_r) |
                    is.null(output_layout$fmtid_for_other_columns_r) |
                    output_layout$fmtid_for_other_columns_r %in% "regular_right"))){
            assign("use_basic_fmtids", 0, envir = .GlobalEnv)
          }
        }
      } # end of if FMTIDS not defined


    } else {
      exitflag <- 1
      errormsgs <- c(errormsgs, "OUTPUT_LAYOUT dataset does not exist")
      besd_global(BESDTI_ERROR, 1)
      besd_log_comment(VCP, 1, "Error", "OUTPUT_LAYOUT dataset does not exist")
    }
  }

  # Format ID placeholder

  # Check CI method ----

  if (besd_object_exists("CI_METHOD") == FALSE){
    besd_global(CI_METHOD, "Wilson")
  } else {
    if (!(str_to_upper(CI_METHOD) %in% c(
      "LOGIT", "WILSON",
      "CLOPPER", "CLOPPER-PEARSON",
      "JEFFREYS"
    ))){
      errormsgs <- c(errormsgs,
                     paste0("Please set CI_METHOD to either LOGIT, WILSON, CLOPPER-PEARSON, or JEFFREYS. It is currently set to: ", CI_METHOD, "."))
      besd_log_comment(VCP, 1, "Error",
                       paste0("Please set CI_METHOD to either LOGIT, WILSON, CLOPPER-PEARSON, or JEFFREYS. It is currently set to: ", CI_METHOD, "."))

      exitflag <- 1
    }
  }

  # Check settings related to making tables and plots

  # EXPORT_TO_EXCEL must exist and equal 0 or 1
  if (besd_object_exists("EXPORT_TO_EXCEL") == TRUE){
    if (!EXPORT_TO_EXCEL %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set EXPORT_TO_EXCEL to 0 or 1.")
      besd_log_comment(VCP, 1, "Error", "Please set EXPORT_TO_EXCEL to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set EXPORT_TO_EXCEL to 0 or 1.")
    besd_log_comment(VCP, 1, "Error", "Please set EXPORT_TO_EXCEL to 0 or 1.")
    exitflag <- 1
  }

  # MAKE_PLOTS must exist and equal 0 or 1
  if (besd_object_exists("MAKE_PLOTS") == TRUE){
    if (!MAKE_PLOTS %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set MAKE_PLOTS to 0 or 1.")
      besd_log_comment(VCP, 1, "Error", "Please set MAKE_PLOTS to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set MAKE_PLOTS to 0 or 1.")
    besd_log_comment(VCP, 1, "Error", "Please set MAKE_PLOTS to 0 or 1.")
    exitflag <- 1
  }

  # MAKE_TEMPLATE_REPORT must exist and equal 0 or 1
  if (besd_object_exists("MAKE_TEMPLATE_REPORT") == TRUE){
    if (!MAKE_TEMPLATE_REPORT %in% c(0, 1)){
      errormsgs <- c(errormsgs, "Please set MAKE_TEMPLATE_REPORT to 0 or 1.")
      besd_log_comment(VCP, 1, "Error", "Please set MAKE_TEMPLATE_REPORT to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set MAKE_TEMPLATE_REPORT to 0 or 1.")
    besd_log_comment(VCP, 1, "Error", "Please set MAKE_TEMPLATE_REPORT to 0 or 1.")
    exitflag <- 1
  }

  # DELETE_DATABASES_AT_END must exist and equal 0 or 1
  if (besd_object_exists("DELETE_DATABASES_AT_END") == TRUE){
    if (!DELETE_DATABASES_AT_END %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set DELETE_DATABASES_AT_END to 0 or 1.")
      besd_log_comment(VCP, 1, "Error", "Please set DELETE_DATABASES_AT_END to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set DELETE_DATABASES_AT_END to 0 or 1.")
    besd_log_comment(VCP, 1, "Error", "Please set DELETE_DATABASES_AT_END to 0 or 1.")
    exitflag <- 1
  }

  # DELETE_TEMP_DATASETS must exist and equal 0 or 1
  if (besd_object_exists("DELETE_TEMP_DATASETS") == TRUE){
    if (!DELETE_TEMP_DATASETS %in% c(0,1)){
      errormsgs <- c(errormsgs, "Please set DELETE_TEMP_DATASETS to 0 or 1.")
      besd_log_comment(VCP, 1, "Error", "Please set DELETE_TEMP_DATASETS to 0 or 1.")
      exitflag <- 1
    }
  } else {
    errormsgs <- c(errormsgs, "Please set DELETE_TEMP_DATASETS to 0 or 1.")
    besd_log_comment(VCP, 1, "Error", "Please set DELETE_TEMP_DATASETS to 0 or 1.")
    exitflag <- 1
  }

  # Default is to generate databases, although user can turn this off if they already exist
  if (besd_object_exists("PREPROCESS_DATA") == FALSE){besd_global(PREPROCESS_DATA, 1)}
  if (besd_object_exists("GENERATE_DVS") == FALSE){besd_global(GENERATE_DVS, 1)}
  if (besd_object_exists("GENERATE_DATABASES") == FALSE){besd_global(GENERATE_DATABASES, 1)}

  # Default is to make all types of plots and not save plot datasets; user may override
  if (besd_object_exists("MAKE_PLOTS") == TRUE){
    if (MAKE_PLOTS == 1){
      if (besd_object_exists("MAKE_OP_PLOTS") == FALSE){besd_global(MAKE_OP_PLOTS, 1)}
      if (besd_object_exists("MAKE_BAR_PLOTS") == FALSE){besd_global(MAKE_BAR_PLOTS, 1)}
      if (besd_object_exists("MAKE_UW_PLOTS") == FALSE){besd_global(MAKE_UW_PLOTS, 1)}
      if (besd_object_exists("SAVE_OP_PLOT_DATA") == FALSE){besd_global(SAVE_OP_PLOT_DATA, 0)}
      if (besd_object_exists("SAVE_BAR_PLOT_DATA") == FALSE){besd_global(SAVE_BAR_PLOT_DATA, 0)}
      if (besd_object_exists("SAVE_UW_PLOT_DATA") == FALSE){besd_global(SAVE_UW_PLOT_DATA, 0)}
    }
  }

  # If user specifies NUM_DECIMAL_DIGITS, use it; default to 1
  if (besd_object_exists("NUM_DECIMAL_DIGITS") == FALSE){
    besd_global(NUM_DECIMAL_DIGITS, 1)
  }
  # Ensure NUM_DECIMAL_DIGITS is an integer
  besd_global(NUM_DECIMAL_DIGITS, as.integer(NUM_DECIMAL_DIGITS))

  # # Default IWPLOT_SHOWBARS to 0
  # if (besd_object_exists("IWPLOT_SHOWBARS") == FALSE){
  #   besd_global(IWPLOT_SHOWBARS, 0)
  # } else if (IWPLOT_SHOWBARS != 1){besd_global(IWPLOT_SHOWBARS, 0)
  # } else if (IWPLOT_SHOWBARS == 1){
  #   besd_log_comment(VCP, 3, "Comment",
  #                    "The global macro IWPLOT_SHOWBARS is set to 1. So BeSD-TI will make barcharts instead of inchworm plots.")
  # }
  #
  # if (IWPLOT_SHOWBARS == 1){
  #   besd_global(IWPLOT_TYPE, "Bar chart")
  # } else {
  #   besd_global(IWPLOT_TYPE, "Inchworm plot")
  # }

  # Global that controls right side text for double inchworm plots

  # Default to point estimates only for both distributions
  if (besd_object_exists("DOUBLE_BAR_PLOT_CITEXT") == FALSE){
    besd_global(DOUBLE_BAR_PLOT_CITEXT, 1)}

  # If user specified something besides 1 or 2 or 3, reset to 1 and issue warning
  if (!DOUBLE_BAR_PLOT_CITEXT %in% c(1,2,3)){
    besd_log_comment(
      VCP, 2, "Warning",
      paste0("User specified an invalid value of DOUBLE_BAR_PLOT_CITEXT. ",
             "Valid values are 1 or 2 or 3. User specified ", DOUBLE_BAR_PLOT_CITEXT,
             ". Resetting this parameter to default value of 1."))
    besd_global(DOUBLE_BAR_PLOT_CITEXT, 1)
  }

  # Aggregate databases unless (programmer's option) the user asks not to by
  # setting AGGREGATE_DATABASES to something other than 1 (like 0)
  # if (besd_object_exists("AGGREGATE_DATABASES") == FALSE){
  # besd_global(AGGREGATE_DATABASES, 1)}

  if (!is.null(warningmsgs)){
    warning(warningmsgs)
  }

  if (exitflag == 1){
    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
  assign("output_layout", output_layout, envir = global_env())
}


# # Check if all inputs are valid for outlinecolor1_r: valid R color or NA
# if ("outlinecolor1_r" %in% names(output_layout)) {
#   outlinecolor1_r <- get("outlinecolor1_r", output_layout)
#   if (any((!(outlinecolor1_r %in% colors()) & !(nchar(outlinecolor1_r) == 7 & substr(outlinecolor1_r, 1, 1) == "#") & !is.na(outlinecolor1_r) & !outlinecolor1_r %in% c(""))) %in% TRUE) {
#     exitflag <- 1
#     #TO DO: update error message and comment
#     errormsgs <-
#       c(errormsgs, "The column outlinecolor1_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#     besd_log_comment(VCP, 1, "Error", "The column outlinecolor1_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#   }
# } # end of checking outlinecolor1_r

# #check if all inputs are valid for outlinecolor2_r: valid R color or NA
# if ("outlinecolor2_r" %in% names(output_layout)) {
#   outlinecolor2_r <- get("outlinecolor2_r", output_layout)
#   if (any((!(outlinecolor2_r %in% colors())&!(nchar(outlinecolor2_r) == 7 & substr(outlinecolor2_r, 1, 1) == "#") & !is.na(outlinecolor2_r)& !outlinecolor2_r %in% c(""))) %in% TRUE) {
#     exitflag <- 1
#     #TO DO: update error message and comment
#     errormsgs <-
#       c(errormsgs, "The column outlinecolor2_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#     besd_log_comment(VCP, 1, "Error", "The column outlinecolor2_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#   }
# } # end of checking outlinecolor2_r

# # Check if all inputs are valid for bar_fillcolor1_r
# if ("bar_fillcolor1_r" %in% names(output_layout)) {
#   bar_fillcolor1_r <- get("bar_fillcolor1_r", output_layout)
#   if (any((!(bar_fillcolor1_r %in% colors()) & !(nchar(bar_fillcolor1_r) == 7 & substr(bar_fillcolor1_r, 1, 1) == "#") & !is.na(bar_fillcolor1_r) & !bar_fillcolor1_r %in% c(""))) %in% TRUE) {
#     errormsgs <- c(errormsgs, "The column bar_fillcolor1_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#     besd_log_comment(VCP, 1, "Error", "The column bar_fillcolor1_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#   }
# } # end of checking bar_fillcolor1_r

# # Check if all inputs are valid for bar_fillcolor2_r
# if ("bar_fillcolor2_r" %in% names(output_layout)) {
#   bar_fillcolor2_r <- get("bar_fillcolor2_r", output_layout)
#   if (any((!(bar_fillcolor2_r %in% colors())&!(nchar(bar_fillcolor2_r) == 7 & substr(bar_fillcolor2_r, 1, 1) == "#") & !is.na(bar_fillcolor2_r)& !bar_fillcolor2_r %in% c(""))) %in% TRUE) {
#     exitflag <- 1
#     errormsgs <-c(errormsgs, "The column bar_fillcolor2_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#     besd_log_comment(VCP, 1, "Error", "The column bar_fillcolor2_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#   }
# } # end of checking bar_fillcolor2_r

# # Check if all inputs are valid for shadecolor1_r
# if ("shadecolor1_r" %in% names(output_layout)) {
#   shadecolor1_r <- get("shadecolor1_r", output_layout)
#   if (any((!(shadecolor1_r %in% colors()) & !(nchar(shadecolor1_r) == 7 & substr(shadecolor1_r, 1, 1) == "#") & !is.na(shadecolor1_r) & !shadecolor1_r %in% c(""))) %in% TRUE) {
#     exitflag <- 1
#     errormsgs <-c( errormsgs, "The column shadecolor1_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#     besd_log_comment(VCP, 1, "Error", "The column shadecolor1_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#   }
# } # end of checking shadecolor1_r

# # Check if all inputs are valid for shadecolor2_r
# if ("shadecolor2_r" %in% names(output_layout)) {
#   shadecolor2_r <- get("shadecolor2_r", output_layout)
#   if (any((!(shadecolor2_r %in% colors()) & !(nchar(shadecolor2_r) == 7 & substr(shadecolor2_r, 1, 1) == "#") & !is.na(shadecolor2_r) & !shadecolor2_r %in% c(""))) %in% TRUE) {
#     exitflag <- 1
#     #TO DO: update error message and comment
#     errormsgs <-c( errormsgs,"The column shadecolor2_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#     besd_log_comment(VCP, 1, "Error","The column shadecolor2_r has invalid values. Valid values are R color names from colors(), valid hex codes, or NA")
#   }
# } # end of checking shadecolor2_r
