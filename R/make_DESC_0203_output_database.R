#' Create database for DESC_02 and DESC_03
#'
#' @param variable Outcome variable
#' @param label Label for the outcome variable
#' @param vid Outcome variable ID for filenames
#' @param measureid Analysis indicator ID
#' @param printprogress Progress tracking string to print
#' @param VCP Current program name to be logged, default to be the function name
#' @param ... Other arguments
#'
#' @return A database in OUTPUT_FOLDER
#'
#' @import dplyr
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import survey
#' @import haven
#' @import stringr

# make_DESC_0203_output_database R version 1.00 - Biostat Global Consulting - 2024-08-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-08  1.00      Caitlin Clary   Original R version adapted from v1.01 of
#                                       make_DESC_0203_output_database in vcqiR
# 2024-10-09  1.01      Caitlin Clary   Adding BESDTI_REPORT_INPUTS logic
# *******************************************************************************

# NOTE: functional, with some details pending (see TO DO notes)

make_DESC_0203_output_database <- function(
    variable,
    label = NA,
    vid,
    measureid,
    printprogress = NULL,
    VCP = "make_DESC_0203_output_database",
    database_id = NA,
    ...){

  if (!is.null(printprogress)){
    print(printprogress)
  }

  besd_log_comment(VCP, 5, "Flow", "Starting")

  # This program is used for DESC_02 and DESC_03...sort out which is calling it
  # now and set the appropriate local macro

  templabel <- label
  tempvid <- vid
  tempmeasure <- measureid

  if (is.null(templabel)){
    templabel <- variable
  }

  if (tempmeasure == "DESC_02"){
    mid <- "02"
  } else if (tempmeasure == "DESC_03"){
    mid <- "03"
  } else {
    errormsgs <- paste0("MEASUREID should be DESC_02 or DESC_03 to call ", VCP)
    besd_log_comment(VCP, 1, "Error", paste0("MEASUREID should be DESC_02 or DESC_03 to call ", VCP))
    besd_global(BESDTI_ERROR, 1)
    besd_halt_immediately(
      halt_message = errormsgs
    )
  }

  # Log the database being made
  besd_log_comment(VCP, 3, "Comment",
                   paste0("measureid: ", measureid,
                          " variable: ", variable,
                          " vid: ", vid,
                          " label: ", label))

  if (is.na(database_id)){
    counter <- get(paste0("DESC_", mid, "_COUNTER"), envir = .GlobalEnv)
    dat <- besd_read(
      paste0(OUTPUT_FOLDER, "/DESC_", mid, "_", ANALYSIS_COUNTER, "_", counter, ".rds"))
  } else {
    dat <- besd_read(
      paste0(OUTPUT_FOLDER, "/DESC_", mid, "_", ANALYSIS_COUNTER, "_", database_id, ".rds"))
  }

  # Set survey design based on SVYDESIGN_SYNTAX

  weight <- get(paste0("DESC_", mid, "_WEIGHTED"), envir = .GlobalEnv)
  if (str_to_upper(weight) == "YES"){
    svy_design <- get("SVYDESIGN_SYNTAX", envir = .GlobalEnv)

    if (substring(SVYDESIGN_SYNTAX$ids, 1)[[2]] == "1"){
      datdesign <- svydesign(ids = ~1, strata = SVYDESIGN_SYNTAX$strata,
                             weights = SVYDESIGN_SYNTAX$weights,
                             fpc = SVYDESIGN_SYNTAX$fpc,
                             nest = SVYDESIGN_SYNTAX$nest, data = dat)
    } else {

      if (str_count(as.character(SVYDESIGN_SYNTAX$ids)[2], pattern = fixed("+")) >= 1){

        clustervars <- str_split(as.character(SVYDESIGN_SYNTAX$ids)[2], pattern = fixed("+"))[[1]]
        clustervars <- str_trim(clustervars)

        justone <- TRUE
        for (i in seq_along(clustervars)){
          clusterid <- get(clustervars[i], dat)
          if (!length(unique(clusterid)) %in% 1){
            justone <- FALSE
          }
        } #test if all cluster vars only have 1 unique value

        if (justone == TRUE){
          datdesign <- svydesign(ids = ~1, strata = SVYDESIGN_SYNTAX$strata,
                                 weights = SVYDESIGN_SYNTAX$weights,
                                 fpc = SVYDESIGN_SYNTAX$fpc,
                                 nest = SVYDESIGN_SYNTAX$nest,
                                 data = dat)
        } else {
          datdesign <- svydesign(ids = SVYDESIGN_SYNTAX$ids,
                                 strata = SVYDESIGN_SYNTAX$strata,
                                 weights = SVYDESIGN_SYNTAX$weights,
                                 fpc = SVYDESIGN_SYNTAX$fpc,
                                 nest = SVYDESIGN_SYNTAX$nest,
                                 data = dat)
        }

      } else {
        clusterid <- get(substring(SVYDESIGN_SYNTAX$ids, 1)[[2]], dat)

        if (length(unique(clusterid)) == 1){
          datdesign <- svydesign(ids = ~1, strata = SVYDESIGN_SYNTAX$strata,
                                 weights = SVYDESIGN_SYNTAX$weights,
                                 fpc = SVYDESIGN_SYNTAX$fpc,
                                 nest = SVYDESIGN_SYNTAX$nest,
                                 data = dat)
        } else {

          datdesign <- svydesign(ids = SVYDESIGN_SYNTAX$ids,
                                 strata = SVYDESIGN_SYNTAX$strata,
                                 weights = SVYDESIGN_SYNTAX$weights,
                                 fpc = SVYDESIGN_SYNTAX$fpc,
                                 nest = SVYDESIGN_SYNTAX$nest,
                                 data = dat)
        }
      } #one stage

    }

    wtd <- 1
  }

  if (str_to_upper(weight) == "NO"){
    datdesign <- svydesign(ids = ~1, data = dat) %>% suppressWarnings()
    wtd <- 0
  }

  # Build the lists of variables to post
  vlist <- NULL
  vorder <- NULL
  level_count_without_subtotals <- get(paste0("DESC_", mid, "_LVL_COUNT_", tempvid), envir = .GlobalEnv)

  if (!besd_object_exists(paste0("DESC_", mid, "_SHOW_SUBTOTALS_ONLY"))){
    for (i in 1:level_count_without_subtotals){

      if (mid == "03"){
        valuematch <- DESC_03_VARIABLES[i]
      }
      if (mid == "02"){
        valuematch <- get(paste0("DESC02_VALUE_LEVEL_",i), envir = .GlobalEnv)
      }

      # If a subtotal is supposed to be listed *before* this individual response...add it here
      if (besd_object_exists(paste0("DESC_",mid,"_ST_COUNT_",tempvid))){
        sub_count <- get(paste0("DESC_",mid,"_ST_COUNT_",tempvid), envir = .GlobalEnv)
        if (sub_count != 0){
          for (k in 1:sub_count){
            if (besd_object_exists(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k))){
              sub_list <- get(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k), envir = .GlobalEnv)
              worldmatch = (word(sub_list,2) %in% as.character(valuematch)) | (is.na(valuematch) & word(sub_list,2) == "NA")
              if (((str_to_upper(word(sub_list,1)) == "BEFORE") %in% TRUE & worldmatch) %in% TRUE){
                vorder <- c(vorder,level_count_without_subtotals + k)
                vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_st",k))
              }
            }
          } # end of sub_count k loop
        }
      }

      # List the individual variable
      vorder <- c(vorder,i)
      vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_",i))

      # If a subtotal is supposed to be listed *after* this individual response...add it here
      if (besd_object_exists(paste0("DESC_",mid,"_ST_COUNT_",tempvid))){
        sub_count <- get(paste0("DESC_",mid,"_ST_COUNT_",tempvid), envir = .GlobalEnv)
        if (sub_count != 0){
          for (k in 1:sub_count){
            if (besd_object_exists(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k))){
              sub_list <- get(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k), envir = .GlobalEnv)
              worldmatch = (word(sub_list,2) %in% as.character(valuematch)) | (is.na(valuematch) & word(sub_list,2) == "NA")
              if ((str_to_upper(word(sub_list,1)) %in% "AFTER" & worldmatch) %in% TRUE){
                vorder <- c(vorder,level_count_without_subtotals + k)
                vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_st",k))
              }
            }
          } #end of sub_count k loop
        }
      }

    }#end of level_count_without_subtotals i loop

    assign(paste0("DESC_",mid,"_VORDER"),vorder, envir = .GlobalEnv)
  }

  # If we haven't already listed this subtotal above either before or after an individual response, then list it here
  if (besd_object_exists(paste0("DESC_",mid,"_ST_COUNT_",tempvid))){
    sub_count <- get(paste0("DESC_",mid,"_ST_COUNT_",tempvid), envir = .GlobalEnv)
    if (sub_count != 0){
      for (k in 1:sub_count){
        if (!besd_object_exists(paste0("DESC_",mid,"_SUBTOTAL_LIST_",k))){
          vorder <- c(vorder,level_count_without_subtotals + k)
          vlist <- c(vlist, paste0("desc",mid,"_",tempvid,"_st",k))
        }
      } #end of sub_count k loop

      assign(paste0("DESC_",mid,"_VORDER"),vorder, envir = .GlobalEnv)
    }
  }

  go <- NULL
  DESC_labels <- NULL

  # Record and save the labels for pct_*
  for (k in seq_along(vlist)){
    varlabel <- attr(get(vlist[k], dat),"label")
    if (is.null(varlabel)){
      varlabel <- ""
    }
    label_temp <- data.frame(var = paste0("pct",vorder[k]), label = varlabel)
    DESC_labels <- rbind(DESC_labels,label_temp)
  }

  assign(paste0("DESC_",mid,"_labels_",tempvid),DESC_labels, envir = .GlobalEnv)


  l <- 4
  for (j in 1:nrow(output_layout)){
    # Pass along the name and id of the sub-stratum
    l4name <- output_layout$label[j]
    rowtype <- output_layout$rowtype[j]

    # Weighted case
    if (wtd == 1){
      if (rowtype == "DATA_ROW"){

        # First build gotemp
        gotemp <- data.frame(level = l, outputid = j, outputname = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>%
            mutate(
              !!paste0("pct", vorder[o]) := NA,
              !!paste0("cill", vorder[o]) := NA,
              !!paste0("ciul", vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA, nwtd = NA)

        condition <- output_layout$condition[j]

        for (k in seq_along(vlist)){
          tempvar <- get(vlist[k], dat)
          varlabel <- attr(tempvar,"label")
          # Count respondents meeting the condition(s)
          count <- subset(dat, eval(rlang::parse_expr(condition)) &
                            tempvar %in% c(0,1)) %>% nrow()

          # Only do the calculation and put out the results if there are
          # respondents in this sub-stratum
          if (count > 0){
            ptest <- svypd(
              svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
              var = vlist[k],
              subset_condition = condition,
              ci_level = 95,
              ci_method = CI_METHOD,
              adjust = TRUE,
              truncate = TRUE
            )

            gotemp <- gotemp %>% mutate(!!paste0("pct", vorder[k]) := ptest$estimate,
                                        !!paste0("cill", vorder[k]) := ptest$cill,
                                        !!paste0("ciul", vorder[k]) := ptest$ciul)

          } #end of count

        } #end of vlist k loop

        gotemp <- gotemp %>% mutate(n = ptest$n, nwtd = ptest$nwtd)
        go <- rbind(go, gotemp)
      }

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(level = l, outputid = j, outputname = "BLANK_ROW", outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA, !!paste0("cill",vorder[o]) := NA, !!paste0("ciul",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA, nwtd = NA)
        go <- rbind(go, gotemp)
      }

      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(level = l, outputid = j, outputname = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>%
            mutate(
              !!paste0("pct", vorder[o]) := NA,
              !!paste0("cill", vorder[o]) := NA,
              !!paste0("ciul", vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA, nwtd = NA)
        go <- rbind(go, gotemp)
      }
    } #end of weighted case

    if (wtd == 0){

      if (rowtype == "DATA_ROW"){

        # First build gotemp
        gotemp <- data.frame(level = l, outputid = j, outputname = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)

        condition <- output_layout$condition[j]

        for (k in seq_along(vlist)){
          tempvar <- get(vlist[k], dat)
          # Count respondents meeting the condition(s)
          count <- subset(dat, eval(rlang::parse_expr(condition)) &
                            tempvar %in% c(0,1)) %>% nrow()

          # Only do the calculation and put out the results if there are
          # respondents in this sub-stratum
          if (count > 0){
            ptest <- svypd(
              svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
              var = vlist[k],
              subset_condition = condition,
              ci_level = 95,
              ci_method = CI_METHOD,
              adjust = TRUE,
              truncate = TRUE
            )
            gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[k]) := ptest$estimate)
          } #end of count

        } #end of vlist k loop
        if (count == 0){
          gotemp <- gotemp %>% mutate(n = count)
        } else {
          gotemp <- gotemp %>% mutate(n = ptest$n)
        }
        go <- rbind(go, gotemp)
      }

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(level = l, outputid = j, outputname = "BLANK_ROW", outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)
        go <- rbind(go, gotemp)
      }


      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(level = l, outputid = j, outputname = l4name, outcome = variable)
        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)
        go <- rbind(go, gotemp)
      }
    } #end of unweighted case
  } #end of j loop

  if (is.na(database_id)){
    if (counter < 10){
      counter <- paste0("0",counter)
    }

    filename <- paste0(OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                       "_", counter, "_", tempvid, "_database.rds")
  } else {
    filename <- paste0(OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                       "_", database_id, "_", tempvid, "_database.rds")
  }
  saveRDS(go, filename)

  # Now do a little work to put the ids and names of the various stratum
  # levels into the database
  #
  # The database will serve at least two purposes:
  #
  # 1. It can be exported to a flat file or excel file or database and
  #    may be used with mail-merge software to generate reporting forms
  #    in programs like Microsoft Word.  This provides future flexibility.
  #
  # 2. It will serve as the basis of the `measureid'_05TO program that
	#    exports requested records out to Microsoft Excel.

  # We have all the components of the names; make a single name variable that
  # holds what we think would be best to list in a table (but also keep the
  # components)

  dat <- go %>%
    mutate(name = NA,
           outputname = as.character(outputname),
           # Append the name to the front of the outputname if we have a single
           # stratifier; otherwise leave it off.
           name = ifelse(!is.na(outputname), outputname, name),
           name = ifelse(outputname == "BLANK_ROW", NA, name))

  # Label variable name "Survey name for table output"
  dat <- dat %>%
    relocate(c(name, outputid, outputname), .after = level) %>%
    arrange(level, outputid) # Arranging by outputid no longer matches Stata output

  # Save these variables to the database for future reference...
  weight_option <- get(paste0("DESC_", mid, "_WEIGHTED"), envir = .GlobalEnv)
  den_option <- get(paste0("DESC_", mid, "_DENOMINATOR"), envir = .GlobalEnv)
  dat <- dat %>% mutate(weighted = weight_option, denominator = den_option)

#   if "${DESC_`mid'_SHOW_SUBTOTALS_ONLY"}" == "" {
#
# 			forvalues i = 1/${DESC_`mid'_LVL_COUNT_`vid'} {
# 				label variable pct`i' "`vlabel`i''"
# 			}
# 		}
#
# 		forvalues i = 1/${DESC_`mid'_ST_COUNT_`vid'} {
# 			local j `=${DESC_`mid'_LVL_COUNT_`vid'}+`i''
# 			label variable pct`j' "`vlabel`j''"
# 		}

  dat$level <- haven::labelled(
    dat$level, label = "Stratum level") %>% suppressWarnings()

  dat$outputid <- haven::labelled(
    dat$outputid, label = "Sub-stratum ID") %>% suppressWarnings()

  dat$name <- haven::labelled(
    dat$name, label = "Stratum name for table output") %>% suppressWarnings()

  if (str_length(templabel) > 80){
    comment(dat$outcome) <- templabel

    dat$outcome <- haven::labelled(
      dat$outcome,
      label = "Table title is very long so it is stored in comment") %>%
      suppressWarnings()
  } else {
    dat$outcome <- haven::labelled(
      dat$outcome, label = templabel) %>% suppressWarnings()
  }
  dat$weighted <- haven::labelled(
    dat$weighted, label = "Are the percentages weighted?") %>% suppressWarnings()

  dat$denominator <- haven::labelled(
    dat$denominator, label = "Which respondents are in the denominator?") %>% suppressWarnings()

  saveRDS(dat, filename)

  if (!besd_object_exists("BESDTI_DATABASES")){BESDTI_DATABASES <- NULL}

  if (is.na(database_id)){
    tempdb <- paste0(tempmeasure, "_", ANALYSIS_COUNTER, "_", counter,
                     "_", tempvid, "_database.rds")
  } else {
    tempdb <- paste0(tempmeasure, "_", ANALYSIS_COUNTER, "_", database_id,
                     "_", tempvid, "_database.rds")
  }

  besd_global(BESDTI_DATABASES,
              c(BESDTI_DATABASES, tempdb))

  if (MAKE_TEMPLATE_REPORT == 1){
    # TO DO add condition for excluding from report?
    # TO DO add variables for subtitle, notes, ...?

    temp_report_row <- data.frame(
      indicator = tempmeasure,
      variable = variable,
      label = label,
      title = ifelse(
        tempmeasure == "DESC_02", DESC_02_TO_TITLE, DESC_03_TO_TITLE),
      database_id = database_id,
      database_path = tempdb,
      plot_path = NA # Currently no plots for DESC_02/03
    )

    BESDTI_REPORT_INPUTS <- bind_rows(BESDTI_REPORT_INPUTS,
                                      temp_report_row)

    assign("BESDTI_REPORT_INPUTS", BESDTI_REPORT_INPUTS, envir = .GlobalEnv)
  }

#   filename <- paste0(OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
#                      "_", counter,"_",tempvid, "_database.rds")
# } else {
#   filename <- paste0(OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
#                      "_", database_id, "_", tempvid, "_database.rds")
# }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}
