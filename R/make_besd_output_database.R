#' Create database for BeSD core indicators
#'
#' @param variable Outcome variable
#' @param label Label for the outcome variable
#' @param weighted Calculate weighted indicator (TRUE) or unweighted (FALSE)
#' @param denominator Which respondents to include in the denominator
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

# make_besd_output_database R version 1.00 - Biostat Global Consulting - 2024-07-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-24  1.00      Caitlin Clary   Original R Package version
# *******************************************************************************

make_besd_output_database <- function(
    variable,
    label = NA,
    weighted,
    denominator,
    VCP = "make_besd_output_database",
    ...){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  if (is.na(label)){
    templabel <- variable
  } else {
    templabel <- label
  }

  # Log the database being made
  besd_log_comment(VCP, 3, "Comment",
                   paste0("measureid: ", measureid,
                          " variable: ", variable,
                          " vid: ", vid,
                          " label: ", label))

  dat <- besd_read(
    paste0(OUTPUT_FOLDER, "/", variable, "_", ANALYSIS_COUNTER, ".rds"))

  # Set survey design based on SVYDESIGN_SYNTAX

  if (weighted %in% TRUE){
    if (substring(SVYDESIGN_SYNTAX$ids, 1)[[2]] == "1"){
      datdesign <- svydesign(ids = ~1, strata = SVYDESIGN_SYNTAX$strata,
                             weights = SVYDESIGN_SYNTAX$weights,
                             fpc = SVYDESIGN_SYNTAX$fpc,
                             nest = SVYDESIGN_SYNTAX$nest, data = dat)
    } else {

      if (str_count(as.character(SVYDESIGN_SYNTAX$ids)[2],
                    pattern = fixed("+")) >= 1){

        clustervars <- str_split(as.character(SVYDESIGN_SYNTAX$ids)[2],
                                 pattern = fixed("+"))[[1]]
        clustervars <- str_trim(clustervars)

        justone <- TRUE
        for (i in seq_along(clustervars)){
          clusterid <- get(clustervars[i], dat)
          if (!length(unique(clusterid)) %in% 1){
            justone <- FALSE
          }
        } # test if all cluster vars only have 1 unique value

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
      } # one stage
    }

    wtd <- 1
  } else {
    datdesign <- svydesign(ids = ~1, data = dat) %>% suppressWarnings()
    wtd <- 0
  }

  l <- 4
  for (j in 1:nrow(level4_layout)){
    # Pass along the name and id of the sub-stratum
    l4name <- level4_layout$label[j]
    rowtype <- level4_layout$rowtype[j]

    # Weighted case
    if (wtd == 1){
      if (rowtype == "DATA_ROW"){

        # First build gotemp
        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = variable,
          pct = NA, cill = NA, ciul = NA, n = NA, nwtd = NA)

        condition <- level4_layout$condition[j]

        tempvar <- get(variable, dat)
        varlabel <- attr(tempvar, "label", exact = TRUE)
        # Count respondents meeting the level4 condition(s)
        count <- subset(dat, eval(rlang::parse_expr(condition)) &
                          tempvar %in% c(0,1)) %>% nrow()

        # Only do the calculation and put out the results if there are
        # respondents in this sub-stratum
        if (count > 0){
          ptest <- svypd(
            svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
            var = variable,
            subset_condition = condition,
            ci_level = 95,
            ci_method = CI_METHOD,
            adjust = TRUE,
            truncate = TRUE
          )

          gotemp <- gotemp %>%
            mutate(pct = ptest$estimate,
                   cill = ptest$cill,
                   ciul = ptest$ciul)

        } # end of count

        gotemp <- gotemp %>% mutate(n = ptest$n, nwtd = ptest$nwtd)
        go <- rbind(go, gotemp)
      }

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(
          level = l, level4id = j, level4name = "BLANK_ROW", outcome = variable,
          pct = NA, cill = NA, ciul = NA, n = NA, nwtd = NA)

        go <- rbind(go, gotemp)
      }

      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = variable,
          pct = NA, cill = NA, ciul = NA, n = NA, nwtd = NA)

        go <- rbind(go, gotemp)
      }
    } # end of weighted case

    if (wtd == 0){

      if (rowtype == "DATA_ROW"){

        # First build gotemp
        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = variable,
          pct = NA, n = NA)

        condition <- level4_layout$condition[j]

        tempvar <- get(variable, dat)
        # Count respondents meeting the level4 condition(s)
        count <- subset(dat, eval(rlang::parse_expr(condition)) &
                          tempvar %in% c(0,1)) %>% nrow()

        # Only do the calculation and put out the results if there are
        # respondents in this sub-stratum
        if (count > 0){
          ptest <- svypd(
            svydf = datdesign, # NOT tempdatdesign - subset *inside* svypd
            var = variable,
            subset_condition = condition,
            ci_level = 95,
            ci_method = CI_METHOD,
            adjust = TRUE,
            truncate = TRUE
          )

          gotemp <- gotemp %>%
            mutate(pct = ptest$estimate)
        } # end of count

        if (count == 0){
          gotemp <- gotemp %>% mutate(n = count)
        } else {
          gotemp <- gotemp %>% mutate(n = ptest$n)
        }
        go <- rbind(go, gotemp)
      }

      if (rowtype == "BLANK_ROW"){
        gotemp <- data.frame(
          level = l, level4id = j, level4name = "BLANK_ROW", outcome = variable,
          pct = NA, n = NA)

        go <- rbind(go, gotemp)
      }

      if (rowtype == "LABEL_ONLY"){
        gotemp <- data.frame(
          level = l, level4id = j, level4name = l4name, outcome = variable,
          pct = NA, n = NA)

        for (o in seq_along(vorder)){
          gotemp <- gotemp %>% mutate(!!paste0("pct",vorder[o]) := NA)
        }
        gotemp <- gotemp %>% mutate(n = NA)
        go <- rbind(go, gotemp)
      }
    } # end of unweighted case
  } # end of j loop

  if (counter < 10){
    counter <- paste0("0", counter)
  }

  filename <- paste0(OUTPUT_FOLDER, "/", tempmeasure, "_", ANALYSIS_COUNTER,
                     "_", counter,"_",tempvid, "_database.rds")
  saveRDS(go, filename)

  # Now do a little work to put the ids and names of the various stratum levels
  # into the database. The database will serve at least two purposes:
  #
  # 1. It can be exported to a flat file or excel file or database and may be
  #    used with mail-merge software to generate reporting forms in programs
  #    like Microsoft Word. This provides future flexibility.
  #
  # 2. It will serve as the basis of the `measureid'_05TO program that exports
  #    requested records out to Microsoft Excel.

  # We have all the components of the names; make a single name variable that
  # holds what we think would be best to list in a table (but also keep the
  # components)

  dat <- go %>%
    mutate(name = NA,
           level4name = as.character(level4name),
           # Append the name to the front of the level4name if we have a single
           # stratifier; otherwise leave it off.
           name = ifelse(!is.na(level4name), level4name, name),
           name = ifelse(level4name == "BLANK_ROW", NA, name))

  # Label variable name "Survey name for table output"
  dat <- dat %>%
    relocate(c(name, level4id, level4name), .after = level) %>%
    arrange(level, level4id) # Arranging by level4id no longer matches Stata output

  # Save these variables to the database for future reference...
  weight_option <- weighted
  den_option <- denominator

  dat <- dat %>% mutate(weighted = weight_option, denominator = den_option)

  dat$level <- haven::labelled(
    dat$level, label = "Stratum level") %>% suppressWarnings()

  dat$level4id <- haven::labelled(
    dat$level4id, label = "Sub-stratum ID") %>% suppressWarnings()

  dat$name <- haven::labelled(
    dat$name, label = "Stratum name for table output") %>% suppressWarnings()

  if (str_length(templabel) > 80){
    comment(dat$outcome) <- templabel
    dat$outcome <- haven::labelled(
      dat$outcome,
      label = "Table title is very long so it is stored in comment") %>% suppressWarnings()
  } else {
    dat$outcome <- haven::labelled(
      dat$outcome, label = templabel) %>% suppressWarnings()
  }
  dat$weighted <- haven::labelled(
    dat$weighted, label = "Are the percentages weighted?") %>%
    suppressWarnings()

  dat$denominator <- haven::labelled(
    dat$denominator,
    label = "Which respondents are in the denominator?") %>%
    suppressWarnings()

  saveRDS(dat, filename)

  if (!besd_object_exists("BESDTI_DATABASES")){BESDTI_DATABASES <- NULL}

  besd_global(
    BESDTI_DATABASES,
    c(BESDTI_DATABASES,
      paste0(tempmeasure, "_", ANALYSIS_COUNTER,"_",
             counter,"_", tempvid, "_database.rds")))

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}
