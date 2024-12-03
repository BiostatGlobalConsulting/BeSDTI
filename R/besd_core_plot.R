#' Make core indicator barplot
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param analysis Type of BeSD-TI analysis ("child", "covid", etc.)
#'
#' @import dplyr
#' @import ggplot2
#' @import stringr
#'
#' @return Core indicator barplot
#'
# besd_core_plot R version 1.01 - Biostat Global Consulting - 2024-10-29

# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-23  1.00      Caitlin Clary   Original R version
# 2024-10-29  1.01      Caitlin Clary   Add footnotes explaining denominator and
#                                       weight options, fix process to specify
#                                       color for unstratified bar plot
# 2024-11-04  1.02      Caitlin Clary   Add options to suppress or annotate
#                                       results depending on N
# *******************************************************************************

# TO DO - avoid dropping unused levels when suppressing results - could just set
# percent to 0 - maybe add a star or something to signify when a level has been
# dropped

# Also want to make footnote printing *contingent* on whether any bars actually
# suppressed/annotated

# Also want to try to get ggpattern working for annotating bars with small sample size

xlab_wrap <- 23

besd_core_plot <- function(
    VCP = "besd_core_plot",
    analysis
){

  # Define dataset and outcomes ----
  if (analysis == "child"){

    plot_name <- paste0(
      "BeSD_Core_", "Child_", ANALYSIS_COUNTER, ".png"
    )

    dat <- readRDS(paste0(OUTPUT_FOLDER, "/BESD_CH_CORE_", ANALYSIS_COUNTER, ".rds"))

    core_outcomes <- c(
      "CHI_confb_binary",
      "CHI_normf_binary",
      "CHI_intent_binary",
      "CHI_where_binary",
      "CHI_afford_binary"
    )

    core_outcome_labels <- c(
      # % of parents/caregivers who say vaccines are moderately or very
      # important for their child's health
      language_string(
        language_use = language_use, str = "OS_B27") %>%
        stringr::str_wrap(., xlab_wrap),

      # % of parents/caregivers who say most of their close family and friends
      # want their child to be vaccinated
      language_string(
        language_use = language_use, str = "OS_B28") %>%
        stringr::str_wrap(., xlab_wrap),

      # % of parents/caregivers who want their child to get all of the
      # recommended vaccines
      language_string(
        language_use = language_use, str = "OS_B26") %>%
        stringr::str_wrap(., xlab_wrap),

      # % of parents/caregivers who know where to get their child vaccinated
      language_string(
        language_use = language_use, str = "OS_B29") %>%
        stringr::str_wrap(., xlab_wrap),

      # % of parents/caregivers who say vaccination is moderately or very easy
      # to pay for
      language_string(
        language_use = language_use, str = "OS_B30") %>%
        stringr::str_wrap(., xlab_wrap)
    )

    header_labels <- c(
      # Confidence in vaccine benefits
      language_string(
        language_use = language_use, str = "OS_B38"
      ) %>% stringr::str_wrap(., 16),

      # Family  norms
      language_string(
        language_use = language_use, str = "OS_B39"
      ) %>% stringr::str_wrap(., 16),

      # Intention to get child vaccinated
      language_string(
        language_use = language_use, str = "OS_B40"
      ) %>% stringr::str_wrap(., 16),

      # Know where to get vaccinated
      language_string(
        language_use = language_use, str = "OS_B41"
      ) %>% stringr::str_wrap(., 16),

      # Affordability
      language_string(
        language_use = language_use, str = "OS_B42"
      ) %>% stringr::str_wrap(., 16)
    )

  } # end if analysis == "child"

  if (analysis == "covid"){

    plot_name <- paste0(
      "BeSD_Core_", "COVID_", ANALYSIS_COUNTER, ".png"
    )

    dat <- readRDS(paste0(OUTPUT_FOLDER, "/besd_covid_core_", ANALYSIS_COUNTER, ".rds"))

    core_outcomes <- c(
      "COV_confb_binary",
      "COV_normf_binary",
      "COV_intent_binary",
      "COV_where_binary",
      "COV_afford_binary"
    )

    if (stringr::str_to_lower(COV_SURVEY_RESPONDENTS) == "adults"){
      core_outcome_labels <- c(
        # % of adults who say a COVID-19 vaccine is moderately or very important
        # for their health
        language_string(
          language_use = language_use, str = "OS_B47") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of adults who say most of their close family and friends want them
        # to get a COVID-19 vaccine
        language_string(
          language_use = language_use, str = "OS_B48") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of adults who say they want to get a COVID-19 vaccine or are already
        # vaccinated
        language_string(
          language_use = language_use, str = "OS_B49") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of adults who say they know where to get a COVID-19 vaccine for
        # themselves
        language_string(
          language_use = language_use, str = "OS_B50") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of adults who say COVID-19 vaccination is moderately or very easy to
        # pay for
        language_string(
          language_use = language_use, str = "OS_B51") %>%
          stringr::str_wrap(., xlab_wrap)
      )
    }

    if (stringr::str_to_lower(COV_SURVEY_RESPONDENTS) == "health workers"){
      core_outcome_labels <- c(
        # % of health workers who say a COVID-19 vaccine is moderately or very important
        # for their health
        language_string(
          language_use = language_use, str = "OS_B52") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of health workers who say most of their close family and friends want them
        # to get a COVID-19 vaccine
        language_string(
          language_use = language_use, str = "OS_B53") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of health workers who say they want to get a COVID-19 vaccine or are already
        # vaccinated
        language_string(
          language_use = language_use, str = "OS_B54") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of health workers who say they know where to get a COVID-19 vaccine for
        # themselves
        language_string(
          language_use = language_use, str = "OS_B55") %>%
          stringr::str_wrap(., xlab_wrap),

        # % of health workers who say COVID-19 vaccination is moderately or very easy to
        # pay for
        language_string(
          language_use = language_use, str = "OS_B56") %>%
          stringr::str_wrap(., xlab_wrap)
      )
    }

    header_labels <- c(
      # Confidence in vaccine benefits
      language_string(
        language_use = language_use, str = "OS_B38"
      ) %>% stringr::str_wrap(., 16),

      # Family norms
      language_string(
        language_use = language_use, str = "OS_B39"
      ) %>% stringr::str_wrap(., 16),

      # Intention to get vaccinated
      language_string(
        language_use = language_use, str = "OS_B57"
      ) %>% stringr::str_wrap(., 16),

      # Know where to get vaccinated
      language_string(
        language_use = language_use, str = "OS_B41"
      ) %>% stringr::str_wrap(., 16),

      # Affordability
      language_string(
        language_use = language_use, str = "OS_B42"
      ) %>% stringr::str_wrap(., 16)
    )

  } # end if analysis == "covid"

  # Define survey design syntax ----
  if (str_to_upper(BESD_CORE_WEIGHTED) == "YES"){
    svy_design <- get("SVYDESIGN_SYNTAX", envir = .GlobalEnv)

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
  }

  if (str_to_upper(BESD_CORE_WEIGHTED) == "NO"){
    datdesign <- svydesign(ids = ~1, data = dat) %>% suppressWarnings()
    wtd <- 0
  }

  # Define stratifier ----
  if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){
    dvar <- get(BESD_CORE_PLOT_STRATIFIER, dat)

    # dvar <- factor(dvar)

    variable_label <- ifelse(
      !is.null(attributes(dvar)$label),
      attributes(dvar)$label, BESD_CORE_PLOT_STRATIFIER)

    if (is.labelled(dvar)){
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

    if (!is.numeric(dvar)){
      valuesandlabels <- valuesandlabels %>%
        mutate(
          condition = paste0(BESD_CORE_PLOT_STRATIFIER, ' == ', '"', val, '"')
        )
    } else {
      valuesandlabels <- valuesandlabels %>%
        mutate(
          condition = paste0(BESD_CORE_PLOT_STRATIFIER, ' == ', val)
        )
    }
    stratvals <- sort(unique(dvar))

    stratifier_label <- ifelse(
      !is.null(attributes(dvar)$label),
      attributes(dvar)$label,
      BESD_CORE_PLOT_STRATIFIER
    )

  } else {
    valuesandlabels <- data.frame(
      val = NA,
      label = NA,
      condition = '1 == 1')
  }


  if (besd_object_exists("BESD_CORE_PLOT_COLORS")) {
    manual_plot_cols <- get("BESD_CORE_PLOT_COLORS", envir = .GlobalEnv)

    if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){

      if (length(manual_plot_cols) != length(stratvals)){
        if (length(manual_plot_cols) > length(stratvals)){
          manual_plot_cols <- manual_plot_cols[1:length(stratvals)]

          warningmsg <- paste0("Too many plot colors provided. Using the first ",
                               length(stratvals),
                               " color(s) specified.")

          warning(warningmsg)

          besd_log_comment(VCP, 2, "Warning", warningmsg)
        }

        if (length(manual_plot_cols) < length(stratvals)){

          warningmsg <- paste0("Too few plot colors provided in BESD_CORE_PLOT_COLORS. ",
                               length(manual_plot_cols), " colors provided; ",
                               length(stratvals), " colors needed. ",
                               "BeSD-TI will use default colors instead.")

          warning(warningmsg)

          besd_log_comment(VCP, 2, "Warning", warningmsg)

          besd_global(BESD_CORE_PLOT_COLORS, NA)

        }
      }
    } else {
      if (length(manual_plot_cols) != 1){
        manual_plot_cols <- manual_plot_cols[1]

        warningmsg <- "Too many plot colors provided. Using the first color specified."

        warning(warningmsg)

        besd_log_comment(VCP, 2, "Warning", warningmsg)
      }
    }

  }

  # Set default color for unstratified bar plot
  if (!besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){
    if (!exists("manual_plot_cols")){
      manual_plot_cols <- "grey80"
    }
  }

  # Generate plot data ----
  plotdat <- NULL
  for(h in seq_along(core_outcomes)){
    for(i in 1:nrow(valuesandlabels)){

      info_i <- svypd(
        svydf = datdesign,
        var = core_outcomes[h],
        subset_condition = valuesandlabels$condition[i],
        ci_level = 95,
        ci_method = CI_METHOD,
        adjust = TRUE,
        truncate = TRUE
      )

      plotdat_i <- data.frame(
        outcome = core_outcomes[h],
        value = valuesandlabels$val[i],
        label = valuesandlabels$lab[i],
        estimate = info_i$estimate,
        cill = info_i$cill,
        ciul = info_i$ciul,
        n = info_i$n,
        nwtd = info_i$nwtd
      )

      plotdat <- bind_rows(plotdat, plotdat_i)

    }
  }

  plotdat <- plotdat %>%
    mutate(
      outcome_temp = outcome,
      outcome = factor(outcome, levels = core_outcomes,
                       labels = core_outcome_labels),
      label = as.character(label),
      estimate_plot = estimate,
      suppress = 0)

  if (besd_object_exists("BESD_CORE_PLOT_SUPPRESS_LOW_N")){

    if (is.numeric(as.numeric(BESD_CORE_PLOT_SUPPRESS_LOW_N))){

      BESD_CORE_PLOT_SUPPRESS_LOW_N <- as.numeric(BESD_CORE_PLOT_SUPPRESS_LOW_N)

      plotdat <- plotdat %>%
        mutate(suppress = ifelse(n < BESD_CORE_PLOT_SUPPRESS_LOW_N, 1, 0),
               estimate_plot = ifelse(suppress == 1, 0, estimate_plot))

      # Only show result suppression footnote when 1+ bars suppressed
      if (sum(plotdat$suppress, na.rm = TRUE) > 0){
        footnote_suppress <- paste0(
          "* Results are suppressed when n < ", BESD_CORE_PLOT_SUPPRESS_LOW_N
        )
      } else {
        footnote_suppress <- NULL
      }

    } else {
      # TO DO add warning message when suppress isn't a number

      footnote_suppress <- NULL

      plotdat <- plotdat %>% mutate(suppress = 0)
    }
  } else {
    footnote_suppress <- NULL
    plotdat <- plotdat %>% mutate(suppress = 0)
  }

  plotdat <- plotdat %>% mutate(suppresstext = ifelse(suppress > 0 & n > 0, "*", ""))


  # if (besd_object_exists("BESD_CORE_PLOT_ANNOTATE_LOW_N")){
  #
  #   if (is.numeric(as.numeric(BESD_CORE_PLOT_ANNOTATE_LOW_N))){
  #
  #     plotdat <- plotdat %>%
  #       mutate(annotate = ifelse(n < BESD_CORE_PLOT_ANNOTATE_LOW_N, 1, 0))
  #
  #     footnote_annotate <- paste0(
  #       "Results are annotated when n < ", BESD_CORE_PLOT_ANNOTATE_LOW_N
  #     )
  #
  #   } else {
  #     # Add warning message when annotate isn't a number
  #
  #     footnote_annotate <- NULL
  #
  #     plotdat <- plotdat %>% mutate(annotate = 0)
  #   }
  # } else {
  #   footnote_annotate <- NULL
  #   plotdat <- plotdat %>% mutate(annotate = 0)
  # }

  headerdat <- plotdat %>%
    select(outcome_temp, outcome) %>% unique() %>%
    mutate(
      header_label = case_when(
        outcome_temp == core_outcomes[1] ~ header_labels[1],
        outcome_temp == core_outcomes[2] ~ header_labels[2],
        outcome_temp == core_outcomes[3] ~ header_labels[3],
        outcome_temp == core_outcomes[4] ~ header_labels[4],
        outcome_temp == core_outcomes[5] ~ header_labels[5]
      )
    )

  # Set some default values
  if (!besd_object_exists("BESD_CORE_PLOT_SHOW_HEADERS")){
    besd_global(BESD_CORE_PLOT_SHOW_HEADERS, 1)
  }

  # Define plot footnotes
  if (stringr::str_to_upper(BESD_CORE_WEIGHTED) == "YES"){
    besd_global(core_plot_footnote1,
                language_string(language_use = language_use, str = "OS_B43"))
  }

  if (stringr::str_to_upper(BESD_CORE_WEIGHTED) == "NO"){
    besd_global(core_plot_footnote1,
                language_string(language_use = language_use, str = "OS_B43"))
  }

  if (stringr::str_to_upper(BESD_CORE_DENOMINATOR) == "ALL"){
    besd_global(core_plot_footnote2,
                language_string(language_use = language_use, str = "OS_B45"))
  }

  if (stringr::str_to_upper(BESD_CORE_DENOMINATOR) == "RESPONDED"){
    besd_global(core_plot_footnote2,
                language_string(language_use = language_use, str = "OS_B46"))
  }

  core_plot_footnote <- paste(core_plot_footnote1, core_plot_footnote2, sep = "\n")

  if (besd_object_exists("BESD_CORE_PLOT_CAPTION")){
    core_plot_footnote <- paste(BESD_CORE_PLOT_CAPTION, core_plot_footnote, sep = "\n")
  }

  if (besd_object_exists("BESD_CORE_PLOT_SUPPRESS_LOW_N")){
    if (!is.null(footnote_suppress)){
      core_plot_footnote <- paste(
        core_plot_footnote, footnote_suppress, sep = "\n"
      )}
  }

  # if (besd_object_exists("BESD_CORE_PLOT_ANNOTATE_LOW_N")){
  #   core_plot_footnote <- paste(
  #     core_plot_footnote, footnote_annotate, sep = "\n"
  #   )
  # }


  # Background colors ----

  core_yellow <- "#FFE699"
  core_green <- "#C5E0B4"
  core_red <- "#FF9999"
  core_blue <- "#B4C7E7"

  if (BESD_CORE_PLOT_SHOW_HEADERS %in% 1){
    plot_ymax <- 115
  } else{
    plot_ymax <- 100
  }

  core_shading <- data.frame(
    domain = c("thinking_feeling", "motivation", "social_processes",
               "practical_issues", "practical_issues"),
    xmin = c(0.5, 1.5, 2.5, 3.5, 4.5)
  ) %>%
    mutate(xmax = xmin + 1,
           ymin = 0,
           ymax = plot_ymax)

  # Make bar plot ----

  axis_text_size <- 11
  header_size <- 4.25

  if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){
    pl_out <- ggplot(data = plotdat) +
      geom_rect(data = core_shading,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = c(core_yellow, core_green, core_red, core_blue, core_blue),
                color = "white",
                alpha = 0.5) +
      geom_bar(
        aes(y = 100*estimate_plot, x = outcome,
            fill = label),
        stat = "identity", position = "dodge", color = "grey10") +
      scale_y_continuous(limits = c(0, plot_ymax),
                         breaks = c(0, 25, 50, 75, 100)) +
      geom_text(data = plotdat,
                aes(y = 5, x = outcome, label = suppresstext, fill = label),
                position = position_dodge2(width = 0.9),
                size = 8) +
      xlab("") + ylab("") +
      theme_minimal() +
      labs(fill = stratifier_label) +
      {if (!besd_object_exists("BESD_CORE_PLOT_COLORS")) scale_fill_brewer(
        palette = "Greys")} +
      {if (besd_object_exists("BESD_CORE_PLOT_COLORS")) scale_fill_manual(
        values = manual_plot_cols
      )} +
      theme(
        plot.background = element_rect(fill = "white", color = "white"),
        axis.text = element_text(size = axis_text_size, color = "black"),
        plot.caption = element_text(size = axis_text_size, hjust = 0),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.text = element_text(size = axis_text_size)
      ) +
      {if (BESD_CORE_PLOT_SHOW_HEADERS %in% 1) geom_text(
        data = headerdat,
        aes(y = 107, x = outcome, label = header_label), fontface = "bold", size = header_size)} +
      labs(caption = core_plot_footnote)
  } else {
    pl_out <- ggplot(data = filter(plotdat, !suppress > 0)) +
      geom_rect(data = core_shading,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = c(core_yellow, core_green, core_red, core_blue, core_blue),
                color = "white",
                alpha = 0.5) +
      geom_bar(
        aes(y = 100*estimate, x = outcome),
        stat = "identity", position = "dodge",
        fill = manual_plot_cols, color = "grey10") +
      scale_y_continuous(limits = c(0, plot_ymax),
                         breaks = c(0, 25, 50, 75, 100)) +
      geom_text(data = plotdat,
                aes(y = 5, x = outcome, label = suppresstext), #, fill = label),
                position = position_dodge2(width = 0.9),
                size = 8) +
      xlab("") + ylab("") +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white", color = "white"),
        axis.text = element_text(size = axis_text_size, color = "black"),
        plot.caption = element_text(size = axis_text_size, hjust = 0),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
      ) +
      {if (BESD_CORE_PLOT_SHOW_HEADERS %in% 1) geom_text(
        data = headerdat,
        aes(y = 105, x = outcome, label = header_label), fontface = "bold", size = header_size)} +
      labs(caption = core_plot_footnote)
  }

  # browser()

  if (!dir.exists(paste0(OUTPUT_FOLDER, "/Plots_CORE"))){
    dir.create(paste0(OUTPUT_FOLDER, "/Plots_CORE"))
  }

  ggsave(
    filename = paste0(OUTPUT_FOLDER, "/Plots_CORE/", plot_name),
    pl_out,
    width = 10, height = 7, units = "in"
  )

  if (SAVE_BAR_PLOT_DATA == 1){

    caption_out <- ifelse(
      besd_object_exists("BESD_CORE_PLOT_CAPTION"),
      BESD_CORE_PLOT_CAPTION, NA
    )

    varlabel_out <- ifelse(
      besd_object_exists("BESD_CORE_PLOT_STRATIFIER"),
      variable_label,
      NA)

    if (besd_object_exists("BESD_CORE_PLOT_STRATIFIER")){
      if (!is.na(varlabel_out)){
        besd_global(BESD_CORE_PLOT_STRATIFIER_LABEL, varlabel_out)
      } else {
        besd_global(BESD_CORE_PLOT_STRATIFIER_LABEL, BESD_CORE_PLOT_STRATIFIER)
      }
    }
    stratified_out <- ifelse(
      besd_object_exists("BESD_CORE_PLOT_STRATIFIER"),
      TRUE, FALSE)

    outdat <- left_join(plotdat, headerdat) %>%
      mutate(
        caption = caption_out,
        stratified = stratified_out,
        show_headers = BESD_CORE_PLOT_SHOW_HEADERS,
        legend_title = varlabel_out,
        core_yellow = core_yellow,
        core_green = core_green,
        core_red = core_red,
        core_blue = core_blue
      )

    saveRDS(
      outdat,
      file = paste0(OUTPUT_FOLDER, "/Plots_CORE/",
                    "BeSD_Core_Plot_Data_", ANALYSIS_COUNTER, ".rds")
    )

  }


} # end function

