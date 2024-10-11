#' Make core indicator barplot
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param analysis Type of BeSD-TI analysis ("child", "covid", etc.)
#'
#' @import dplyr
#' @import ggplot2
#'
#' @return Core indicator barplot

besd_core_plot <- function(
    VCP = "besd_core_plot",
    analysis
){

  # TO DO - headers should be analysis dependent. Maybe make data frame up top
  # where core_outcomes object defined - add labels, then later join onto
  # plotdat, and then select outcome + header and keep unique to create
  # headerdat.

  # TO DO logic when too few manual colors defined. Revert to defaults? Error?

  # TO DO BESD_CORE_PLOT_STRUCTURE is not defined - not sure this needs to be
  # implemented now, but if not implemented, need to remove check for that
  # global

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
        stringr::str_wrap(., 25),

      # % of parents/caregivers who say most of their close family and friends
      # want their child to be vaccinated
      language_string(
        language_use = language_use, str = "OS_B28") %>%
        stringr::str_wrap(., 25),

      # % of parents/caregivers who want their child to get all of the
      # recommended vaccines
      language_string(
        language_use = language_use, str = "OS_B26") %>%
        stringr::str_wrap(., 25),

      # % of parents/caregivers who know where to get their child vaccinated
      language_string(
        language_use = language_use, str = "OS_B29") %>%
        stringr::str_wrap(., 25),

      # % of parents/caregivers who say vaccination is moderately or very easy
      # to pay for
      language_string(
        language_use = language_use, str = "OS_B30") %>%
        stringr::str_wrap(., 25)
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
      label = as.character(label))


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
    besd_global(BESD_CORE_PLOT_SHOW_HEADERS, 0)
  }

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
        aes(y = 100*estimate, x = outcome,
            fill = label),
        stat = "identity", position = "dodge", color = "grey10") +
      scale_y_continuous(limits = c(0, plot_ymax),
                         breaks = c(0, 25, 50, 75, 100)) +
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
      {if (besd_object_exists("BESD_CORE_PLOT_CAPTION")) labs(
        caption = BESD_CORE_PLOT_CAPTION)} +
      {if (BESD_CORE_PLOT_SHOW_HEADERS %in% 1) geom_text(data = headerdat,
        aes(y = 107, x = outcome, label = header_label), fontface = "bold", size = header_size)}
  } else {
    pl_out <- ggplot(data = plotdat) +
      geom_rect(data = core_shading,
                aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill = c(core_yellow, core_green, core_red, core_blue, core_blue),
                color = "white",
                alpha = 0.5) +
      geom_bar(
        aes(y = 100*estimate, x = outcome),
        stat = "identity", position = "dodge",
        fill = "grey70", color = "grey10") +
      scale_y_continuous(limits = c(0, plot_ymax),
                         breaks = c(0, 25, 50, 75, 100)) +
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
      {if (besd_object_exists("BESD_CORE_PLOT_CAPTION")) labs(
        caption = BESD_CORE_PLOT_CAPTION)} +
      {if (BESD_CORE_PLOT_SHOW_HEADERS %in% 1) geom_text(data = headerdat,
        aes(y = 105, x = outcome, label = header_label), fontface = "bold", size = header_size)}
  }

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

