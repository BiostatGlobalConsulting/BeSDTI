#' Check/create unique ID variables in COVID-19 dataset and merge stratifiers
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @return COV_with_ids dataset in OUTPUT_FOLDER
#'
#' @export
#'
#' @import dplyr
#' @import tidyselect
#'
#' @examples
#' establish_unique_COV_ids()

# establish_unique_COV_ids R version 1.00 - Biostat Global Consulting - 2024-08-26
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-26   1.00      Caitlin Clary  Original R version
# *******************************************************************************

establish_unique_COV_ids <- function(VCP = "establish_unique_COV_ids"){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  # if (!CHECK_INSTEAD_OF_RUN %in% 1){

    dat <- besd_read(paste0(OUTPUT_FOLDER, "/", COV_DATASET)) %>%
      mutate(
        stratum_name = Districtname,
        cluster_id = Clusternum,
        dataframe = "cov")

    # Merge cluster metadata if defined
    if (besd_object_exists("CM_DATASET")){

      cm <- besd_read(paste0(DATA_FOLDER, "/", CM_DATASET)) %>%
        mutate(dataframe = "cm") %>%
        select(stratum_id, stratum_name, cluster_id, cluster_name, province_id,
               urban_cluster, psweight_cov, dataframe)

      dat <- full_join(
        dat, cm, by = c("stratum_name", "cluster_id"))

        rm(cm)

        # We want to keep clusters that do not appear in the dataset, for purposes
        # of calculating degrees of freedom. Be sure to set their weight to zero so
        # they are included properly in the calculations. Note that all outcomes
        # will be missing for these respondents, so they will not affect point
        # estimates, but their presence will help make the DOF calculation right.

        dat <- dat %>%
          mutate(
            psweight_cov = ifelse(
              (is.na(dataframe.x) & dataframe.y %in% "cm") %in% TRUE,
              0, psweight_cov),
            Districtname = ifelse(
              is.na(dataframe.x) & dataframe.y %in% "cm",
              stratum_name, Districtname),
            Clusternum = ifelse(
              is.na(dataframe.x) & dataframe.y %in% "cm",
              cluster_id, Clusternum)
          )

    }

    flag_create_psweight <- 0
    if (!"psweight_cov" %in% names(dat)){
      if ("weights" %in% names(SVYDESIGN_SYNTAX)){
        if (!is.null(SVYDESIGN_SYNTAX$weights)){
          weightvar <- as.character(SVYDESIGN_SYNTAX$weights)[2]
          if (weightvar %in% names(dat)){
            dat$psweight_cov <- get(weightvar, dat)
          } else {flag_create_psweight <- 1}
        } else {flag_create_psweight <- 1}
      } else {flag_create_psweight <- 1 }
    }

    if (flag_create_psweight == 1){
      dat$psweight_cov <- 1

      message("Variable psweight_cov was not found in the COVID-19 dataset or the CM dataset (if provided) and a weight variable was not provided in SVYDESIGN_SYNTAX. BeSD-TI will create a placeholder psweight_cov variable with equal values for all observations.")

      besd_log_comment(VCP, 3, "Comment",
                       "Variable psweight_cov was not found in the COVID-19 dataset or the CM dataset (if provided) and a weight variable was not provided in SVYDESIGN_SYNTAX. BeSD-TI will create a placeholder psweight_cov variable with equal values for all observations.")
    }

    dat <- dat %>%
      rename(psweight = psweight_cov) %>%
      select(-contains("dataframe"))

    # 2024-11-22: if weights var was psweight_cov, swap in psweight
    if ("weights" %in% names(SVYDESIGN_SYNTAX)){
      if (!is.null(SVYDESIGN_SYNTAX$weights)){
        if (SVYDESIGN_SYNTAX$weights == "~psweight_cov"){
          besd_global(SVYDESIGN_SYNTAX,
            list(
              ids = SVYDESIGN_SYNTAX$ids,
              strata = SVYDESIGN_SYNTAX$strata,
              weights = ~psweight,
              fpc = SVYDESIGN_SYNTAX$fpc,
              nest = SVYDESIGN_SYNTAX$nest
            ))
        }
      }
    }

    # If Clusternum is unique within Districtname, we can simply use Clusternum
    # as the clusterid; otherwise we want to make a unique clusterid

    dat <- dat %>%
      group_by(Districtname, Clusternum) %>%
      mutate(rowid = row_number()) %>%
      ungroup() %>%
      mutate(dropthis1 = ifelse(rowid == 1, 1, 0)) %>%
      group_by(Clusternum) %>%
      mutate(dropthis2 = sum(dropthis1)) %>%
      ungroup()

    if (all(dat$dropthis2 == 1)){
      dat <- mutate(dat, clusterid = Clusternum)
    } else{
      dat <- dat %>%
        group_by(Districtname, Clusternum) %>%
        mutate(clusterid = cur_group_id()) %>% ungroup()
    }

    dat <- select(dat, -c(rowid, dropthis1, dropthis2))

    # 2024-11-22: if ids (cluster) var was Clusternum, swap in clusterid
    if ("ids" %in% names(SVYDESIGN_SYNTAX)){
      if (!is.null(SVYDESIGN_SYNTAX$ids)){
        if (SVYDESIGN_SYNTAX$ids == "~Clusternum"){
          besd_global(SVYDESIGN_SYNTAX,
                      list(
                        ids = ~clusterid,
                        strata = SVYDESIGN_SYNTAX$strata,
                        weights = SVYDESIGN_SYNTAX$weights,
                        fpc = SVYDESIGN_SYNTAX$fpc,
                        nest = SVYDESIGN_SYNTAX$nest
                      ))
        }
      }
    }

    dat <- dat %>% arrange(Districtname, clusterid, Housenum ) %>%
      group_by(Districtname, clusterid, Housenum) %>%
      mutate(hhid = cur_group_id()) %>%
      ungroup() %>%
      group_by(Districtname, clusterid, Housenum, PUID) %>%
      mutate(n = n(), respid = cur_group_id()) %>%
      ungroup()

    if (besd_object_exists("OUTPUT_VARLIST")){
      output_vars <- get("OUTPUT_VARLIST", envir = .GlobalEnv)

      for(v in seq_along(output_vars)){

        strat_in_COV <- FALSE

        if(output_vars[v] %in% names(dat)){
          print(paste0("The stratifier ", output_vars[v],
                       " is already part of the COVID-19 dataset."))
          strat_in_COV <- TRUE
        }

        if (strat_in_COV == FALSE){

          # NOTE - since CM merged above, not sure this section will ever be
          # relevant
          # Try to merge stratifier from CM dataset
          if (besd_object_exists("CM_DATASET")){
            print(paste0("Trying to merge variable ", output_vars[v], " from CM"))

            CM <- vcqi_read(paste0(VCQI_DATA_FOLDER, "/", VCQI_CM_DATASET))

            if (output_vars[v] %in% names(CM)){
              CM <- select(CM, stratum_name, cluster_id, output_vars[v])

              dat <- left_join(dat, CM, by = c("stratum_name", "cluster_id"))

              print(paste0("Variable ", output_vars[v], " found in CM dataset"))

              rm(CM)

              strat_in_COV <- TRUE
            }
          }

          # If not in COVID-19 or merged in CM, print message
          if (strat_in_COV == FALSE){
            print(paste0("Did not merge ", output_vars[v], " onto COVID-19 dataset"))
          }

        } # End process when stratifier was not already in COVID-19 dataset
      } # End output_vars loop

    } # End if output varlist exists

    besd_global(TEMP_DATASETS, c(TEMP_DATASETS, "COV_with_ids.rds"))

    # Save updated COV_with_ids
    saveRDS(dat, file = paste0(OUTPUT_FOLDER, "/COV_with_ids.rds"))
  #} # end check instead of run if()

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}
