make_ensemble <- function(data,
                          summary_function = median,
                          model_name = NULL,
                          excl = c("EuroCOVIDhub-baseline",
                                   "EuroCOVIDhub-ensemble"),
                          extra_excl = NULL,
                          incl = NULL,
                          strat = c("location", "forecast_date", "quantile",
                                    "horizon", "target_type"),
                          extra_vars = c("target_end_date",
                                         "population",
                                         "period_cat"),
                          avail_threshold = NULL){

  #extract function name to make model name
  if(is.null(model_name)){
    model_name <- deparse(summary_function) |>
      paste(collapse = " ") |>
      (\(x) gsub(".*\"(.*)\".*", "\\1", x = x))() |>
      paste0("_ensemble")
  }

  #Input checks
  if(model_name %in% c("weighted.mean_ensemble", "weighted.median_ensemble") & is.null(data$weights)){
    stop("can't compute weighted mean or median without weights")
  }
  if(model_name %in% c("median_ensemble", "mean_ensemble") & !is.null(data$weights)){
    warning("There are weights in the data, but an unweighted summary function was chosen. Was this intended?")
  }

  #check if weights are alright
  if(!is.null(data$weights)){

    if(any(is.na(data$weights))){
      stop("some weights are missing")
    }

    sum_weights <- data |>
      select(model, target_type, location, forecast_date, weights) |>
      distinct() |>
      group_by(target_type, location, forecast_date) |>
      summarise(weight_sum = round(sum(weights), digits = 3),
                .groups = "drop") |>
      select(weight_sum) |>
      distinct() |>
      pull()

    if(length(sum_weights) > 1){
      print(sum_weights)
      message("At the level of one forecast instance, not all weights sum to the same value. Is this intended?")
    } else if (round(sum_weights,3) != 1){
      message("At the level of one forecast instance, weights do not sum to 1. Due to automatic correction,
              this is not necessarily a problem - but check if this is intended")
    }

  }

  #######make model vector based on incl/excl arguments#####
  if(!is.null(extra_excl)){
    excl <- c(excl, extra_excl)
  }

  models <- incl

  #check if any ensembles in models (this should in general not be the case)
  is_ensemble <- grepl(".*ensemble.*", models)

  if(any(is_ensemble)){
    warning(paste0("There seems to be at least one ensemble (\"" ,
                   paste(models[is_ensemble], collapse = "\", \""),
                   "\") that is not in the list of excluded models. Is this intended?"))
  }


  #############make mean/median forecast################

  #extract extra columns before summarising (merge again later)
  extra_cols <- data |>
    dplyr::select(all_of(c(strat, extra_vars))) |>
    dplyr::distinct()

  ensemble <- data |>
    dplyr::group_by(across(all_of(strat))) |>
    dplyr::filter(model %in% models) |>
    dplyr::summarise(prediction = summary_function(prediction,
                                                   w = weights),
                     tvsd = stats::sd(true_value),
                     true_value = mean(true_value), #this is not totally clean, so check further down
                     .groups = 'drop') |>
    dplyr::mutate(model = model_name,
                  availability = 1,
                  model_type = "ensemble") |> #for appending to original data
    dplyr::select(model, everything()) |> #reordering
    merge(extra_cols, by = strat) #add back extra cols

  if(!is.null(data$weights)){
    data <- data |>
      select(-weights)
  }

  #check if true values were unique before summarising
  if(any(is.na(ensemble$tvsd))){
    warning("only one model in the ensemble")
    ensemble$tvsd <- NULL
  } else {
    if(any(ensemble$tvsd!=0)){
      warning("there are multiple true values for one instance")
    } else {
      ensemble$tvsd <- NULL
    }
  }

  return(ensemble)
}

score_ensemble <- function(fcdat, incl, gran = "period_cat"){

  ensdat <- make_ensemble(fcdat, incl = incl)

  scoreens <- ensdat |>
    scoringutils::score() |>
    scoringutils::summarise_scores(by = c("location", "target_type", gran))

  ivscore <- scoreens |> select(interval_score) |> pull()
  return(ivscore)
}
