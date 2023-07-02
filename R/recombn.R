library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

nfcsts <- 100
plength <- 10
model_avail <- 0.8

ks <- c(4, 5, 6, 7)

set.seed(2915)

fcdat <- fread(here("data", "depldat.csv")) |>
  filter(location == "DE",
         target_type == "Cases")

#identify models that are above availability threshold
for(k in ks){

  loc <- "DE"
  tt <- "Cases"

  thresh_models <- fcdat |>
    filter(period_cat %in% c(2,3,4, 5)) |>
    #filter(period_cat == pc) |>
    select(model, forecast_date, period_cat, location, target_type) |>
    distinct()|>
    DT(,  n := .N, by = c("model", "period_cat", "location", "target_type")) |>
    filter(n >= model_avail * plength) |>
    select(model, period_cat) |>
    distinct() |>
    split(by = "period_cat") |>
    lapply(function(mods) mods$model) |>
    lapply(function(mods) combn(mods, k) |> t()) |>
    lapply(function(mods) mods[sample(nrow(mods), nfcsts), ]) |>
    lapply(function(mods) as.data.table(mods)) |>
    rbindlist(idcol = "period_cat")


  data.table::fwrite(thresh_models, here("recombn-ens", paste0("recombn-ens", loc, tt,"_k", k, ".csv")))

}
