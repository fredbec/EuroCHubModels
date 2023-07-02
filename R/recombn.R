library(data.table)
library(dplyr)
library(here)
library(ggplot2)
DT <- `[`

source(here("R", "utils-ext.R"))

nfcsts <- 100
plength <- 10
model_avail <- 0.8

ks <- c(4, 5, 6, 7) |> as.list()
periods <- 2:5

random.seed <- 2915

fcdat <- fread(here("data", "depldat.csv")) |>
  filter(location == "DE")


lapply(ks, function(k)
  fcdat |>
    filter(period_cat %in% periods) |>
    recombn_ensemble(k = k,
                     p_length = plength,
                     avail_thresh = model_avail,
                     nfcsts = nfcsts,
                     random.seed = random.seed) |>
    data.table::fwrite(here("recombn-ens", paste0("recombn-ens", "_k", k, ".csv")))
)
