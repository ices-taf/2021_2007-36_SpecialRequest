## Extract results of interest, write TAF output tables

## Before:
## After:

library(icesTAF)
library(data.table)
library(dplyr)
library(sf)

source("utilities.R")

mkdir("output")

vms_sar <- fread("model/vms_sar.csv")
vms_sar <-
  vms_sar %>%
  filter(
    !CountryCode %in% c("IS", "NO")
  )

# compute summaries of swept area over benthis groups
out_total <-
  vms_sar %>%
  group_by(Year, `C-square`, lat, lon) %>%
  sums()

out_agg <-
  vms_sar %>%
  group_by(Year, `C-square`, lat, lon, fishingCategory) %>%
  sums()

out_benth <-
  vms_sar %>%
  group_by(Year, `C-square`, lat, lon, benthisMet) %>%
  sums()

# save
save_output_total(20)
save_output_agg(20)
save_output_benth(20)
