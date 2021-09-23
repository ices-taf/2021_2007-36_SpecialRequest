## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
taf.library(sfdSAR)
library(data.table)

library(ggplot2)
library(dplyr)

mkdir("data")


# load ospar csquares
load(taf.data.path("csquare_lookup", "csquares_ospar.rData"), verbose = TRUE)

# get vms data
vms_files <- dir(taf.data.path("db"), pattern = "vms_*", full.names = TRUE)
vms_all <- lapply(vms_files, fread)
vms_all <- do.call(rbind, vms_all)

# check dates on imports
vms_all %>%
  distinct(CountryCode, importDate) %>%
  mutate(
    importDate = as.POSIXct(round(importDate, units = "mins"))
  ) %>%
  distinct(CountryCode, importDate) %>%
  arrange(desc(importDate))


makeNA <- function(x) {
  ifelse(
    x <= 0 | x == 999999,
    NA,
    x
  )
}

# clean
vms <-
  vms_all %>%
  filter(
    Year < 2021 &
    `C-square` %in% csquares_ospar$csquares
  ) %>%
  mutate(
    AverageVesselLength = makeNA(AverageVesselLength),
    AveragekW = makeNA(AveragekW),
    kWFishingHour = makeNA(kWFishingHour),
    AverageGearWidth = makeNA(AverageGearWidth)
  ) %>%
  select(
    CountryCode, Year, Month, "C-square", MetierL4, MetierL6,
    AverageFishingSpeed, FishingHour, AverageVesselLength, AveragekW,
    kWFishingHour, TotWeight, TotValue, AverageGearWidth, importDate,
    AnonymizedVesselID, NoDistinctVessels
  ) %>%
  mutate(
    AverageGearWidth =
      case_when(
        CountryCode == "NL" & MetierL4 == "SSC" ~ AverageGearWidth * 1000,
        TRUE ~ AverageGearWidth
      )
  )

save(vms, file = "data/vms.rData")
