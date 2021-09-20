## Run analysis, write model results

## Before:
## After:

library(icesTAF)

mkdir("model")

load("data/vms.rData", verbose = TRUE)

load(taf.data.path("benthis_gears", "aux_lookup.rData"), verbose = TRUE)


# add aux data to vms
vms <-
  vms %>%
  left_join(aux_lookup, by = c("MetierL6" = "leMetLevel6"))

vms_benth <-
  vms %>%
  filter(
    !is.na(benthisMet)
  ) %>%
  rename(
    avg_kw = AveragekW,
    avg_oal = AverageVesselLength,
    a = firstFactor,
    b = secondFactor
  )

# calculate the gear width model for benthis gears only
vms_benth$gearWidth_model <-
  predict_gear_width(
    vms_benth$gearModel,
    vms_benth$gearCoefficient,
    vms_benth
  )


# do the fillin for gear width:
# select provided average gear width, then modelled gear with, then benthis
# average if no kw or aol supplied
vms_benth$gearWidth_filled <-
  with(
    vms_benth,
    ifelse(!is.na(AverageGearWidth), AverageGearWidth / 1000,
      ifelse(!is.na(gearWidth_model), gearWidth_model / 1000,
        gearWidth
      )
    )
  )

# calculate surface contact
vms_benth$surface <-
  predict_surface_contact(
    vms_benth$contactModel,
    vms_benth$FishingHour,
    vms_benth$gearWidth_filled,
    vms_benth$AverageFishingSpeed
  )

# calculate subsurface contact
vms_benth <-
  vms_benth %>%
  mutate(
    subsurface = surface * subsurfaceProp * .01,
    lat = csquare_lat(`C-square`),
    lon = csquare_lon(`C-square`),
    area = csquare_area(`C-square`),
    sar = surface / area,
    subsar = subsurface / area
  )

data.table::fwrite(vms_benth, file = "model/vms_benth.csv")

vms_sar <-
  vms_benth %>%
  select(
    CountryCode, Year, Month, benthisMet, fishingCategory,
    `C-square`, lat, lon,
    surface, subsurface, sar, subsar,
    TotWeight, TotValue, kWFishingHour, FishingHour,
    AnonymizedVesselID, NoDistinctVessels
  )

# save only what we need
data.table::fwrite(
  vms_sar,
  file = "model/vms_sar.csv"
)
