# 2021_2007-36_SpecialRequest
 OSPAR 2021/Production of 2018-2020 and update of 2009-2017 spatial data layers of fishing intensity/pressure

## Request (abridged)

ICES is requested to specifically produce fishing intensity/ pressure spatial layers containing the following information per c-square and per year:
* Aggregated layers:
  * Total
  * beam trawl
  * dredge
  * demersal seine
  * otter trawl
* Aggregated Benthic gear grouping layers:
  * OT_CRU
  * OT_DMF
  * OT_MIX
  * OT_MIX_CRU
  * OT_MIX_DMF_BEN
  * OT_MIX_DMF_PEL
  * OT_MIX_CRU_DMF
  * OT_SPF
  * TBB_CRU
  * TBB_DMF
  * TBB_MOL
  * DRB_MOL
  * SDN_DMF
  * SSC_DMF

This equals 19 layers per year, delivered as Shapefiles with the highest possible resolution, with the following attributes included in each layer:
* Surface area in Km2 (Swept area),
* Surface area ratio,
* Sub-surface area in Km2 (Swept area),
* Sub-surface area ratio,
* Total Weight,
* Total value,
* Kw Fishing Hours,
* Fishing hour

## preparation of sensitive attributes.

The attributes considered sensitive are:

* Total Weight,
* Total value,
* Kw Fishing Hours,
* Fishing hour

As such these values require to be anonymized (any published value must have 3 or more vessels contributing to it). This was ensured by first converting values into one of 20 categories based on quantiles so that each category contains 5% of the data. However, it is likely that the lower categories contain only single vessel values. So to ensure anonimity, the lowest category is redefined to be from greater than zero to an upper value given by the 5th percentile of the values for which there are 3 or more contributing vessels. This ensures that the lowest category range contains cells with one two and three or more vessels. If this crosses one or more of the twenty categories, they are collapsed into this lower category such that all categories contain no less that 5%  of the data. Hence there may be less than 20 categories over all. Further checks are undertaken after this to ensure that each category has 3 or more vessels contained within it. Categories are defined separately for each layer, but are common across years and metiers so that data can be accumulated over time if necessary.

The values provided in the spatial layers are the lower (_low), upper (_upp), and coverage (_cov).  The coverage provides the proportion of the data contained in that category; by design there should be at lease 5% of the data in any category, and only the first 2 categories will contain more than 5% of the data. There, an algorithm which aggregates over the top 5 categories will provide a method of finding the smallest footprint containing 25% of the catches.

## Definition of data sets

### Shapefiles

A total of 209 shapefiles are provided named following the convention: `<layer name>-<year>.shp` for example `Beam-2020.shp`. Each shape file has an attribute table with the following column names:

|Year| . |
|C.squar| . |
|lat| . |
|lon| . |
|fshngCt| . |
|subsrfc| . |
|surface| . |
|sar| . |
|subsar| . |
|kWH_low| . |
|kWH_upp| . |
|kWH_cov| . |
|Hour_lw| . |
|Hour_pp| . |
|Hour_cv| . |
|TtWt_lw| . |
|TtWt_pp| . |
|TtWt_cv| . |
|TtVl_lw| . |
|TtVl_pp| . |
|TtVl_cv| . |
|geometry| . |
