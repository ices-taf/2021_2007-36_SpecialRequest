# 2021_2007-36_SpecialRequest
 OSPAR 2021/Production of 2018-2020 and update of 2009-2017 spatial data layers of fishing intensity/pressure

## preparation of sensitive attributes.

The attributes considered sensitive are:

* kW fishing hours
* Fishing hours
* Total weight in the catch
* Total value in the catch

As such these values require to be anonymized (any published value must have 3 or more vessels contributing to it). This was ensured by converting values into one of 20 categories, specified by a lower and upper bound. Checks are undertaken to ensure that each category has 3 or more vessels are contained in that category. Categories are defined separately for each layer, but are common across years and metiers so that data can be accumulated over time if necessary. Importantly, the lowest category is defined to be greater than zero to an upper value that is defined as the 5th percentile of the values for which there are 3 or more vessels contributing to it. This ensures that the lowest category range contains cells with one 2 and 3 or more vessels. The remaining categories are defined as percentiles, so that each category has a meaning in terms of the range of observed values. For example if you combine the top 10 categories you will have the cells with value greater than median, or the top category is the highest 5% of values.

The values provided in the spatial layers are the lower (_min), upper (_max), and midpoint (_mid).

## Request

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
