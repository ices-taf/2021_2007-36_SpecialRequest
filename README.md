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
* Surface area in Km2 (Swept area)
* Surface area ratio
* Sub-surface area in Km2 (Swept area)
* Sub-surface area ratio
* Total Weight
* Total value
* Kw Fishing Hours
* Fishing hour

## preparation of sensitive attributes.

The attributes considered sensitive are:

* Total Weight
* Total value
* Kw Fishing Hours
* Fishing hour

As such these values require to be anonymized (any published value must have 3 or more vessels contributing to it). This was ensured by first converting values into one of 20 categories based on quantiles so that each category contains 5% of the data. However, it is likely that the lower categories contain only single vessel values. So to ensure anonimity, the lowest category is redefined to be from greater than zero to an upper value given by the 5th percentile of the values for which there are 3 or more contributing vessels. This ensures that the lowest category range contains cells with one two and three or more vessels. If this lower category crosses one or more of the 20 categories, they are collapsed into this lower category such that all categories contain no less that 5% of the data, hence, there may be less than 20 categories over all. Further checks are undertaken after this to ensure that each category has 3 or more vessels contained within it. Categories are defined separately for each layer, but are common across years and grouping variable (metier, fishing category) so that data can be accumulated over time if necessary.

Rather than a single value, 3 values are provided in the spatial layers for each sensitive attribute: lower (_low), upper (_upp), and coverage (_cov), giving the lower bound, upper bound and the data coverage for the category that that cell belongs to.  The coverage provides the proportion of the data contained in that category; by design there should be at lease 5% of the data in any category, and only the first 2 categories will contain more than 5% of the data. Therefor, an algorithm which aggregates over the top 5 categories will provide a method of finding the smallest footprint containing 25% of the catches.

As an example, these is the categories defined for DRB_MOL, where it can be seen that the first category contains the lowest 32% of the data, the second category 7.6 and the remaining categories containing 5% each.

|   Hour_low|    Hour_upp| Hour_cov|
|----------:|-----------:|--------:|
|   0.000000|    5.245975|    0.320|
|   5.245975|    8.264933|    0.076|
|   8.264933|   12.000000|    0.050|
|  12.000000|   16.872458|    0.050|
|  16.872458|   23.281783|    0.050|
|  23.281783|   31.701000|    0.050|
|  31.701000|   42.429600|    0.050|
|  42.429600|   56.988306|    0.050|
|  56.988306|   76.849500|    0.050|
|  76.849500|  103.259800|    0.050|
| 103.259800|  144.222800|    0.050|
| 144.222800|  207.148014|    0.050|
| 207.148014|  340.060728|    0.050|
| 340.060728| 2872.000000|    0.050|

## Definition of data sets

### Shapefiles

A total of 209 shapefiles are provided named following the convention: `<layer name>-<year>.shp` for example `Beam-2020.shp`. Each shape file has an attribute table with the following column names:
| Attribute name | Definition |
| -------------- | ---------- |
| Year           | Year       |
| C.squar | C-square |
| lat | Midpoint latitude of c-square |
| lon | Midpoint longitude of c-square |
| subsrfc | Subsurface swept area |
| surface | Surface swept area |
| sar | Surface swept area ratio |
| subsar | Subsurface swept area ratio |
| kWH_low | Kw Fishing Hours lower bound for category |
| kWH_upp | Kw Fishing Hours upper bound for category |
| kWH_cov | Kw Fishing Hours data coverage for category |
| Hour_lw | Fishing hours lower bound for category |
| Hour_pp | Fishing hours upper bound for category |
| Hour_cv | Fishing hours data coverage for category |
| TtWt_lw | Total Weight lower bound for category |
| TtWt_pp | Total Weight upper bound for category |
| TtWt_cv | Total Weight data coverage for category |
| TtVl_lw | Total value lower bound for category |
| TtVl_pp | Total value upper bound for category |
| TtVl_cv | Total value data coverage for category |
| geometry | WKT (Well known text) representation of the c-square |

### csv files (simple features)

Also provided are three csv files containing the same information as the shapefiles:
* total.csv - fully aggregated by year
* fishing_categories.csv - aggregated by fishing category (Beam, etc) and year
* benthic_metiers.csv - aggregated by benthic metier and year

These contain several layers at once with the following structure:

| Column name | Definition |
| -------------- | ---------- |
| Year | Year |
| C-square | C-square |
| lat | Midpoint latitude of c-square |
| lon | Midpoint longitude of c-square |
| benthisMet / FishingCategory / <missing> | For files which contain aggregated data, the aggregated group name |
| subsurface | Subsurface swept area |
| surface | Surface swept area |
| sar | Surface swept area ratio |
| subsar | Subsurface swept area ratio |
| kWH_low | Kw Fishing Hours lower bound for category |
| kWH_upp | Kw Fishing Hours upper bound for category |
| kWH_cov | Kw Fishing Hours data coverage for category |
| Hour_low | Fishing hours lower bound for category  |
| Hour_upp | Fishing hours upper bound for category |
| Hour_cov | Fishing hours data coverage for category |
| TotWt_low | Total Weight lower bound for category  |
| TotWt_upp | Total Weight upper bound for category |
| TotWt_cov | Total Weight data coverage for category |
| TotVal_low | Total value lower bound for category  |
| TotVal_upp | Total value upper bound for category |
| TotVal_cov | Total value data coverage for category |
| wkt | WKT (Well known text) representation of the c-square |
