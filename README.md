# 2021_2007-36_SpecialRequest
 OSPAR 2021/Production of 2018-2020 and update of 2009-2017 spatial data layers of fishing intensity/pressure

## preparation of sensitive attributes.

The attributes considered sensitive are:

* kW fishing hours
* Fishing hours
* Total weight in the catch
* Total value in the catch

As such these values require to be anonymized (any published value must have 3 or more vessels contributing to it). This was ensured by converting values into one of 20 categories, specified by a lower and upper bound. Checks are undertaken to ensure that each category has 3 or more vessels are contained in that category. Categories are defined separately for each layer, but are common across years so that data can be accumulated over time if necessary. Importantly, the lowest category is defined from zero to an upper value that is defined as the 5th percentile of the values for which there are 3 or vessels contributing to it. This ensures that the lowest category range contains cells with one 2 and 3 or more vessels. The remaining categories are defined as percentiles, so that each category has a meaning in terms of the range of observed values. For example if you combine the top 10 categories you will have the cells with value greater than median, or the top category is the highest 5% of values.

The values provided in the spatial layers are the lower (_min), upper (_max), and midpoint (_mid).