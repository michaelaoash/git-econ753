## holyoke-poverty-minimal.R
## Michael Ash mash AT umass.edu

## This is a minimal script that downloads and maps Census Tract data
## from the American Community Survey (on the share of families living
## in poverty) for the City of Holyoke, Massachusetts.  The
## accompanying script holyoke-poverty.R in the same repository
## downloads (finer resolution) Block Groups and includes useful
## boundaries, bodies of water, and roads as well as other useful
## additions.

## ACS variables are documented online and there is a load_variables()
## function in tidycensus that downloads a codebook.

library(tidyverse)
library(tidycensus)
## Documentation https://walker-data.com/tidycensus/articles/basic-usage.html
## Get your own Census API Key
## https://api.census.gov/data/key_signup.html
census_api_key("cb9bd8756de7ba64b3c95ec0bd9193fc98d7cfe1")

## Download Census Tract family poverty counts for all of Massachusetts
## Note of caution: Note that every variable has a version that ends in "E" (estimate) and "M" (margin of error)
## Use the E values for the analysis but review the margin of error (which can be large for detailed categories in small areas)
ma_tracts_acs2020 <-
    get_acs(year=2020, geography = "tract", state="MA", keep_geo_vars=TRUE, geometry = TRUE, output="wide",
            variables = c("B17010_004", "B17010_011", "B17010_017",
                          "B17010_024", "B17010_031", "B17010_037")
            )

## Compute the poverty rate from the count data
ma_tracts_acs2020 <- ma_tracts_acs2020 %>%
    mutate(poor_families_with_children       = B17010_004E + B17010_011E + B17010_017E,
           families_with_children            = B17010_004E + B17010_011E + B17010_017E + B17010_024E + B17010_031E + B17010_037E,
           poverty_rt_families_with_children = poor_families_with_children/families_with_children * 100)

## Tracts can distinguish Holyoke from surrounding cities and towns.
holyoke_tracts_acs2020 <- filter(ma_tracts_acs2020, substr(TRACTCE,1,4) %in% c("8114","8115","8116","8117","8118","8119","8120","8121"))

## Plot a choropleth map
ggplot(holyoke_tracts_acs2020) + geom_sf(aes(fill=poverty_rt_families_with_children))
