## pdf(file="holyoke-poverty.pdf",height=0,width=0, paper="USr")
## https://www.holyoke.org/maps-of-holyoke/
## https://en.wikipedia.org/wiki/Template:Holyoke,_Massachusetts_Labelled_Map
library(here)
library(tidyverse)
library(janitor)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(sp)
library(sf)
options(width=400)
options(tibble.width=Inf)
options(rlang_backtrace_on_error = "none")
## Documentation https://walker-data.com/tidycensus/articles/basic-usage.html
## Get your own Census API Key
## https://api.census.gov/data/key_signup.html
census_api_key("cb9bd8756de7ba64b3c95ec0bd9193fc98d7cfe1")



## Variable list for American Community Survey (ACS) of the U.S. Bureau of the Census
## Looking for variables with grep
acs_v2020 <- load_variables(2020, "acs5", cache = TRUE)
acs_v2020 %>% filter(grepl("poverty level.*family.*With related children of the householder under 18 years:$",label),
                     !grepl("INDIAN|ASIAN|PACIFIC|RACE",concept))  %>% print(n=40)

acs_v2020 %>% filter(grepl("B17010[AB]_((004)|(011)|(017)|(024)|(031)|(037))",name)) %>% select(label)
acs_v2020 %>% filter(grepl("B17010._(004)",name))

## Get the blockgroups from ACS
## Family poverty counts are available at the Block Group Level
## Family poverty counts by race are available only at/above the Tract Level
## "B17010_004","Estimate!!Total:!!Income in the past 12 months below poverty level:!!Married-couple family:!!With related children of the householder under 18 years:"
## "B17010_011","Estimate!!Total:!!Income in the past 12 months below poverty level:!!Other family:!!Male householder, no spouse present:!!With related children of the householder under 18 years:"
## "B17010_017","Estimate!!Total:!!Income in the past 12 months below poverty level:!!Other family:!!Female householder, no spouse present:!!With related children of the householder under 18 years:"
## "B17010_024","Estimate!!Total:!!Income in the past 12 months at or above poverty level:!!Married-couple family:!!With related children of the householder under 18 years:"
## "B17010_031","Estimate!!Total:!!Income in the past 12 months at or above poverty level:!!Other family:!!Male householder, no spouse present:!!With related children of the householder under 18 years:"
## "B17010_037","Estimate!!Total:!!Income in the past 12 months at or above poverty level:!!Other family:!!Female householder, no spouse present:!!With related children of the householder under 18 years:"


## Use these to get some useful geography
ma_towns_acs2020 <- get_acs(year=2020, geography = "county subdivision", state="MA", keep_geo_vars=TRUE, geometry = TRUE, output="wide",
                            variables = "B01001_001")
holyoke_acs2020 <- ma_towns_acs2020 %>% filter(NAME.x=="Holyoke")

## Download blockgroup-level family poverty counts for all of Massachusetts
## Note of caution: Note that every variable has a version that ends in "E" (estimate) and "M" (margin of error)
## Use the E values for the main analysis but review the margin of error (which can be large for detailed categories in small areas)
## Consider tracts instead of block groups for more precision (lower margin of error)
ma_blockgroups_acs2020 <- get_acs(year=2020, geography = "block group", state="MA", keep_geo_vars=TRUE, geometry = TRUE, output="wide",
                                  variables = c(
                                      "B17010_004",
                                      "B17010_011",
                                      "B17010_017",
                                      "B17010_024",
                                      "B17010_031",
                                      "B17010_037"
                                  )
                                  )


## Compute the poverty rate from the count data
ma_blockgroups_acs2020 <- ma_blockgroups_acs2020 %>% mutate(
                                                         poor_families_with_children = B17010_004E + B17010_011E + B17010_017E,
                                                         families_with_children = B17010_004E + B17010_011E + B17010_017E + B17010_024E + B17010_031E + B17010_037E,
                                                         poverty_rt_families_with_children = poor_families_with_children/families_with_children * 100
                                                         )

## Plot some maps and subset
ggplot(ma_blockgroups_acs2020) + geom_sf()
hampden_blockgroups_acs2020 <- filter(ma_blockgroups_acs2020, COUNTYFP=="013")
ggplot(hampden_blockgroups_acs2020) + geom_sf()

## As it happens, Tracts can neatly distinguish Holyoke from surrounding cities and towns.
holyoke_blockgroups_acs2020 <- filter(hampden_blockgroups_acs2020, substr(TRACTCE,1,4) %in% c("8114","8115","8116","8117","8118","8119","8120","8121"))
ggplot(holyoke_blockgroups_acs2020) + geom_sf() + geom_sf_text(aes(label=GEOID))
ggplot(holyoke_blockgroups_acs2020) + geom_sf(aes(fill=poverty_rt_families_with_children))


## Add some decoration
## Source: https://www2.census.gov/geo/tiger/TIGER2022/
## Get the 2020 Precinct Map of Hampden (from Census/Tigerline)
hampden_VTD_2020  <- voting_districts(state="MA", county="013")
head(hampden_VTD_2020)
ggplot(hampden_VTD_2020) + geom_sf() + geom_sf_text(aes(label=VTDST20))
filter(hampden_VTD_2020,grepl('Holyoke', NAME20, ignore.case=TRUE )) %>% print(n=Inf)
holyoke_VTD_2020  <-    filter(hampden_VTD_2020, as.numeric(substr(VTDST20,1,6))>=930, as.numeric(substr(VTDST20,1,6))<=943)
ggplot(holyoke_VTD_2020) + geom_sf() + geom_sf_text(aes(label=NAME20))

holyoke_VTD_2020 <- holyoke_VTD_2020 %>% mutate(
                         ward = substr(NAME20,14,19)
                     ) %>%
    group_by(ward) %>%
    summarize()

ggplot(holyoke_VTD_2020) + geom_sf() + geom_sf_text(aes(label=ward))


## Limit roads to interstates and major roads in Holyoke
hampden_roads <- roads(state="MA", county="013") 

## s2::s2_options(model = "closed", snap = s2::s2_snap_level(30))
sf_use_s2(FALSE)  ## Aagh don't understand but needed
holyoke_roads  <- hampden_roads %>% filter(MTFCC %in% c("S1100","S1200")) %>% st_intersection(holyoke_acs2020, hampden_roads, model="open")


## See https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2009/TGRSHP09AF.pdf for MTFCC codes
hampden_area_water <- area_water(state="MA", county="013")
ggplot(hampden_area_water) + 
  geom_sf() + 
  theme_void()
ggplot(data = hampden_area_water %>% ## BAD! st_crop(xmin=-90, xmax=-92, ymin=30, ymax=90) %>%
           filter(MTFCC=="H3010" | MTFCC=="H2030")) +
    geom_sf(aes(geometry=geometry), fill="blue", color="blue") +
    geom_sf_text(aes(label=HYDROID), size=2)
holyoke_water  <- st_intersection(hampden_area_water,holyoke_acs2020)

ggplot(data=holyoke_VTD_2020) + geom_sf() + geom_sf_text(aes(label=ward)) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") 

ggplot(holyoke_blockgroups_acs2020) +
    geom_sf(aes(fill=poverty_rt_families_with_children), lwd=0) +
    scale_fill_gradient(low="gray", high="yellow") +
    geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=2) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
    geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))


## Distance from each Massachusetts fossil fuel facility in MA to each of the Seven Wards of Holyoke
fossil_ma <- readRDS(here("egrid2020","fossil-ma-egrid-2020.RDS"))
fossil_ma_sf <- st_as_sf(fossil_ma, crs="NAD83", coords = c("LON", "LAT"))
st_distance(fossil_ma_sf, holyoke_VTD_2020)

fossil_ma_sf[23,]
fossil_ma_sf[39,]


ggplot(ma_towns_acs2020) + geom_sf() + geom_sf(data=fossil_ma_sf, aes(color=PLFUELCT))
