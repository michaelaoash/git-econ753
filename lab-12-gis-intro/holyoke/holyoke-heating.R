pdf(file="holyoke-heating-blockgroups.pdf",height=0,width=0, paper="USr")
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
acs_v2020 %>% filter(grepl("HEATING", concept)) %>% print(n=50)
acs_v2020 %>% filter(concept=="TENURE") %>% print(n=15)


## Use these to get the outline of Holyoke (for cutting maps)
ma_towns_acs2020 <- get_acs(year=2020, geography = "county subdivision", state="MA", keep_geo_vars=TRUE, geometry = TRUE, output="wide",
                            variables = "B01001_001")
holyoke_acs2020 <- ma_towns_acs2020 %>% filter(NAME.x=="Holyoke")

## Download blockgroup-level heating/tenure variables (counts) for Hampden County
## Download tract-level heating/tenure variables (counts) for Hampden County
## Note of caution: Note that every variable has a version that ends in "E" (estimate) and "M" (margin of error)
## Use the E values for the main analysis but review the margin of error (which can be large for detailed categories in small areas)
## Consider tracts instead of block groups for more precision (lower margin of error)
hampden_blockgroups_acs2020 <- get_acs(year=2020, geography = "block group", state="MA", county="013", keep_geo_vars=TRUE, geometry = TRUE, output="wide",
                                       variables = c(
                                           "B25003_001",
                                           "B25003_002",
                                           "B25003_003",                                           
                                           "B25040_001",
                                           "B25040_002",
                                           "B25040_003",
                                           "B25040_004",
                                           "B25040_005",
                                           "B25040_006",
                                           "B25040_007",
                                           "B25040_008",
                                           "B25040_009",
                                           "B25040_010",
                                           "B992511_001",
                                           "B992511_002",
                                           "B992511_003"
                                       )
                                       )

## Compute rates from count data
hampden_blockgroups_acs2020 <- hampden_blockgroups_acs2020 %>%
    mutate(
        owner_occupied_rt = B25003_002E / B25003_001E * 100,
        renter_occupied_rt = B25003_003E / B25003_001E * 100,
        heat_util_gas_rt = B25040_002E / B25040_001E * 100,
        heat_tank_gas_rt = B25040_003E / B25040_001E * 100,
        heat_electricity_rt = B25040_004E / B25040_001E * 100,
        heat_oil_rt = B25040_005E / B25040_001E * 100,
        heat_coal_rt = B25040_006E / B25040_001E * 100,
        heat_wood_rt = B25040_007E / B25040_001E * 100,
        heat_solar_rt = B25040_008E / B25040_001E * 100,
        heat_other_rt = B25040_009E / B25040_001E * 100,
        heat_none_rt = B25040_010E / B25040_001E * 100,
        heat_estimated_rt = B992511_002E / B992511_001E * 100,
    )

hampden_blockgroups_acs2020 %>% tidyfst::select_dt("heat.*rt")

## Compute most common modal heat source
heat_source <- hampden_blockgroups_acs2020 %>%
    select(GEOID, heat_util_gas_rt, heat_tank_gas_rt, heat_electricity_rt, heat_oil_rt, heat_coal_rt, heat_wood_rt, heat_solar_rt, heat_other_rt, heat_none_rt) %>%
    st_drop_geometry()
modal_heat <- tidyfst::col_max(heat_source, .name="modal_heat_source") %>%
    select(GEOID, modal_heat_source) %>%
    mutate(
        modal_heat_source = gsub("heat_", "", modal_heat_source),
        modal_heat_source = gsub("_rt", "", modal_heat_source)
        )
hampden_blockgroups_acs2020 <- left_join(hampden_blockgroups_acs2020,modal_heat)


## This is a short demo section about improving col_max
heat_source <- hampden_blockgroups_acs2020 %>%
    select(GEOID, owner_occupied_rt, heat_util_gas_rt, heat_electricity_rt) %>% 
    st_drop_geometry() 
modal_heat <- tidyfst::col_max(heat_source, .name="modal_heat_source")


## Plot some maps
## ggplot(hampden_blockgroups_acs2020) + geom_sf()

## As it happens, Tracts can neatly distinguish Holyoke from surrounding cities and towns.
holyoke_blockgroups_acs2020 <- filter(hampden_blockgroups_acs2020, substr(TRACTCE,1,4) %in% c("8114","8115","8116","8117","8118","8119","8120","8121"))
## ggplot(holyoke_blockgroups_acs2020) + geom_sf() + geom_sf_text(aes(label=GEOID))
## ggplot(holyoke_blockgroups_acs2020) + geom_sf(aes(fill=renter_occupied_rt))
## ggplot(holyoke_blockgroups_acs2020) + geom_sf(aes(fill=heat_oil_rt))


## Add some decoration: Voting districts (Wards), roads, and water
## Source: https://www2.census.gov/geo/tiger/TIGER2022/
## Get the 2020 Precinct Map of Hampden (from Census/Tigerline) and collapse to wards
hampden_VTD_2020 <- voting_districts(state="MA", county="013")
filter(hampden_VTD_2020,grepl('Holyoke', NAME20, ignore.case=TRUE )) %>% print(n=Inf)
holyoke_VTD_2020 <- filter(hampden_VTD_2020, as.numeric(substr(VTDST20,1,6))>=930, as.numeric(substr(VTDST20,1,6))<=943)
## ggplot(holyoke_VTD_2020) + geom_sf() + geom_sf_text(aes(label=NAME20))
holyoke_VTD_2020 <- holyoke_VTD_2020 %>% mutate(
                         ward = substr(NAME20,14,19)
                     ) %>%
    group_by(ward) %>%
    summarize()

## Get roads and limit to interstates and major roads in Holyoke
hampden_roads <- roads(state="MA", county="013") 
## s2::s2_options(model = "closed", snap = s2::s2_snap_level(30))
sf_use_s2(FALSE)  ## Aagh don't understand but needed. A GIS issue
holyoke_roads  <- hampden_roads %>% filter(MTFCC %in% c("S1100","S1200")) %>% st_intersection(holyoke_acs2020, hampden_roads, model="open")

## Get water and limit to Holyoke and major
## See https://www2.census.gov/geo/pdfs/maps-data/data/tiger/tgrshp2009/TGRSHP09AF.pdf for MTFCC codes
hampden_area_water <- area_water(state="MA", county="013")
holyoke_water <- st_intersection(hampden_area_water,holyoke_acs2020)

## Check roads, water, Wards
## ggplot(data=holyoke_VTD_2020) + geom_sf() + geom_sf_text(aes(label=ward)) +
##     geom_sf(data=holyoke_water, fill="blue") +
##     geom_sf(data=holyoke_roads, color="pink") + 
##     geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
##     geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))



## Map Renters, Oil Heat, Utility Gas Heat, Electric Heat, and Allocated Data for Heat
## Percent Renters
## ggplot(holyoke_blockgroups_acs2020) +
##     geom_sf(aes(fill=renter_occupied_rt), lwd=0) +
##     scale_fill_gradient(low="gray", high="yellow") +
##     geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
##     geom_sf(data=holyoke_water, fill="blue") +
##     geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
##     geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))


## Utility (HGE presumably) Gas heat
ggplot(holyoke_blockgroups_acs2020) +
    geom_sf(aes(fill=heat_util_gas_rt), lwd=0) +
    scale_fill_gradient(low="gray", high="yellow", limits=c(0,100)) +
    geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
    geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))

## Electric heat (presumably baseboard, not heat pump, but not explicitly documented)
ggplot(holyoke_blockgroups_acs2020) +
    geom_sf(aes(fill=heat_electricity_rt), lwd=0) +
    scale_fill_gradient(low="gray", high="yellow", limits=c(0,100)) +
    geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
    geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))

## Oil heat
ggplot(holyoke_blockgroups_acs2020) +
    geom_sf(aes(fill=heat_oil_rt), lwd=0) +
    scale_fill_gradient(low="gray", high="yellow", limits=c(0,100)) +
    geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
    geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))

## Modal (most common) heat source
ggplot(holyoke_blockgroups_acs2020) +
    geom_sf(aes(fill=modal_heat_source), lwd=0) +
    geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
    geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))

## ## Allocated (estimated) heat data
ggplot(holyoke_blockgroups_acs2020) +
    geom_sf(aes(fill=heat_estimated_rt), lwd=0) +
    scale_fill_gradient(low="gray", high="yellow", limits=c(0,100)) +
    geom_sf(data=holyoke_VTD_2020, color="darkgray", fill=NA, linewidth=1) +
    geom_sf(data=holyoke_water, fill="blue") +
    geom_sf(data=holyoke_roads, color="pink") + ## + geom_sf_text(data=holyoke_roads, aes(label=FULLNAME)) +
    geom_sf_text(data=holyoke_VTD_2020, aes(label=ward))
