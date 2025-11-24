## pdf(file="question-1.pdf",height=0,width=0, paper="USr")
library(here)
library(tidyverse)
library(tidycensus)
## Documentation https://walker-data.com/tidycensus/articles/basic-usage.html
## Get your own Census API Key https://api.census.gov/data/key_signup.html
census_api_key("cb9bd8756de7ba64b3c95ec0bd9193fc98d7cfe1")
library(tigris)
options(tigris_use_cache = TRUE)
options(width=200)


acsvars  <- load_variables("acs5", year=2020)
options(width=600, tibble.print_max=400)
filter(acsvars, grepl("median .* income", label,ignore.case=TRUE))
filter(acsvars, grepl("Estimate!!Total:$", label,ignore.case=TRUE)) 

income  <-  get_acs(year=2020, geography="county subdivision", state="MA",
                    output = "wide",  ## Estimates end in "E", Margins of error end in "M"
                    variables=c("B01001_001","B19013_001","B19113_001","B19101_017","B19101_001"),
                    geometry=TRUE)

inc  <- income %>% mutate(
                       Town = gsub(" town,.*", "", NAME),
                       Town = gsub(" city,.*", "", Town),
                       Town = gsub(" Town.*", "", Town),
                       `Pct family income > $200K` = B19101_017E/B19101_001E,
                       `Median family income` = B19113_001E
                   )

## Source: https://www.nytimes.com/interactive/2022/11/08/us/elections/results-massachusetts-question-1-create-new-income-tax-level.html
q1times  <- read_tsv(here("tidycensus-demo/mass-2022-question-1","question-1.csv"))

## Source: https://www.bostonglobe.com/elections/2022/massachusetts/question/1%20-%20create%20new%20income%20tax%20level/
q1globe  <- read_tsv(here("tidycensus-demo/mass-2022-question-1","question-1-globe.csv"))
q1globe  <- q1globe %>%
    mutate(No = ifelse(Type1=="No", Votes1, Votes2),
           Yes = ifelse(Type1=="Yes", Votes1, Votes2),
           `Pct Yes on 1` = Yes / (Yes + No),
           Winner = ifelse(Yes>No, "Yes", "No")
           )

q1inc  <- left_join(q1globe, inc)


q1inc  <- q1inc %>% arrange(`Median family income`) %>%
    mutate(poorrich = ifelse(row_number()<=50,1,ifelse(n()-row_number()<=50,2,NA))
           )



q1inc %>% ggplot(aes(x=`Median family income`,y=`Pct Yes on 1`)) + geom_point(aes(size=B01001_001E)) + geom_smooth()
q1inc %>% ggplot(aes(x=`Pct family income > $200K`,y=`Pct Yes on 1`)) + geom_point(aes(size=B01001_001E)) + geom_smooth()

q1inc %>% ggplot(aes(x=`Median family income`,y=`Pct Yes on 1`)) + geom_point(aes(size=B01001_001E)) + geom_text(aes(label=Town), size=3, hjust=-0.1) + geom_smooth()
q1inc %>% ggplot(aes(x=`Pct family income > $200K`,y=`Pct Yes on 1`)) + geom_point(aes(size=B01001_001E)) + geom_text(aes(label=Town), size=3, hjust=-0.1) + geom_smooth()

limit <- max( abs(q1inc$`Pct Yes on 1` - 0.5), na.rm=TRUE )
limit <- c(0.5-limit,0.5+limit)
q1inc %>% ggplot() + geom_sf(aes(geometry=geometry, fill=`Pct Yes on 1`)) + scale_fill_distiller(type = "div", limit = limit) + geom_sf(aes(geometry=geometry, color=poorrich),alpha=0)
