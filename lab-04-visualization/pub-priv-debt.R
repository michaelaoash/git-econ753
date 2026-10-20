## Write R code that will read the contents of https://www.federalreserve.gov/releases/z1/dataviz/z1/nonfinancial_debt/table/ into R using tidyverse.

library(tidyverse)
library(lubridate)
library(rvest)
library(httr)

url <- "https://www.federalreserve.gov/releases/z1/dataviz/z1/nonfinancial_debt/table/"

# Read the webpage
page <- read_html(url)

# Extract the table - the data is in a table element
debt_table <- page %>%
  html_element("table") %>%
  html_table()

# Clean column names
debt_table <- debt_table %>%
  janitor::clean_names()

# If you need to handle the date column or numeric conversions:
debt_table_clean <- debt_table %>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  mutate(across(-1, ~parse_number(., na = c("", "NA", "--"))))

# View the result
glimpse(debt_table_clean)
```

If the table structure is more complex or you need to extract specific elements, here's an alternative approach:

```r
library(tidyverse)
library(rvest)

url <- "https://www.federalreserve.gov/releases/z1/dataviz/z1/nonfinancial_debt/table/"

# Read and parse
page <- read_html(url)

# Extract all tables (in case there are multiple)
all_tables <- page %>%
  html_elements("table") %>%
  map(html_table, fill = TRUE)

# Get the first/main table
debt_data <- all_tables[[1]]

# Clean and process
debt_data_clean <- debt_data %>%
  rename_with(~str_replace_all(., "\\s+", "_")) %>%
  rename_with(tolower) %>%
  mutate(date = yq(quarter))

debt_data_clean %>%
  select(date, 
         `households_and_nonprofits;_percent_of_gdp_(%)`,
         `nonfinancial_business;_percent_of_gdp_(%)`,
##         `nonfinancial_corporate_business;_percent_of_gdp_(%)`,
##         `nonfinancial_noncorporate_business;_percent_of_gdp_(%)`,
         `federal_government;_percent_of_gdp_(%)`,
         `state_and_local_governments;_percent_of_gdp_(%)`) %>%
  pivot_longer(cols = -date, 
               names_to = "sector", 
               values_to = "percent_gdp") %>%
ggplot(aes(x = date, y = percent_gdp, color = sector)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Debt as Percent of GDP Over Time",
       x = "Date",
       y = "Percent of GDP (%)",
       color = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Create data frame with presidential terms (adjust dates as needed)
presidential_terms <- data.frame(
    name = c("Truman", "Eisenhower", "Kennedy", "Johnson", "Nixon", "Ford", "Carter", "Reagan", "Bush", "Clinton", "Bush", "Obama", "Trump", "Biden"),
    start = as.Date(c("1945-04-12", "1953-01-20", "1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20", "1989-01-20",   "1993-01-20", "2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20")),
    end = as.Date(c("1953-01-20", "1961-01-20", "1963-11-22", "1969-01-20", "1974-08-09", "1977-01-20", "1981-01-20", "1989-01-20",   "1993-01-20", "2001-01-20", "2009-01-20", "2017-01-20", "2021-01-20", "2025-01-20")),
    party = c("Democratic", "Republican", "Democratic", "Democratic", "Republican", "Republican", "Democratic", "Republican", "Republican",  "Democratic", "Republican", "Democratic", "Republican", "Democratic")
)

## Extend the presidential term code as far back as Truman



debt_data_clean %>%
  select(date, 
         `households_and_nonprofits;_percent_of_gdp_(%)`,
         `nonfinancial_business;_percent_of_gdp_(%)`,
##         `nonfinancial_corporate_business;_percent_of_gdp_(%)`,
##         `nonfinancial_noncorporate_business;_percent_of_gdp_(%)`,
         `federal_government;_percent_of_gdp_(%)`,
         `state_and_local_governments;_percent_of_gdp_(%)`) %>%
  pivot_longer(cols = -date, 
               names_to = "sector", 
               values_to = "percent_gdp") %>%
ggplot(aes(x = date, y = percent_gdp, color = sector, label = sector)) +
  geom_rect(data = presidential_terms,
            aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = party),
            alpha = 0.2, inherit.aes = FALSE) +
  scale_fill_manual(values = c("Democratic" = "blue", "Republican" = "red"),
                    name = "President") +
    geomtextpath::geom_textline() +
    guides(label = "none") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(title = "Debt as Percent of GDP Over Time",
       x = "Date",
       y = "Percent of GDP (%)",
       color = "Sector") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




