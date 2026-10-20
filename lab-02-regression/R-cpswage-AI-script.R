### Write an R script using tidyverse that reads data from https://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta and suggest some ways to determine the return to education.

# R Script to Analyze Return to Education using CPS Data

# Load required libraries
library(tidyverse)
library(haven)      # for reading Stata files
library(broom)      # for tidy regression output
library(modelsummary) # for nice tables

# Read the data from URL
url <- "https://courses.umass.edu/econ753/berndt/stata/chap5-cps.dta"
cps_data <- read_dta(url)

# Explore the data
glimpse(cps_data)
summary(cps_data)

# Basic descriptive statistics
cps_data %>%
  select(lnwage, ed, ex) %>%
  summary()

# ===== METHOD 1: Simple Linear Regression =====
# Basic Mincer equation: lnwage = β0 + β1*ed + ε
model1 <- lm(lnwage ~ ed, data = cps_data)
summary(model1)

# ===== METHOD 2: Extended Mincer Equation =====
# Add experience and experience squared
model2 <- lm(lnwage ~ ed + ex + I(ex^2), data = cps_data)
summary(model2)

# ===== METHOD 3: Control for Demographics =====
# Add gender, race, union status if available
model3 <- lm(lnwage ~ ed + ex + I(ex^2) + 
               fe + nonwh + union, data = cps_data)
summary(model3)

# ===== METHOD 4: Education Categories =====
# Non-linear returns by education level
cps_data <- cps_data %>%
  mutate(ed_cat = case_when(
    ed < 12 ~ "Less than HS",
    ed == 12 ~ "High School",
    ed > 12 & ed < 16 ~ "Some College",
    ed >= 16 ~ "College+"
  ),
  ed_cat = factor(ed_cat, levels = c("Less than HS", "High School", 
                                          "Some College", "College+")))

model4 <- lm(lnwage ~ ed_cat + ex + I(ex^2), data = cps_data)
summary(model4)

# ===== METHOD 5: Quantile Regression =====
# Check if returns vary across wage distribution
library(quantreg)
model5 <- rq(lnwage ~ ed + ex + exsq, 
             tau = c(0.25, 0.5, 0.75), data = cps_data)
summary(model5)

# ===== Visualizations =====

# 1. Scatter plot: Education vs Log Wage
ggplot(cps_data, aes(x = ed, y = lnwage)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue") +
  labs(title = "Return to Education",
       x = "Years of Education",
       y = "Log Wage") +
  theme_minimal()

# 2. Compare models
modelsummary(list("Simple" = model1, 
                  "Mincer" = model2, 
                  "Full Controls" = model3),
             stars = TRUE,
             gof_map = c("nobs", "r.squared", "adj.r.squared"))

# 3. Predicted wages by education level
pred_data <- expand_grid(
  ed = 8:20,
  ex = mean(cps_data$ex, na.rm = TRUE)
)
pred_data$predicted_log_wage <- predict(model2, newdata = pred_data)
pred_data$predicted_wage <- exp(pred_data$predicted_log_wage)

ggplot(pred_data, aes(x = ed, y = predicted_wage)) +
  geom_line(color = "darkblue", size = 1) +
  labs(title = "Predicted Wage by Years of Education",
       x = "Years of Education",
       y = "Predicted Wage ($)",
       subtitle = "Holding experience constant at mean") +
  theme_minimal()

# Calculate marginal returns
pred_data <- pred_data %>%
  mutate(marginal_return = (predicted_wage - lag(predicted_wage)) / lag(predicted_wage) * 100)

print("Marginal Return to Each Year of Education:")
print(pred_data %>% select(ed, predicted_wage, marginal_return))


## Key Insights:

## 1. **Simple Model**: Each year of education increases wages by ~β₁ × 100%
## 2. **Mincer Equation**: Controls for experience (concave relationship)
## 3. **Full Controls**: Accounts for gender, race, union membership
## 4. **Non-linear**: Check if college degree has premium beyond years
## 5. **Quantile Regression**: Returns may differ for low vs high earners

## The coefficient on `ed` in log-wage models gives the percentage return to an additional year of education.


## Create scatterplot of lnwage vs. ed
ggplot(cps_data, aes(x = ed, y = lnwage)) +
  geom_point() +
  labs(x = "Education (years)", 
       y = "Log Wage",
       title = "Scatterplot of Log Wage vs. Education") +
  theme_minimal()

