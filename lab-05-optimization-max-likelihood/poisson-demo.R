### Write R code using tidyverse methods that plots a bar plot for the theoretical poisson distribution using dpois and a simulation using rpois for a given value of lambda, the arrival rate.

## Here's R code using tidyverse to compare theoretical and simulated Poisson distributions:

library(tidyverse)

# Set parameters
lambda <- 3.2
n_simulations <- 100

# Define x range
x_range <- 0:15

# Create theoretical distribution
theoretical <- tibble(
  x = x_range,
  probability = dpois(x, lambda),
  type = "Theoretical"
)

# Create simulated distribution
simulated <- tibble(
  x = rpois(n_simulations, lambda)
) %>%
    count(x) %>%
    mutate(probability = n / sum(n)) %>%
    complete(x = x_range, fill = list(n = 0, probability = 0)) %>%
    mutate(type = "Simulated") %>%
  select(x, probability, type)

# Combine and plot
bind_rows(theoretical, simulated) %>%
  ggplot(aes(x = x, y = probability, fill = type)) +
    geom_col(position = position_dodge2(preserve = "single"), alpha = 0.7) +
    scale_x_continuous(breaks = 0:15) +
    scale_fill_manual(values = c("Theoretical" = "steelblue",
                                 "Simulated" = "coral")) +
  labs(
    title = paste("Poisson Distribution: Theoretical vs Simulated (λ =", lambda, ", N =", n_simulations, ")"),
    x = "Number of Events",
    y = "Probability",
    fill = "Distribution"
  ) +
  theme_minimal() +
  theme(legend.position = "top")
