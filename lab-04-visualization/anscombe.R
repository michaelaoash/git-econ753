pdf(file="anscombe.pdf", paper="USr", height=0, width=0)

library(tidyverse)

anscombe

anscombe_tidy <- anscombe %>%
    mutate(observation = seq_len(n())) %>%
    gather(key, value, -observation) %>%
    separate(key, c("variable", "set"), 1, convert = TRUE) %>%
    mutate(set = c("I", "II", "III", "IV")[set]) %>%
    spread(variable, value) %>% arrange(set,observation)

anscombe_tidy

summarize(group_by(anscombe_tidy,set),
          mean(x), sd(y), mean(y), sd(y), cor(x,y), alpha=coef(lm(y ~ x))[1], beta=coef(lm(y ~ x))[2], R2=summary(lm(y ~ x))$r.squared 
          )

ggplot(anscombe_tidy, aes(x, y)) +
    geom_point() +
    facet_wrap(~ set) +
    geom_smooth(method = "lm", se = FALSE)

