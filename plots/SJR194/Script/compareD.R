library(tidyverse)

results <- read.csv("Output/Data/allComparison.csv")

results$t <- as.factor(rep(c(10,60,100), each = 60))
results$D <- rep(c(0.01,0.05,0.1,0.25,0.5,0.75,1.0,1.5,2.0,3.0), each = 6)
results$N <- as.factor(rep(c(1000,5000,7774,10000,16982,50000), times = 10))

results %>%
  filter(N != 7774, N != 16982) %>%
  ggplot(aes(x = D, y = dee, group = N, colour = N)) +
  geom_line() +
  geom_abline(slope = 1, linetype = 2, colour = "grey") +
  scale_x_log10(limits = c(0.003,10)) +
  scale_y_log10(limits = c(0.003,10)) +
  labs(x = "Ground truth (D)", y = "TrackMate (D)") +
  facet_wrap(. ~ t) +
  coord_fixed() +
  theme_minimal(9)

ggsave("Output/Plots/concordance.pdf", width = 17, height = 10, units = "cm")
