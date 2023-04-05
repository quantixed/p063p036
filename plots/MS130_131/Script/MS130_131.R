library(readxl)
library(dplyr)
library(ggbeeswarm)
library(cowplot)
library(rstatix)

# Read csv
xlfilepath <- list.files("Data","*.xlsx", full.names = TRUE)
Pool <- read_excel(xlfilepath[1])

# superplot 
DFsummary <- Pool %>% 
  group_by(Cond, Experiment) %>%
  summarise(mean = mean(DiffusionCoeff), sd = sd(DiffusionCoeff))

Condnamelevel <- unique(Pool$Cond)

# filter only before and 3min 
DFpool <- Pool
DFsummary <- DFsummary

p1 <- ggplot(DFpool, aes(x = factor(Cond, Condnamelevel))) + 
  geom_quasirandom(aes(y = as.numeric(DiffusionCoeff), colour = Experiment), size = 0.5) + #individual points spaced with density
  geom_errorbar(data = DFpool %>%
                  group_by(Cond) %>%
                  summarise(mean = mean(DiffusionCoeff), sd = sd(DiffusionCoeff)), 
                aes(y = mean, ymin = mean - sd, ymax = mean + sd), 
                width = 0.2) +   #errorbar
  geom_errorbar(data = DFpool %>%
                  group_by(Cond) %>%
                  summarise(mean = mean(DiffusionCoeff), sd = sd(DiffusionCoeff)), 
                aes(y = mean, ymin = mean, ymax = mean),
                width = 0.4, size = 1 ) +   #mean line
  geom_point(data = DFsummary, aes(y = mean, colour = Experiment), size = 2,  shape = 15, alpha = 0.5 ) + #Mean value
  geom_point(data = DFsummary, aes(y = mean), size = 2, shape = 0, colour= "black") +  #contour mean value
  geom_line(data = DFsummary, aes( y = mean, colour = Experiment, group = Experiment)) +   #for paired data
  scale_color_manual(values = c("#228833", "#ccbb44")) +
  labs(y = "Diffusion coefficient (µm2/s)", x = "") +
  theme_cowplot(9) +
  theme(legend.position = "none")

p1 

ggsave ("Pool_Diffusion.pdf", p1, path = "Output/Plots", width = 3.5, height = 4  , units = "cm" ,  bg = NULL)

# t-test on two groups
stat_test <- DFpool %>%
  t_test(DiffusionCoeff ~ Cond)

stat_test

