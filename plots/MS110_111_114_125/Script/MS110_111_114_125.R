library(readxl)
library(dplyr)
library(stringr)
library(ggbeeswarm)
library(cowplot)
library(rstatix)

# Read csv
xlfilepath <- list.files("Data","*.xlsx", full.names = TRUE)
Pool <- read_excel(xlfilepath[1])

# superplot 
DFsummary <- Pool %>% 
  group_by(SimpleCond, Experiment) %>%
  summarise(mean = mean(NbEventsperArea), sd = sd(NbEventsperArea))

Condnamelevel <- unique(Pool$SimpleCond)

# filter only before and 3min 
DFpool <- Pool %>% filter( grepl('before|3min',SimpleCond)) 
DFsummary <- DFsummary %>% filter( grepl('before|3min',SimpleCond))

p1 <- ggplot(DFpool, aes(x = factor(SimpleCond, Condnamelevel))) + 
  geom_quasirandom(aes(y = as.numeric(NbEventsperArea), colour = Experiment), size = 0.5) + #individual points spaced with density
  geom_errorbar(data = DFpool %>%
                  group_by(SimpleCond) %>%
                  summarise(mean = mean(NbEventsperArea), sd = sd(NbEventsperArea)), 
                aes(y = mean, ymin = mean - sd, ymax = mean + sd), 
                width = 0.2) +   #errorbar
  geom_errorbar(data = DFpool %>%
                  group_by(SimpleCond) %>%
                  summarise(mean = mean(NbEventsperArea), sd = sd(NbEventsperArea)), 
                aes(y = mean, ymin = mean, ymax = mean),
                width = 0.4, size = 1 ) +   #mean line
  geom_point(data = DFsummary, aes(y = mean, colour = Experiment), size = 2,  shape = 15, alpha = 0.5 ) + #Mean value
  geom_point(data = DFsummary, aes(y = mean), size = 2, shape = 0, colour= "black") +  #contour mean value
  geom_line(data = DFsummary, aes( y = mean, colour = Experiment, group = Experiment)) +   #for paired data
  scale_color_manual(values = c("#228833", "#ccbb44", "#ee6677","#aa3377")) +
  labs(y = "NbEvents/um2", x = "") +
  theme_cowplot(9) +
  theme(legend.position = "none")

p1 

ggsave ("Pool_NbExocytosisEvents.pdf", p1, path = "Output/Plots", width = 7.5, height = 5.5  , units = "cm" ,  bg = NULL)

# t-test on two groups
stat_test_wt <- DFpool %>%
  dplyr::filter(str_detect(SimpleCond,"^WT")) %>% 
  t_test(NbEventsperArea ~ SimpleCond)

stat_test_wt

stat_test_mCh <- DFpool %>%
  dplyr::filter(str_detect(SimpleCond,"^mCh")) %>% 
  t_test(NbEventsperArea ~ SimpleCond)

stat_test_mCh

