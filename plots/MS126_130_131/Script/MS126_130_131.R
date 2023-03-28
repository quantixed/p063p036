library(readxl)
library(dplyr)
library(ggbeeswarm)
library(cowplot)

# Read csv
xlfilepath <- list.files("Data","*.xlsx", full.names = TRUE)
Pool <- read_excel(xlfilepath[1])

# NbEventsperArea is for 20 s, multiply by 3 for per min
Pool$NbEventsperArea <- Pool$NbEventsperArea * 3

# superplot 
DFsummary <- Pool %>% 
  group_by(SimpleCond, Experiment) %>%
  summarise(mean = mean(NbEventsperArea), sd = sd(NbEventsperArea))

Condnamelevel <- unique(Pool$SimpleCond)

# filter only before and 3min 
DFpool <- Pool
DFsummary <- DFsummary

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
  scale_color_manual(values = c("#44aa99", "#117733", "#999933")) +
  scale_y_continuous(limits = c(0,NA)) +
  labs(y = "NbEvents/um2", x = "") +
  theme_cowplot(9) +
  theme(legend.position = "none")

#theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p1 

ggsave ("Pool_NbExocytosisEvents.pdf", p1, path = "Output/Plots", width = 3.5, height = 4 , units = "cm" ,  bg = NULL)

