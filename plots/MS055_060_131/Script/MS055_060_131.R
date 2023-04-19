library(readxl)
library(dplyr)
library(ggbeeswarm)
library(cowplot)

# Read excel spreadsheet (first = only one in directory)
xlfilepath <- list.files("Data","*.xlsx", full.names = TRUE)
Pool <- read_excel(xlfilepath[1])

# Superplot comparison WT vs mut------

# Filter out LAMP1 data
DFpoolComp <- Pool %>%
  filter(!grepl('Lamp1',Expression))
# Add a column name to differentiate WT from mut
DF_WT <- DFpoolComp %>%
  filter(!grepl('R159E',Expression)) %>%
  mutate(TPD54 = "WT")
DF_MUT <- DFpoolComp %>%
  filter(grepl('R159E',Expression)) %>%
  mutate(TPD54 = "MUT")
DFpoolComp <- rbind(DF_WT, DF_MUT)

# make superplot DF
DFsummaryComp = DFpoolComp %>% 
  group_by( TPD54, Experiment) %>%
  summarise( mean = mean(DiffusionCoeff), sd = sd(DiffusionCoeff))

# Plot
Condnamelevel <- unique(DFpoolComp$TPD54)

p1 <- ggplot(DFpoolComp, aes(x = factor(TPD54, Condnamelevel))) + 
  geom_quasirandom(aes(y = as.numeric(DiffusionCoeff), colour= Experiment), size = 0.5) + #individual points spaced with density
  geom_errorbar(data = DFpoolComp %>% group_by(TPD54) %>% summarise(mean = mean(DiffusionCoeff), sd = sd(DiffusionCoeff)), 
                aes(y = mean, ymin=mean-sd, ymax=mean+sd), width= 0.2)+   # errorbar
  geom_errorbar(data = DFpoolComp %>% group_by(TPD54) %>% summarise(mean=mean(DiffusionCoeff), sd= sd(DiffusionCoeff)), 
                aes(y = mean, ymin=mean, ymax=mean), width= 0.4, linewidth = 1)+   # mean line
  geom_point(data = DFsummaryComp, aes(y = mean, colour = Experiment), size = 2,  shape = 15, alpha= 0.5 )+ #Mean value
  geom_point(data = DFsummaryComp, aes(y = mean), size = 2, shape = 0, colour= "black")+  #contour mean value
  scale_color_manual(values = c("#44aa99", "#117733", "#999933")) +
  scale_y_log10() +
  labs(y = "Diffusion coefficient (µm2/s)", x = "") +
  theme_cowplot(9) +
  theme(legend.position = "none")

p1

ggsave ("Pool_WTvsMUT.pdf", p1, path = "Output/Plots", width = 4, height = 4.5, units = "cm" ,  bg = NULL)
