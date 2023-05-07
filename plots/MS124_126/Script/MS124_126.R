library(TrackMateR)

# compare bleached and unbleached
compareDatasets()

library(ggplot2)
library(ggforce)
library(cowplot)
library(patchwork)

# if this has been done, load the output to plot the density
# Load the summary data from TrackMateR to make the other plots----
# the plots were made by TrackMateR already, however the order and colours need to match the HC result
df <- read.csv("Output/Data/allComparison.csv")
msddf <- read.csv("Output/Data/allMSDCurves.csv")
units <- c("um","s")

# plot diffusion constant
p_dee <- ggplot(data = df, aes(x = condition, y = dee, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = c("#999933","#117733")) +
  ylim(c(0,NA)) +
  labs(x = "", y = substitute(paste("Diffusion coefficient (",mm^2,"/",nn,")"), list(mm = units[1], nn = units [2]))) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot neighbour density
p_density <- ggplot(data = df, aes(x = condition, y = neighbours, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = c("#999933","#117733")) +
  ylim(c(0,NA)) +
  labs(x = "", y = paste0("Median track density")) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot msd summary curves altogether
msddf$qual <- ifelse((msddf$condition == "SG-TPD54" & msddf$t > 0.9) |
                 (msddf$condition == "SG-TPD54_F" & msddf$t > 0.5), NA,1)
msddf <- na.omit(msddf)
  
p_msd <- ggplot(data = msddf, aes(x = t, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = condition), alpha = 0.2) +
  geom_line(aes(colour = condition), linewidth = 0.5) +
  scale_colour_manual(values = c("#999933","#117733")) +
  scale_fill_manual(values = c("#999933","#117733")) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Time (s)", y = "MSD") +
  theme_cowplot(8) +
  theme(legend.title = element_blank(), legend.position = c(0.5, 0.2))

r_report <- p_density | p_dee | p_msd
ggsave("Output/Plots/bleach_compare_plots.pdf", plot = r_report, width = 11, height = 5, units = "cm")
