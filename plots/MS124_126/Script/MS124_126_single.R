library(TrackMateR)

# compare bleached and unbleached
# compareDatasets()

library(ggplot2)
library(ggforce)
library(cowplot)
library(patchwork)
library(dplyr)

# if this has been done, load the output to plot the density
# Load the summary data from TrackMateR to make the other plots----
alldf <- read.csv("Output/Data/allTraceData.csv")
# filter for only SG-TPD54_F
alldf <- alldf[alldf$condition == "SG-TPD54_F", ]

tmdf <- read.csv("Output/Data/SG-TPD54_F/allTM.csv")
msddf <- read.csv("Output/Data/SG-TPD54_F/allMSD.csv")
msdmean <- read.csv("Output/Data/allMSDCurves.csv")
msdmean <- msdmean[msdmean$condition == "SG-TPD54_F", ]
units <- c("µm","s")

# plot alpha ----
alphas <- na.omit(alldf$alpha)
# within a sensible range (log 2)
identify <- alphas <= 4 & alphas >= -4
# we will take log2
# alphas <- data.frame(alpha = alphas[identify,])
alphas <- subset(alphas, identify)
# convert back to real numbers
median_alpha <- 2^(median(alphas, na.rm = TRUE))

p_alpha <- ggplot(data = alldf, aes(x = alpha)) +
  geom_histogram(binwidth = 0.1) +
  geom_text(aes(label = paste0("median = ",format(round(median_alpha,3), nsmall = 3)), x = min(alpha, na.rm = TRUE), y = Inf), size = 3, hjust = 0, vjust = 1, check_overlap = TRUE) +
  labs(x = "alpha (log2)", y = "Frequency") +
  lims(x = c(-2,2)) +
  theme_cowplot(9) +
  theme(legend.position = "none")

# plot speed ----
xstr <- paste0("Speed (",units[1],"/",units[2],")")
ystr <- "Frequency"
speedDF <- alldf %>% 
  select(trace, cumdist, cumtime)
speedDF$speed <- speedDF$cumdist / speedDF$cumtime
median_speed <- median(speedDF$speed, na.rm = TRUE)
nBin <- max(floor(1 + log2(nrow(speedDF))),30)
  
p_speed <- ggplot(data = speedDF, aes(x = speed)) +
  geom_histogram(bins = nBin) +
  geom_text(aes(label = paste0("median = ",format(round(median_speed,3), nsmall = 3)), x = max(speed, na.rm = TRUE), y = Inf), size = 3, hjust = 1, vjust = 1, check_overlap = TRUE) +
  labs(x = xstr, y = ystr) +
  theme_cowplot(9) +
  theme(legend.position = "none")
  

# plot msd summary curves altogether
msddf$qual <- ifelse(msddf$t > 0.5, NA,1)
msddf <- na.omit(msddf)
mod <- lm(mean ~ t, data = msdmean[1:4,])
# make a column containing model y points for each t
msdmean$pred <- (mod$coefficients[2] * msdmean$t) + mod$coefficients[1]
# calculate diffusion constant (D)
dee <- msdmean$pred[1] / (4 * msdmean$t[1])
# add line to show MSD with alpha = 1

# p_msd <- ggplot(data = msddf, aes(x = t, y = mean)) +
#   geom_line(aes(group = dataid), colour = "#117733", linewidth = 0.5, alpha = 0.5) +
#   geom_ribbon(data = msdmean, aes(ymin = mean - sd, ymax = mean + sd), fill = "#117733", alpha = 0.2) +
#   geom_line(data = msdmean, aes(x = t, y = mean), colour = "#117733", size = 1) +
#   geom_line(data = msdmean, aes(x = t, y = pred), colour = "grey", linetype = 2) +
#   scale_x_log10(limits = c(NA,0.5)) +
#   scale_y_log10() +
#   labs(x = "Time (s)", y = "MSD") +
#   geom_text(aes(label = paste0("D = ",format(round(dee,3), nsmall = 3)), x = min(msdmean$t, na.rm = TRUE), y = Inf), size = 3, hjust = 0, vjust = 1, check_overlap = TRUE) +
#   theme_cowplot(9) +
#   theme(legend.position = "none")

p_msd <- ggplot(data = msddf, aes(x = t, y = mean)) +
  geom_ribbon(data = msdmean, aes(ymin = mean - sd, ymax = mean + sd), fill = "#117733", alpha = 0.2) +
  geom_line(data = msdmean, aes(x = t, y = mean), colour = "#117733", size = 1) +
  scale_x_log10(limits = c(NA,0.5)) +
  scale_y_log10() +
  labs(x = "Time (s)", y = "MSD") +
  geom_text(aes(label = paste0("D = ",format(round(dee,3), nsmall = 3)), x = min(msdmean$t, na.rm = TRUE), y = Inf), size = 3, hjust = 0, vjust = 1, check_overlap = TRUE) +
  theme_cowplot(9) +
  theme(legend.position = "none")


r_report <- p_alpha | p_msd | p_speed
ggsave("Output/Plots/three_plots.pdf", plot = r_report, width = 13, height = 5, units = "cm")
