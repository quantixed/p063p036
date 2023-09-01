# Script which will first analyse all data using TrackMateR and then do PCA
# The project on GitHub requires the TrackMate XML files from zenodo to be placed into Data/
# The PCA part needs the file allTraceData.csv in Output/Data/ folder
# Written by Stephen Royle & Meghane Sittewelle
# Modified 10 July 2023

# Packages ----
library(tidyverse)
library(ggforce)
library(patchwork)
library(reshape2)
library(FactoMineR)
library(cowplot)
# install/update TrackMateR
devtools::install_github("quantixed/TrackMateR")
library(TrackMateR)

# Use TrackMateR ----
# this part can take a while...
compareDatasets()

# Load data ----
alldf <- read_csv("Output/Data/allTraceData.csv", show_col_types = FALSE)
# place condition as first column
alldf <- alldf[ , c("condition", names(alldf)[names(alldf) != "condition"])]

# Run the PCA ----
dfPCA <- alldf %>%
  unite("UniqID", "dataid", "trace", sep= "_", remove = FALSE)
# remove redundant columns - expanded because in TrackMateR v 0.3.8 onwards more outputs are generated
drops <- c("fraction", "dee", "estdee", "estsigma", "intensity")
dfPCA <- dfPCA[ , !(names(dfPCA) %in% drops)]
nbParamCols <- ncol(dfPCA) - 4

dfPCA <- dfPCA[is.finite(rowSums(dfPCA[ , 4 + 1:nbParamCols])), ]

res.pca <- PCA(dfPCA[ , 4 + 1:nbParamCols])

plot(res.pca, cex = 0)

# extract pc scores for first two component and add to dataframe
dfPCA$pc1 <- res.pca$ind$coord[ , 1] # indexing the first column
dfPCA$pc2 <- res.pca$ind$coord[ , 2] # indexing the second column
dfPCA$pc3 <- res.pca$ind$coord[ , 3] # indexing the third column

# We also need to extract the data for the variable contributions to each of the pc axes.
pca.vars <- res.pca$var$coord %>%
  data.frame
pca.vars$vars <- rownames(pca.vars)
pca.vars.m <- melt(pca.vars, id.vars = "vars")

# By convention, the variable contribution plot has a circle around the variables that has a radius of 1. Here’s some code to make one.
circleFun <- function(center = c(0,0), diameter = 1, npoints = 100) {
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ <- circleFun(c(0,0),2,npoints = 500)

# Plot individual values of the PCA ----

# Plot
plotPCA <- ggplot(data = dfPCA, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = condition), alpha = 0.25, size = 0.5, shape = 16) +
  coord_fixed() +
  labs(x = "PC1", y = "PC2") +
  theme_cowplot(8) +
  theme(legend.title=element_blank())

# Plot the variables of the PCA
VarLabel <- colnames(dfPCA[ , 4 + 1:nbParamCols])

vars_p <- ggplot() +
  geom_path(data = circ,aes(x,y), lty = 2, color = "grey", alpha = 0.7) +
  geom_hline(yintercept = 0, lty = 2, color = "grey", alpha = 0.7) +
  geom_vline(xintercept = 0, lty = 2, color = "grey", alpha = 0.7) +
  geom_segment(data = pca.vars, aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2),
               arrow = arrow(length = unit(0.025, "npc"), type = "open"),
               lwd = 0.5) +
  geom_text(data = pca.vars,
            aes(x = Dim.1 * 1.2, y =  Dim.2 * 1.2, label = VarLabel),
            check_overlap = F, size = 2) +
  labs(x = "PC1", y = "PC2") +
  coord_equal() +
  xlim(-1.5,1.5) +
  theme_cowplot(8) +
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill= "transparent"))

# Print plots ----

plotPCA | vars_p
# save last plot
ggsave ("PCAinitial.pdf", path = "Output/Plots", dpi=300, width = 17, height = 12  , units = "cm" ,  bg = NULL)


# Get only a subset of data to plot - resampling ----
# Get the limiting condition - how much value is plotted for each target?
NbValperTarget <- dfPCA %>%
  group_by(condition) %>%
  mutate (nrowTarget = n()) %>%
  summarise(nrow= unique(nrowTarget))

minPts = min(NbValperTarget$nrow)

set.seed(1) # Allows to plot always the same thing even if we ask for random values

dfPCAsubset <- dfPCA %>%
  group_by(condition) %>%
  slice_sample(n = minPts, replace = FALSE)

# we return to this later...

# Plot PCA result with stat_density to give contours ----

# Manually group
groups <- c(1,2,3)
group3 <- "EB3"
group2 <- c("Rab5","LAMP1")
group1 <- c("Rab35", "Clathrin", "SCAMP1", "TPD54", "SCAMP3", "Rab30", "ATG9A", "Rab11", "ML1N")

dfPCAgroup <- tibble()
for ( i in groups) {
  group <- get(paste0("group", i))
  dfPCAgrouptemp <- dfPCA %>%
    filter(condition %in% group)
  dfPCAgrouptemp$group <- as.factor(i)
  dfPCAgroup <- rbind(dfPCAgroup, dfPCAgrouptemp)
}
# Change order
order <- factor(c(group1,group2,group3))
dfPCAgroup$condition <- factor(dfPCAgroup$condition, levels = order)

# plot in Figure uses bins = 10
bins <- 10
myColours <- c("#3681c2", "#e78745", "#852fae")
plotDensity <- ggplot(data = dfPCAgroup, aes(x = pc1, y = pc2)) +
  stat_density2d(aes(colour = group), bins = bins, linewidth = 0.25) +
  scale_colour_manual(values = myColours) +
  facet_wrap(vars(condition), nrow = 4, ncol = 3) +
  lims(x = c(-3,8), y = c(-4,4)) +
  coord_fixed() +
  labs(x = "PC1", y = "PC2") +
  theme_cowplot(8) +
  theme(legend.position = "none")
plotDensity

# save last plot
ggsave (paste0("PCAdensity_bin", bins,".pdf"), path = "Output/Plots", dpi=300, width = 7.7, height = 10.5, units = "cm" ,  bg = NULL)

# plot of a subsample (equal number for each condition)
myColours2 <- c(rep(myColours[1],9), rep(myColours[2],2), rep(myColours[3],1))
dfPCAsubset$condition <- factor(dfPCAsubset$condition, levels = order)
plotDensityResampled <- ggplot(data = dfPCAsubset, aes(x = pc1, y = pc2)) +
  geom_point(aes(colour = condition), alpha = 0.2, size = 0.2) +
  scale_colour_manual(values = myColours2) +
  facet_wrap(vars(condition)) +
  coord_fixed() +
  lims(x = c(-3,8), y = c(-4,4)) +
  labs(x = "PC1", y = "PC2") +
  theme_cowplot(8) +
  theme(legend.position = "none")
plotDensityResampled

# save last plot
ggsave ("PCA_resampled.pdf", path = "Output/Plots", width = 17, height = 12  , units = "cm" ,  bg = NULL)

# Get the all line coordinates of each density for bin = 15  ----
# this is so we can do hierarchical clustering after exporting this data
conditionsNames <- unique(dfPCA$condition) # Get all conditions names
coordinatesAll <- tibble() #initialise df
for ( i in conditionsNames) {
  # Filter dfPCA to get one condition by one condition
  dfPCA_filtered <- dfPCA %>%
    filter (condition == i)

  #Get the list used to plot
  ggplotList <- ggplot_build(ggplot(data = dfPCA_filtered, aes(x = pc1, y = pc2)) +
                                stat_density2d(aes(x = pc1, y = pc2, colour=..group..), bins = 15) +
                                xlim(-3,8) +
                                ylim(-4,4))

    # Extract the data out of the list as a dataframe
    dfDataggplot <- as.data.frame(ggplotList$data[[1]])

    #Add information to the list
    #coordinatesOuter = dfDataggplot %>% filter( group == "-1-002-001")
    dfDataggplot$bins <- 15 # add a column with bin number
    dfDataggplot$condition <- i #add a column with the name of the condition
    coordinatesAll <- rbind(coordinatesAll,dfDataggplot )

}

# this is used as input for Igor Pro code to do hierarchical clustering
write.csv(coordinatesAll, "Output/Data/coordinatesAll.csv", row.names = FALSE)
# n numbers for the number of tracks
write.csv(NbValperTarget, "Output/Data/tracksPerCondition.csv", row.names = FALSE)

# Load the summary data from TrackMateR to make the other plots----
# the plots were made by TrackMateR already, however the order and colours need to match the HC result
df <- read.csv("Output/Data/allComparison.csv")
msddf <- read.csv("Output/Data/allMSDCurves.csv")
units <- c("um","s")

df$condition <- factor(df$condition, levels = order)
msddf$condition <- factor(msddf$condition, levels = order)

# plot alpha comparison
p_alpha <- ggplot(data = df, aes(x = condition, y = alpha, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = myColours2) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey") +
  scale_y_continuous(limits = c(0.5,2), trans = "log2") +
  guides(x =  guide_axis(angle = 90)) +
  labs(x = "", y = "Mean alpha") +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot speed comparison
p_speed <- ggplot(data = df, aes(x = condition, y = speed, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = myColours2) +
  ylim(c(0,NA)) +
  guides(x =  guide_axis(angle = 90)) +
  labs(x = "", y = paste0("Mean speed (",units[1],"/",units[2],")")) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot diffusion constant
p_dee <- ggplot(data = df, aes(x = condition, y = dee, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = myColours2) +
  ylim(c(0,NA)) +
  guides(x =  guide_axis(angle = 90)) +
  labs(x = "", y = substitute(paste("Diffusion coefficient (",mm^2,"/",nn,")"), list(mm = units[1], nn = units [2]))) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot neighbour density
p_density <- ggplot(data = df, aes(x = condition, y = neighbours, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = myColours2) +
  ylim(c(0,NA)) +
  guides(x =  guide_axis(angle = 90)) +
  labs(x = "", y = paste0("Median track density")) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot fractal dimension
p_fd <- ggplot(data = df, aes(x = condition, y = fd, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = myColours2) +
  ylim(c(0,NA)) +
  guides(x =  guide_axis(angle = 90)) +
  labs(x = "", y = paste0("Median fractal dimension")) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot width of track summary
p_width <- ggplot(data = df, aes(x = condition, y = width, colour = condition)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(alpha = 0.5, stroke = 0, size = 1) +
  scale_colour_manual(values = myColours2) +
  ylim(c(0,NA)) +
  guides(x =  guide_axis(angle = 90)) +
  labs(x = "", y = paste0("Median track width")) +
  theme_cowplot(8) +
  theme(legend.position = "none")

# plot msd summary curves altogether
# trim ends
msddf$qual <- ifelse(msddf$condition != "EB3" & msddf$t > 1.2, NA, 1)
msddf <- na.omit(msddf)

p_msd <- ggplot(data = msddf, aes(x = t, y = mean)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = condition), alpha = 0.2) +
  geom_line(aes(colour = condition), linewidth = 0.5) +
  scale_fill_manual(values = myColours2) +
  scale_colour_manual(values = myColours2) +
  scale_x_log10(limits = c(NA,3)) +
  scale_y_log10() +
  labs(x = "Time (s)", y = "MSD") +
  theme_cowplot(8) +
  theme(legend.position = "none")

# r_report <- (p_alpha + p_speed + p_dee) / (p_fd + p_width + p_density) / (p_msd + plot_spacer() + plot_spacer())
# r_report <- (p_alpha + p_speed) / (p_fd + p_density) / (p_dee + p_msd)
r_report <- (p_density + p_speed) / (p_alpha + p_msd)

ggsave("Output/Plots/all_tm_plots.pdf", plot = r_report, width = 13, height = 8, units = "cm")
# patchwork makes msd plot a bit weird. plot it separately.
ggsave("Output/Plots/msd_only.pdf", plot = p_msd, width = 6.2, height = 3.8, units = "cm")

ggplot(alldf, aes(x = intensity)) +
  geom_histogram(binwidth = 8) +
  facet_wrap(. ~ condition, scales = "free") +
  lims(x = c(0,500)) +
  labs(x = "Fluorescence intensity (AU)", y = "Frequency") +
  theme_cowplot(9)
# intensity plot for Supp Fig
ggsave("Output/Plots/all_int.pdf", width = 16, height = 12, units = "cm")