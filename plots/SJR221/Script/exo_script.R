library(tidyverse)
library(cowplot)
library(pracma) # to use the function findpeaks
library(zoo)
library(patchwork)
library(ggbeeswarm)

source("Script/exo_functions.R")

# Variables ----

# Parameters for peakfinding
minpeakheight <- 10
threshold <- 9
threshold2 <- 0.7

# Load data ----
# Make a list of all conditions and files
datalist <- LISTCOND(dirCond = "Data")
datalistIntensity <- LISTCOND(dirCond = "Data", pattern = "*IntensityData.csv")
datalistInfo <- LISTCOND(dirCond = "Data", pattern = "*CellInfo.csv")

# Read csv files
READDATA(datalistInfo, list = FALSE, prefix = "info")
READDATA(datalistIntensity, list = FALSE, prefix = "intensity")

# Norm data and align peaks ----
# Norm data 
AllNormdata <- data.frame()
for ( i in 1 : length(datalistIntensity)) { #loopthrough conditions
  for ( j in 1 : length(datalistIntensity[[i]])){ #loopthrough files
    datatemp <- get(paste0("intensityCond", i, "_file",j ))
    Normdata <- DATANORM(datatemp)
    Normdata <- cbind(ID = paste0("intensityCond", i, "_file",j ), Normdata)
    Normdata <- cbind(Cond = paste0("Cond", i), Normdata)
    Normdata <- cbind(Condname = Condname[i], Normdata)
    
    # Save each df individually
    name <- paste0("norm_Cond", i, "_File", j)
    assign(name, Normdata)
    
    # Concatenate everything together in one big df
    AllNormdata <- rbind(AllNormdata, Normdata)
  }
}

# Retrieve name of files ----
Filenumber <- tibble()
n <- 0
for ( i in 1 : length(datalistIntensity)) { #loopthrough conditions
  for ( j in 1 : length(datalistIntensity[[i]])){ #loopthrough files
    n <- n + 1
    Filenumber[n,"ID"] <- paste0("Cond", i , "_file", j)
    Filenumber[n,"filename"] <- datalistIntensity[[i]][j]
  }
}

#Create unique name for each spot 
AllNormdata <- AllNormdata %>% mutate(UniqueIDspot = paste(ID, name, sep = "-"))
Unique_Spot <- unique(AllNormdata$UniqueIDspot)

#Loop pour chaque spot and find peak
AllNormdataCentered <- data.frame()
for (i in 1:length(Unique_Spot)){
  #Filter one unique spot
  spot <- Unique_Spot[i]
  SubsetAllNormdata <- AllNormdata %>% filter (UniqueIDspot == spot)

  
  #Find Peak and center at zero around the peak
  peaks <- FINDPEAK(Normdata = SubsetAllNormdata, minpeakheight = minpeakheight, threshold = threshold, threshold2 = threshold2)
  if (length(peaks)>0){
    max <- as.numeric(peaks$val)
    index <- as.numeric(peaks$index) 
    newFrameZero <- SubsetAllNormdata$frame[index]
    
    #Create new Frame column with Max centered to 0 and another with intensities scaled to 1
    SubsetAllNormdata <- SubsetAllNormdata %>%
      mutate(NewFrame = frame - newFrameZero, NewNorm = norm / max )
    
    #Reconcatenate everything
    AllNormdataCentered <- rbind(AllNormdataCentered, SubsetAllNormdata)
  }
}

# Add a column with time instead of Frame - take informations from infoCond file 
AllNormdataCentered_withTime <- data.frame()
for (i in 1 : length(datalistIntensity)) { #loopthrough conditions
  for (j in 1 : length(datalistIntensity[[i]])){ #loopthrough files
    filetemp <- paste0("intensityCond", i, "_file", j)
    temp <- AllNormdataCentered %>% filter((grepl(filetemp, ID)))
    infotemp <- get(paste0("infoCond", i, "_file", j))
    temp <- temp %>% mutate( RelativeTime = NewFrame*as.numeric(infotemp[2,2]))
    
    AllNormdataCentered_withTime <- rbind(AllNormdataCentered_withTime,temp )
  }
}
AllNormdataCentered <- AllNormdataCentered_withTime

# Calculate the mean with sd on scaled curves
AveragedScaledData <- data.frame()
for ( i in 1: length(datalistIntensity)) { #loopthrough conditions
  Condn <- paste0("Cond", i)
  DataTemp <- AllNormdataCentered_withTime %>%
    filter(grepl(Condn, Cond)) %>%
    group_by(NewFrame) %>%
    summarise(mean = mean(NewNorm),
              median = median(NewNorm),
              sd = sd(NewNorm),
              sem= sd(NewNorm) / sqrt(length(NewNorm)))
  DataTemp <- cbind(DataTemp, Cond = Condn)
  DataTemp <- cbind(DataTemp, Condname = Condname[i])
  AveragedScaledData <- rbind(AveragedScaledData,DataTemp)
}

# Save important df ----
write.csv(AllNormdataCentered, paste("Output/Data/", "AllNormdataCentered.csv"), row.names = FALSE)
write.csv(AveragedScaledData, paste("Output/Data/", "AveragedScaledData.csv"), row.names = FALSE)


# Plotting ----

exopoint <- AllNormdataCentered %>%
  filter(NewFrame == 0) %>%
  group_by(Condname) %>%
  summarise(mean=mean(norm),
            sd = sd(norm))
forStatTest <- AllNormdataCentered %>% 
  filter(NewFrame == 0) %>% 
  select(Condname,norm)
stat.test <- wilcox.test(forStatTest$norm[forStatTest$Condname == "Control"],
            forStatTest$norm[forStatTest$Condname == "Dongle"],
            alternative = "two.sided")

PeakSpotplot <- ggplot(forStatTest, aes(y = norm, x = Condname)) + 
  geom_quasirandom(aes(colour = Condname), size = 0.5, alpha = 0.75) +
  scale_colour_manual(values = c("#3f3f3f","#ab6ded")) +
  geom_errorbar(data = exopoint, 
                aes(y = mean, ymin = mean - sd, ymax = mean + sd), 
                width = 0.2) +   #errorbar
  geom_errorbar(data = exopoint, 
                aes(y = mean, ymin = mean, ymax = mean), 
                width = 0.4, size = 1 ) +   #mean line
  scale_y_continuous(limits = c(-0, 50), expand = c(0, 0)) +
  geom_text(aes(label = paste0("p = ",round(stat.test[["p.value"]],3)), x = Inf, y = Inf), size = 3, hjust = 1, vjust = 1, check_overlap = TRUE) +
  labs(y = "Peak amplitude (AU)", x ="") +
  theme_cowplot(9) +
  theme(legend.position="none")


# resampling and averaging ----

avgDF <- rbind(average_waves(df = AllNormdataCentered_withTime, str = "Control"),
               average_waves(df = AllNormdataCentered_withTime, str = "Dongle"))

#Plot with all curves in grey- facet by condition 
plotCenteredDataScaledVsTime <- ggplot() +
  geom_path(data = AllNormdataCentered_withTime, aes(x = RelativeTime, y = NewNorm, colour = Condname), alpha = 0.5) +
  scale_colour_manual(values = c("#3f3f3f","#ab6ded")) +
  facet_wrap(. ~ Condname, ncol = 1) + 
  scale_x_continuous(limits = c(-1.5, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0, 0)) +
  ylab(label = "Normalized intensity") +
  xlab(label = "Time (s)") +
  theme_cowplot(9) +
  theme(legend.position="none")

#Plot with all curves with average on top 
plotCenteredDataScaled_withaverage_overlayVsTime <- plotCenteredDataScaledVsTime +
  geom_line(data = avgDF, aes(x = t, y = avg, color = Condname), size = 1) +
  geom_ribbon(data = avgDF , aes(x = t, ymin = avg - sd, ymax = avg + sd, fill = Condname), alpha =0.5) +
  scale_fill_manual(values = c("#3f3f3f","#ab6ded"))

plotAveragedScaledDataVsTime <- ggplot() +
  geom_ribbon(data = avgDF , aes(x = t, ymin = avg - sd, ymax = avg + sd, fill = Condname), alpha =0.5) +
  geom_line(data = avgDF, aes(x = t, y = avg, color = Condname), size = 1, alpha = 0.75) +
  scale_x_continuous(limits = c(-1.5, 2), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0, 0)) +
  scale_colour_manual(values = c("#3f3f3f","#ab6ded")) +
  scale_fill_manual(values = c("#3f3f3f","#ab6ded")) +
  ylab(label = "Normalized intensity") +
  xlab(label = "Time (s)") +
  theme_cowplot(9) +
  theme(legend.position="none")

plotCenteredDataScaled_withaverage_overlayVsTime | plotAveragedScaledDataVsTime / PeakSpotplot

#save last plot
ggsave ("AlignedMeanPeaks_time.png", path = "Output/Plots", dpi = 300, width = 15, height = 15  , units = "cm" ,  bg = "white")

# Plot with all curves facet by condition 
plotCenteredDataVsTime <- ggplot() +
  geom_path(data = AllNormdataCentered_withTime, aes(x = RelativeTime, y = norm, colour = Condname), alpha = 0.5) +
  scale_colour_manual(values = c("#3f3f3f","#ab6ded")) +
  facet_wrap (. ~ Condname, ncol = 1) + 
  scale_x_continuous(limits = c(-1.5, 2), expand = c(0, 0)) +
  ylab(label = "Intensity") +
  xlab(label = "Time (s)") +
  theme_cowplot(9) +
  theme(legend.position="none")

plotCenteredDataVsTime | PeakSpotplot / plotAveragedScaledDataVsTime

#save last plot
ggsave ("AlignedMeanPeaks_time2.png", path = "Output/Plots", dpi = 300, width = 15, height = 15  , units = "cm" ,  bg = "white")

sparklines <- ggplot() +
  geom_vline(xintercept = 0, colour = "#efefef") +
  geom_path(data = AllNormdataCentered_withTime, aes(x = RelativeTime, y = norm, colour = Condname)) +
  scale_colour_manual(values = c("#3f3f3f","#ab6ded")) +
  facet_wrap(. ~ UniqueIDspot, nrow = 9) +
  scale_x_continuous(limits = c(-1.5, 2), expand = c(0, 0)) +
  ylab(label = "Intensity") +
  xlab(label = "Time (s)") +
  theme_cowplot(9) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

sparklines | PeakSpotplot / plotAveragedScaledDataVsTime

#save last plot
ggsave ("AlignedMeanPeaks_time3.png", path = "Output/Plots", dpi = 300, width = 15, height = 15  , units = "cm" ,  bg = "white")
ggsave ("AlignedMeanPeaks_time3.pdf", path = "Output/Plots", width = 15, height = 15  , units = "cm", bg = NULL)

# save individual plots for figure
ggsave("peakSpotPlot.pdf", PeakSpotplot, path = "Output/Plots", width = 3.5, height = 4, units = "cm", bg = NULL)
ggsave("plotAveragedScaledDataVsTime.pdf", plotAveragedScaledDataVsTime, path = "Output/Plots", width = 3.5, height = 4, units = "cm", bg = NULL)
ggsave("plotAveragedScaledDataVsTime_legend.pdf", plotAveragedScaledDataVsTime + theme(legend.position = "left"), path = "Output/Plots", width = 3.5, height = 4, units = "cm", bg = NULL)

sparklines2 <- ggplot() +
  geom_vline(xintercept = 0, colour = "#efefef") +
  geom_path(data = AllNormdataCentered_withTime, aes(x = RelativeTime, y = norm, colour = Condname), size = 0.25) +
  scale_colour_manual(values = c("#3f3f3f","#ab6ded")) +
  facet_wrap(. ~ UniqueIDspot, ncol = 10) +
  scale_x_continuous(limits = c(-1.5, 2), expand = c(0, 0)) +
  ylab(label = "Intensity") +
  xlab(label = "Time (s)") +
  theme_cowplot(4) +
  theme(legend.position="none",
        strip.background = element_blank(),
        strip.text.x = element_blank())

ggsave("sparklines.pdf", sparklines2, path = "Output/Plots", width = 7.8, height = 5, units = "cm", bg = NULL)
