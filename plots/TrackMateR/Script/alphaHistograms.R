# For this script we work with the output from TrackMateR
# I am using SJR215 as the basis for SJR222

# Packages ----
library(tidyverse)
library(cowplot)
# install/update TrackMateR
devtools::install_github("quantixed/TrackMateR")
library(TrackMateR)

# Load data ----
alldf <- read.csv("Output/Data/allTraceData.csv")

subdf <- alldf[alldf$condition == "TPD54" | alldf$condition == "Clathrin" | alldf$condition == "LAMP1", ]
subdf <- subdf[!is.na(subdf$alpha),]
sumdf <- subdf %>% filter(cumtime > 1) %>%
  group_by(condition) %>%
  summarise(alpha = 2^median(alpha),
            ntrack = n(),
            ncell = length(unique(dataid)))

make_label <- function(df,row) {
  s <- paste0(sumdf[row,1],"\n",
              names(sumdf)[2]," = ",round(sumdf[row,2],3),"\n",
              names(sumdf)[3]," = ",sumdf[row,3],"\n",
              names(sumdf)[4]," = ",sumdf[row,4])
  return(s)
}

make_label_from_data <- function(df) {
  sum_df <- df %>% group_by(condition) %>%
    summarise(alpha = 2^median(alpha),
              ntrack = n(),
              ncell = length(unique(dataid)))
  s <- paste0(sumdf[1,1],"\n",
              names(sum_df)[2]," = ",round(sum_df[1,2],3),"\n",
              names(sum_df)[3]," = ",sum_df[1,3],"\n",
              names(sum_df)[4]," = ",sum_df[1,4])
  return(s)
}

neworder <- c("TPD54","Clathrin","LAMP1")
subdf <- arrange(transform(subdf,condition = factor(condition, levels=neworder)),condition)

subdf %>% filter(cumtime > 1) %>%
  ggplot(aes(x = alpha, fill = ..x..)) +
  geom_histogram(binwidth = 0.1) +
  scale_colour_gradient2(
    low = "#9eb0fe",
    mid = "#190c0a",
    high = "#feadad",
    midpoint = 0,
    limits = c(-0.5,0.5),
    na.value = "#ffffffff",
    guide = "colourbar",
    aesthetics = "fill",
    oob = scales::squish
  ) +
  facet_wrap(. ~ condition, scale = "free_y") +
  labs(x = "alpha (log2)", y = "Frequency") +
  theme_cowplot(8) +
  theme(legend.position = "none")

ggsave("Output/Plots/alpha_three.pdf", width = 8.8, height = 4, units = "cm")
