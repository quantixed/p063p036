# Generate AlphaPlot graphics to visualise each dataset

# Packages ----
library(tidyverse)

# functions ----
find_allTM_data <- function (dirCond = "Output/Data", pattern= "*.csv"){
  # List all Condition names
  condname <- list.dirs(path = dirCond, recursive = FALSE, full.names = FALSE)
  # List all Condition paths
  condListPath <- list.dirs(path = dirCond, recursive = FALSE, full.names = TRUE)

  # List all files for each condition
  listfiles <- list()
  for (i in 1:length(condListPath)){
    fileList <-  list.files(path = condListPath[i], pattern = pattern, full.names = TRUE)
    listfiles[[i]] <- fileList
  }

  return(listfiles)
}

plot_tm_allTracks_dataset <- function(input, colourby = "trace", alphaLevel = 1, x0 = 0, xlim = NULL, y0 = 0, ylim = NULL) {
  x <- y <- dataid <- NULL
  colourby = sym(colourby)
  if(inherits(input, "list")) {
    df <- input[[1]]
  } else {
    df <- input
  }
  if (is.null(xlim)) xlim <- max(df$x,df$y)
  if (is.null(ylim)) ylim <- max(df$x,df$y)
  # offset tracks so that plotting is from 0 on the axes
  df$x <- df$x - x0
  df$y <- df$y - y0
  xlim <- xlim - x0
  ylim <- ylim - y0

  p <- ggplot(data = df, aes(x = x, y = y)) +
    geom_path(aes(group = trace, colour = !!colourby), alpha = alphaLevel, size = 0.2) +
    scale_colour_gradient2(
      low = "#9eb0fe",
      mid = "#190c0a",
      high = "#feadad",
      midpoint = 0,
      limits = c(-0.5,0.5),
      na.value = "#ffffffff",
      guide = "colourbar",
      aesthetics = "colour",
    ) +
    lims(x = c(0,xlim), y = c(ylim,0)) +
    labs(x = "", y = "") +
    coord_fixed() +
    theme_bw(8) +
    theme(legend.position = "none")

  return(p)
}

process_data <- function(x) {
  df <- read.csv(x)
  df <- df %>%
    unite("UniqID", "dataid", "trace", sep= "_", remove = FALSE)
  props <- properties %>% select(UniqID,alpha)
  df <- merge(df, props, by = "UniqID", all.x = TRUE, sort = FALSE)
  df <- df[!is.na(df$alpha),]
  df$alphas <- ifelse(df$alpha > 0.5, 0.5, df$alpha)
  df$alphas <- ifelse(df$alphas < -0.5, -0.5, df$alphas)
  allexpts <- unique(df$dataid)

  for(expt in allexpts) {
    p <- plot_tm_allTracks_dataset(input = df %>% filter(dataid == expt), colourby = "alphas")
    ggsave(paste0("Output/Plots/dataset_",expt,".pdf"), p, width = 8, height = 8, units = "cm", dpi = 300, bg = "white")
  }
}

process_data2 <- function(x, condition, dataset, x0 = 0, xlim = NULL, y0 = 0, ylim = NULL) {
  if(!grepl(condition, x, fixed = TRUE)) {
    return()
  }
  df <- read.csv(x)
  df <- df %>%
    unite("UniqID", "dataid", "trace", sep= "_", remove = FALSE)
  props <- properties %>% select(UniqID,alpha)
  df <- merge(df, props, by = "UniqID", all.x = TRUE, sort = FALSE)
  df <- df[!is.na(df$alpha),]
  df$alphas <- ifelse(df$alpha > 0.5, 0.5, df$alpha)
  df$alphas <- ifelse(df$alphas < -0.5, -0.5, df$alphas)

  p <- plot_tm_allTracks_dataset(input = df %>% filter(dataid == dataset),
                                 colourby = "alphas", x0 = x0, xlim = xlim, y0 = y0, ylim = ylim)
    ggsave(paste0("Output/Plots/aDataset_",dataset,".pdf"), p, width = 6, height = 6, units = "cm", dpi = 300, bg = "white")
}

# Script ----
# load the summary properties for each track from compareDatasets()
properties <- read.csv("Output/Data/allTraceData.csv")
# we need to make a column called UniqID to map to the track coordinate data
properties <- properties %>%
  unite("UniqID", "dataid", "trace", sep= "_", remove = FALSE)
# find all files called allTM.csv in subfolders of Data
files <- find_allTM_data(pattern = "allTM.csv")
# for each csv plot the dataset
lapply(files, plocess_data)
lapply(files, process_data2, condition = "Clathrin", dataset = "Clathrin_5", xlim = 8, ylim = 8)
lapply(files, process_data2, condition = "LAMP1", dataset = "LAMP1_2", x0 = 1, xlim = 9, y0 = 3.5, ylim = 11.5)
