# Packages ----
library(tidyverse)
library(gridExtra)

# Load data ----
alldf <- read_csv("Output/Data/allTraceData.csv", show_col_types = FALSE)
# place condition as first column
alldf <- alldf[ , c("condition", names(alldf)[names(alldf) != "condition"])]

# This is how the PCA is run ----
dfPCA <- alldf %>%
  unite("UniqID", "dataid", "trace", sep= "_", remove = FALSE)
# remove redundant columns
drops <- c("fraction", "dee", "estdee", "estsigma", "intensity")
dfPCA <- dfPCA[ , !(names(dfPCA) %in% drops)]
nbParamCols <- ncol(dfPCA) - 4

dfPCA <- dfPCA[is.finite(rowSums(dfPCA[ , 4 + 1:nbParamCols])), ]

pca_n <- dfPCA %>% group_by(condition) %>%
  summarise(Tracks = n_distinct(UniqID),
            Expts = n_distinct(dataid))

pdf("Output/Plots/data_pca_n.pdf")
  grid.table(pca_n, rows = NULL)
dev.off()
