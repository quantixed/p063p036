## Written by Méghane Sittewelle
# edits by Stephen Royle

####### Call libraries #######
# purr to use the function "map"
if (!require(purrr)) {
  install.packages("purrr")
  require(purrr)
}

# ggplot2
if (!require(ggplot2)) {
  install.packages("ggplot2")
  require(ggplot2)
}

# dplyr, to use the function bind_rows that bind dataframes into one, or the function select
if (!require(dplyr)) {
  install.packages("dplyr")
  require(dplyr)
}

if (!require(ggforce)) {
  install.packages("ggforce")
  require(ggforce)
}

if (!require(cowplot)) {
  install.packages("cowplot")
  require(cowplot)
}


#########################.
######## VARIABLES ######

#########################.
# Functions
loadData= function(FileList) {
  
  allData = data.frame() #create new data frame
  
  for (j in 1:length(FileList)) {     #concatenate each all files 
    temp= read.delim(FileList[j], header = FALSE)
    allData = rbind(allData, temp)
  }
  
  return(allData)
}


#########################.
# Code 

FileList = list.files(path="Data", full.names = TRUE) #List all files of this directory
Median = data.frame()

for (i in 1:length(FileList)) {
  assign(paste0("df", i),loadData(FileList[i]))
  statstemp = summary(get(paste0("df", i)))
  Median = rbind(Median, median(get(paste0("df",i))[,1]))
}

colnames(Median)[1] = "Value" #rename new column

summary_df <- data.frame(x = c(rep(mean(Median$Value) - sd(Median$Value),2), rep(mean(Median$Value) + sd(Median$Value),2)),
                         y = c(0,5,5,0))

summary_line <- data.frame(x = rep(mean(Median$Value),2),
                           y = c(0,5))

p1 <- ggplot(Median, aes(x = Value)) +
  geom_histogram(binwidth = 0.06, colour = "black", fill = "grey") +
  geom_area(data = summary_df, aes(x = x, y = y), fill = "blue", alpha = 0.2) +
  geom_line(data = summary_line, aes(x = x, y = y), colour = "blue", alpha = 0.8) +
  labs(x = "Median distance to mitochondria (µm)", y = "Count") +
  lims(x = c(0,2.2)) +
  theme_cowplot(9)

####### Saving 
# save plot
ggsave ("Output/Plots/MedianDistancestoMito.pdf", p1, dpi=300, width = 6, height = 4  , units = "cm" ,  bg = NULL)

# save important df 
write.csv(Median, "Output/Data/MedianDistances.csv", row.names = FALSE)


