# Package names
packages <- c("ggplot2", "ggforce", "patchwork", "plyr", "dplyr", "tidyr", "cowplot")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

## Used functions 
loadData <- function (nameCond, dirCond){
  #List all files
  FileList <- list.files(path = dirCond, pattern = " * trajectories.csv")
  
  #Load each file
  DataList <- list() # create empty list to store the files
  
  for (i in 1:length(FileList)){
    filename <- paste(nameCond[1],"_file", i, sep = "" ) 
    DataList[[filename]] <- read.csv(paste(dirCond, FileList[i], sep = "")) #append each dataframe into the list, with as a name filename
  }
  return(DataList)
}



## Variables 
#Indicate the number of condition to compare:
nbCond <- 6

#Indicate the name that you want to give to your conditions:
nameCond <- c("BeforeNoco1", "BeforeNoco2", "BeforeNoco3", "AfterNoco1", "AfterNoco2", "AfterNoco3")

#Indicate the path of the folders contaning your data in the same order of the name provided before. 

dirCond1 <- "Data/Data Noco/BeforeNoco_crop2_window10/" #put the name of your folders here
dirCond2 <- "Data/Data Noco/BeforeNoco_crop1_window10/" #put the name of your folders here
dirCond3 <- "Data/Data Noco/BeforeNoco_crop1_001/" #put the name of your folders here

dirCond4 <- "Data/Data Noco/AfterNoco_crop2_window10/" #put the name of your folders here
dirCond5 <- "Data/Data Noco/AfterNoco_crop1_window10/" #put the name of your folders here
dirCond6 <- "Data/Data Noco/AfterNoco_crop1_043/" #put the name of your folders here

# Core of the code {.tabset}

## Load data
# Load Data
for (loopcondition in 1:nbCond){
  ### Load data of each condition ----
  ## Retrive the list made by the function loadData
  dirCondtemp <- get(paste("dirCond", loopcondition, sep = ""))
  assign(paste("DataCond", loopcondition, sep = ""),loadData(nameCond = nameCond[loopcondition], dirCond = dirCondtemp) )
  
  ## Extract data frame from the list  ----
  Datacondtemp <- get(paste("DataCond", loopcondition, sep = "")) # will extract a list of DatacondX
  for (i in 1:length(Datacondtemp)) {     #Loop through each file
    assign(paste("DataCond", loopcondition, "class", i, sep = ""), get(paste("DataCond", loopcondition, sep = ""))[[i]])
  }
}


## Regroup all data in one big data frame for each condition ----
for (i in 1:nbCond) { #loop through conditions
  dfCondtemp <- data.frame()
  for (j in 1:length(get(paste0("DataCond",i)))){   #loop through classes
    DataCondtemp <- get(paste0("DataCond",i,"class",j))
    dfCondtemp <- rbind.fill(dfCondtemp,DataCondtemp )
  }
  assign(paste0("dfCond", i), dfCondtemp)
}


## Characterise the classes 
#### Caculate occurence of each class

## Calculate occurence of each class for each condition ----
OccurenceClassCond <- data.frame()
for (i in 1:nbCond){
  dfCondtemp <- get(paste0("dfCond",i))
  OccurenceClassCondtemp <- data.frame(table(dfCondtemp$CLASS) / length(dfCondtemp$CLASS) * 100) #table() allows to count nb of occurence
  colnames(OccurenceClassCondtemp)[1] <- "CLASS" #rename column
  OccurenceClassCondtemp <- cbind(OccurenceClassCondtemp, data.frame(Cond = nameCond[i]))
  # assign(paste0("OccurenceClassCond",i), OccurenceClassCondtemp)
  OccurenceClassCond <- bind_rows(OccurenceClassCond,OccurenceClassCondtemp)
}


plotFreqClass <- ggplot(OccurenceClassCond, aes(fill = CLASS, y = Freq, x = Cond)) + 
  geom_bar(position = "stack", stat = "identity") + 
  theme_classic()

# create big data frame of all conditions
dfCond <- data.frame()
for ( i  in 1:nbCond){
  dfCondtemp <- cbind(get(paste0("dfCond",i)), data.frame(COND = nameCond[i]))
  dfCond <- bind_rows(dfCond, dfCondtemp)
}

## Plot the different parameters of TraJ with all parameters and classes:----
dfCondnoNA <- dfCond[ , colSums(is.na(dfCond)) == 0]
parameters <- colnames(dfCond)
for (i in parameters){
  plot <- ggplot(dfCondnoNA, aes_string(x = "COND", y = i, fill = "COND" )) + 
    geom_violin() + 
    geom_sina(size = 0.5) + 
    theme_classic() + 
    theme(legend.position = "none")
  
  assign(paste0("plot_", i), plot)
}

#filter the class directed / active
DirectedClass <- filter(dfCond, CLASS == "DIRECTED/ACTIVE")
DirectedClass <- DirectedClass[ , colSums(is.na(DirectedClass)) == 0] # remove columns with NA values in it

## Plot the different parameters of TraJ of Directed Class: ----
parameters <- colnames(DirectedClass)
for (i in parameters){
  plot <- ggplot(DirectedClass, aes_string(x = "COND", y = i, fill = "COND" )) + 
    geom_violin() + 
    geom_sina(size = 0.5) + 
    theme_classic() + 
    theme(legend.position = "none")
  
  assign(paste0("plotDirected_", i), plot)
}

# Filter out the directed / active class
AllClassExceptDirect <- filter(dfCond, CLASS != "DIRECTED/ACTIVE")
AllClassExceptDirect <- AllClassExceptDirect[ , colSums(is.na(AllClassExceptDirect)) == 0] # remove columns with NA values in it

## Plot the different parameters of TraJ of NOT Directed Class:
parameters <- colnames(AllClassExceptDirect)
for (i in parameters){
  plot <- ggplot(AllClassExceptDirect, aes_string(x = "COND", y = i, fill = "COND" )) + 
    labs(title = "AllClassExceptDirect") + 
    geom_violin() + 
    geom_sina(size = 0.5) + 
    theme_classic() + 
    theme(legend.position = "none")
  
  assign(paste0("plotNOTdirected_", i), plot)
}

# Get the maximum efficiency by COND: ----
MaxEff <- AllClassExceptDirect %>% 
  group_by(COND) %>% 
  mutate(Max_eff = max(EFFICENCY)) %>%
  summarise( MAXeff = max(Max_eff))

Threeshold <- median(MaxEff$MAXeff)

# Threeshold all the data by the maximum efficiency found previously to keep only the "straight and directive" ones ----

AllClassFilterByEff <- filter(dfCond, EFFICENCY > Threeshold)
AllClassFilterByEff <- AllClassFilterByEff[ , colSums(is.na(AllClassFilterByEff)) == 0] # remove columns with NA values in it

knitr::kable(head(AllClassFilterByEff))

## Plot the different parameters of TraJ with efficiency threeshold: ----
parameters <- colnames(AllClassFilterByEff)
for (i in parameters){
  plot <- ggplot(AllClassFilterByEff, aes_string(x = "COND", y = i, fill = "COND" )) + 
    labs(title = "AllClassFilterByEff") + 
    geom_violin() + 
    geom_sina(size = 0.5) + 
    theme_classic() + 
    theme(legend.position = "none")
  
  assign(paste0("plotThreeshold_", i), plot)
}

# Calculate the %efficiency above 1.5 ( = threeshold) on the total for each condition:  ----
fractioncount <- data.frame()
for (i in 1:nbCond) {
  CountTemp <- dfCond %>%  #take all the conditions then %>%
    filter(COND == nameCond[i]) %>%  #filter condition per condition then %>%
    count(EFFICENCY > Threeshold)  #count the number of values above the threeshold and total
  
  fractioncounttemp <- cbind(COND = nameCond[i], FracAboveThreeshold = (CountTemp[2,2] / sum(CountTemp$n)) * 100)
  fractioncount <- rbind(fractioncount, fractioncounttemp) # concatenate all fraction count for all condition
  Fractioncount <- fractioncount %>% separate(COND, c("Treatment", "replicate"), sep = "(?<=[A-Za-z])(?=[0-9])") #Separate the name between letters and numerics
}

#calculate for each group the mean and sd of the fractAbove threeshold
FractionAboveThreeshold <- Fractioncount %>% 
  group_by(Treatment) %>% 
  summarise(MEANFracAboveThreeshold = mean(as.numeric(FracAboveThreeshold)), SD = sd(as.numeric(FracAboveThreeshold)) ) #calculate for each group the mean and sd of the fractAbove threeshold


# I want control first: 
FractionAboveThreeshold <- FractionAboveThreeshold %>%
  mutate(Treatment = reorder(Treatment, desc(Treatment)))
Fractioncount <- Fractioncount %>%
  mutate(Treatment = reorder(Treatment, desc(Treatment)))


# plotFraction = ggplot(FractionAboveThreeshold, aes(x = Treatment, y = MEANFracAboveThreeshold, fill = Treatment)) + 
#   labs(x = "", y = paste0("Tracks above ", Threeshold, " efficiency (%)")) + 
#   geom_bar(stat = "identity") + 
#   geom_errorbar(aes(ymin = MEANFracAboveThreeshold-SD, ymax = MEANFracAboveThreeshold + SD), width = .2) + 
#   theme_classic()

myColours <- c("#117733","#aa4499")

plotFraction <- ggplot() + 
  geom_point(data = Fractioncount, aes(x = Treatment, y = as.numeric(FracAboveThreeshold), colour = Treatment)) + #Replicate points
  scale_color_manual(values = myColours) + 
  geom_errorbar(data = FractionAboveThreeshold, aes(x = Treatment, y = MEANFracAboveThreeshold, ymin = MEANFracAboveThreeshold-SD, ymax = MEANFracAboveThreeshold + SD), width = .2) + #Error bar
  geom_errorbar(data = FractionAboveThreeshold, 
                aes(x = Treatment, y = MEANFracAboveThreeshold, ymin = MEANFracAboveThreeshold, ymax = MEANFracAboveThreeshold), 
                width = 0.4, size = 1 ) +   #mean line
  ylim(0,NA) + 
  labs(x = "", y = paste0("Tracks above ", Threeshold, " efficiency (%)")) + 
  theme_cowplot(9) + 
  theme(legend.position = "none") #remove legend

plotFraction


# save last plot
ggsave("MS099_Ranalysis_plotFraction.pdf", path = "Output/Plots", dpi = 300, width = 33, height = 50  , units = "mm" ,  bg = NULL)

#Print plot
plotFreqClass

# save last plot
ggsave("MS099_Ranalysis_FreqClass.pdf", path = "Output/Plots", dpi = 300, width = 40, height = 20  , units = "cm" ,  bg = NULL)

# Display plots for all class:
(plot_STRAIGHTNESS | plot_EFFICENCY | plot_FRACT..DIM. | plot_KURTOSIS) / 
  (plot_LENGTH | plot_X.FIT..ALPHA | plot_SPEED | plot_TRAPPEDNESS) / 
  (plot_Asymmetry | plot_GAUSSIANITY | plot_MSDRatio | plot_X.FIT..D)

# save last plot
ggsave("MS099_Ranalysis_parameters_directed.pdf", path = "Output/Plots", dpi = 300, width = 40, height = 20  , units = "cm" ,  bg = NULL)

# Display plots for directed class:
(plotDirected_STRAIGHTNESS | plotDirected_EFFICENCY | plotDirected_FRACT..DIM. | plotDirected_KURTOSIS) / 
  (plotDirected_LENGTH | plotDirected_X.FIT..ALPHA | plotDirected_SPEED | plotDirected_TRAPPEDNESS) / 
  (plotDirected_Asymmetry | plotDirected_GAUSSIANITY | plotDirected_MSDRatio | plotDirected_X.FIT..D)

# save last plot
ggsave("MS099_Ranalysis_parameters_directed.pdf", path = "Output/Plots", dpi = 300, width = 40, height = 20  , units = "cm" ,  bg = NULL)

# Display plots for all EXCEPT directed class:
(plotNOTdirected_STRAIGHTNESS | plotNOTdirected_EFFICENCY | plotNOTdirected_FRACT..DIM. | plotNOTdirected_KURTOSIS)

# Display plots for after threeshold of maxEfficiency :
(plotThreeshold_STRAIGHTNESS | plotThreeshold_EFFICENCY | plotThreeshold_FRACT..DIM. | plotThreeshold_KURTOSIS)

plotFreqClass / (plot_STRAIGHTNESS | plot_EFFICENCY | plotFraction)

# save last plot
ggsave("MS099_Ranalysis_Final.pdf", path = "Output/Plots", dpi = 300, width = 40, height = 20  , units = "cm" ,  bg = NULL)

# save important df 
write.csv(dfCond, paste("Output/Data/", "MS099_AllConditions.csv"), row.names = FALSE)
write.csv(dfCondnoNA, paste("Output/Data/", "MS099_AllConditions_noNA.csv"), row.names = FALSE)
write.csv(OccurenceClassCond, paste("Output/Data/", "MS099_OccurenceClassCond.csv"), row.names = FALSE)
write.csv(FractionAboveThreeshold, paste("Output/Data/", "MS099_FractionAboveThreeshold.csv"), row.names = FALSE)
write.csv(Fractioncount, paste("Output/Data/", "MS099_FractionAboveThreeshold_replicates.csv"), row.names = FALSE)
