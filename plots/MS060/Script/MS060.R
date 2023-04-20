#.--------------------------------------------------------.
# Translated code for FLIP experiment - from IgorCode 
# This code is working from csv files obtained after the Macro ImageJ "Macro_FLIP"
# Need a folder with FLIP measurement and timestamps (both csv files) altogether in the same folder 
# Updated 19.04.23
#.--------------------------------------------------------.

# Dependencies ----
library(tidyverse)
library(patchwork)
library(minpack.lm)
library(plyr)
library(cowplot)
library(ggforce)

# Functions ----
LISTCOND <- function (dirCond, pattern = "*.csv"){
  # List all Conditions names
  Condname <<- list.dirs(path = dirCond, recursive = FALSE, full.names = FALSE)
  # List all Conditions path
  CondListPath <<- list.dirs(path = dirCond, recursive = FALSE, full.names = TRUE)
  
  # List all files for each condition
  listfiles <- list()
  for (i in 1:length(CondListPath)){
    FileList <-  list.files(path= CondListPath[i], pattern = pattern)
    name <- paste0("Cond", i )
    listfiles[[name]] <- FileList
  }
  return(listfiles)
}

#' READDATA - Function to read all csv files from different conditions. It uses function: LISTCOND
#'
#' @param listcond List coming from function LISTCOND. List of names' files for each condition.
#' @param list Boolean argument. If TRUE, returns a list of dataframes with all csv files. 
#' If false, assigns to each csv file a dataframe in the global environment. 
#' @param prefix To use if list=FALSE to add a prefix to the name of the df.

READDATA <- function (listcond, list = FALSE, prefix = "") {
  if(list == TRUE) {
    biglistofdf <- list()
    for(i in 1:length(listcond)) {   #loop through each condition 
      dircond <- CondListPath[i]
      for (j in 1:length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        data_cond_test <- read_csv(paste(dircond, dirfile, sep="/"), show_col_types = FALSE )
        name <- paste0("Cond", i, "_file", j)
        biglistofdf[[name]] <- data_cond_test
      }
    } 
    return(biglistofdf)
  }
  else {
    for(i in 1:length(listcond)) {   #loop through each condition 
      dircond <- CondListPath[i]
      for(j in 1:length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        assign(paste0(prefix, "Cond", i, "_file", j), read_csv(paste(dircond, dirfile ,sep="/"),show_col_types = FALSE ), envir = .GlobalEnv)
      }
    }
  }
}

cbind.fill <- function(...) {
  transpoted <- lapply(list(...),t)
  transpoted_dataframe <- lapply(transpoted, as.data.frame)
  return (data.frame(t(rbind.fill(transpoted_dataframe))))
}

# Load data ----

# Make a list of all conditions and files
# We only have a single condition for this script so data are in Data/TPD54/
datalistCond <- LISTCOND(dirCond = "Data")
datalistIntensity <- LISTCOND(dirCond = "Data", pattern = "*results.csv")
datalistTime <- LISTCOND(dirCond = "Data", pattern = "*TimeStamp.csv")

# Read csv files
testList <- READDATA(datalistIntensity, list = FALSE, prefix = "Data")
TimeList <- READDATA(datalistTime, list = FALSE, prefix = "Time")


# Normalise Data with background and then to 1
for(loopcondition in 1:length(datalistIntensity)) { #loop through conditions
  # Get timestamp representative for the condition
  Timestamps <- get(paste("TimeCond", loopcondition, "_file", 1, sep = ""))
  Timestamps <- select(Timestamps, contains("Time")) #keep only the column with time in seconds
  
  # Initialise df for later
  Normalisedcells <- Timestamps
  
  for ( i in 1: length(datalistIntensity[[loopcondition]])){ #loopthrough files
    Datacondtemp <- get(paste("DataCond", loopcondition, "_file", i, sep="")) # get generic name of data
    
    Nbcell <- (ncol(Datacondtemp) - 3) / 4 #calcul number of cell in each file
    
    # Do a big data frame with all normalised cells
    for (j in 0:(Nbcell-1)) { #loop through each cell
      meancelltemp <- select(Datacondtemp, paste("Mean(entire_cell",j,")", sep="")) #use dplyr to select directly a column named xx
      backgroundtemp <- select(Datacondtemp, "Mean(Background)")
      
      Normalisedcelltemp <- (meancelltemp - backgroundtemp) / as.numeric((meancelltemp[1,] - backgroundtemp[1,]))
      colnames(Normalisedcelltemp)[1] <- paste("Norm_cond", loopcondition, "_file", i, "_cell", j, sep="")
      Normalisedcells <- cbind(Normalisedcells, Normalisedcelltemp)
    }
 
    
  }
  Normalisedcells[1] <- NULL
  assign(paste("NormalisedcellsCond", loopcondition, sep=""), Normalisedcells)  # Attribute condition in the name. Not clever, but I don't know how to do it.

  
  
  ### Individual plots
  ### Do one unique graph with all cell of the condition ----
  
  #Need first to reorganise the data into a long data frame to be able to plot all of them at once
  Data <- get(paste("NormalisedcellsCond", loopcondition, sep="")) #give generic name to be easier to work with
  Data <- cbind(Timestamps, Data) # Attach the timestamps to the values
  DFtall <- pivot_longer(data = Data, cols = contains("Norm")) #reorganise with tidyr: regroup all columns that contains "Mean" in the name
  
  #Plot on graph by condition
  plotConditiontemp <- ggplot(DFtall, aes(x=`Time(s)` , y=value, color=name))+
    geom_point() +
    labs(x = "Time(s)", y = "Relative intensity", title = "Allplots", subtitle = Condname[loopcondition],caption="Normalised" )
  
  assign(paste("plotCondition", loopcondition, sep=""), plotConditiontemp) #Create a new plot wiht a different name for each loop (which correspond to each condition)
}

# 
# # FITS ------------------------------
nbCond <- length(Condname)
# Do fit on each curve ----

## single exponential fit: eDecay <- function(t, ampl, tau) (ampl*exp(-t/tau)) ----
# This function defines a simple exponential decay with starting amplitude “ampl” and following a decay lifetime of “tau”

for(loopcondition in 1:nbCond) { # loop through conditions
Timestamps <- get(paste("TimeCond", loopcondition, "_file", 1, sep=""))
Timestamps[1] <- NULL
  
  #Initialise data frames needed after
  allSimplefittemp <- Timestamps
  allDoublefittemp <- Timestamps
  dftemp <- get(paste("NormalisedcellsCond", loopcondition, sep=""))
  Taudf <- data.frame()
  Tau1df <- data.frame()
  Tau2df <- data.frame()

  for (i in 1:length(dftemp)) { # loop through column of NormalisedcellCond (or dftemp here)
    #define curve to fit
    y <- dftemp[,i]
    Time <- Timestamps$`Time(s)`

    #Determine the function
    simpleDecay <- function(x, ampl, tau) (ampl*exp(-x/tau)) # with x is the Time here
    
    #Do the fit with nls Nonlinear Least Squares to estimates the parameters
    # The nls has this form: nls( ExpData ~ TheoryFunction, data=DataFrame, parameter initial guesses)
    # ExpData is the column y from Test
    # Theory function where x is the Time from Test, and myA and myT correspond to initial guesses for ampl and tau
    simpleModel <- nlsLM(y ~ simpleDecay(Time,myA,myT), start = list(myA=1,myT=0.5))
    print(summary(simpleModel)) # rint in the console a summary of the fit
    sumSingle <- summary(simpleModel) # get summary of the fit

    #Create a data.frame from the model
    fitmodelS <- data.frame(x = Time , y = predict(simpleModel))
    allSimplefittemp <- cbind(allSimplefittemp, predict(simpleModel))
 
    #Quality of the fit : calcul of RSS
    RSS.p_s <- sum(residuals(simpleModel)^2)  # Residual sum of squares
    TSS_s <- sum((y - mean(y))^2)  # Total sum of squares
    RSM_s = 1 - (RSS.p_s/TSS_s) # R-squared measure
    print(paste("Quality of fit_ condition", loopcondition, Condname[loopcondition], "_cell", i, sep=""))
    print(paste("Residual sum of square=", RSS.p_s, SEP=""))
    print(paste("Total sum of squares=", TSS_s, SEP=""))
    print(paste("R-squared measure=", RSM_s, SEP=""))

    #Get Tau from single exponential to estimate initial Tau for the double exponential
    ParaSingle <- sumSingle$parameters
    Tau <- ParaSingle[2,1]

    Taudf <- rbind(Taudf, Tau) #concatenate all Tau in one data.frame
    assign(paste("TauCond", loopcondition, sep=""), Taudf)
  }
  assign(paste("allSimplefit_Cond", loopcondition, sep=""), allSimplefittemp)
}


#Comparison of Tau between conditions ----

allTau <- data.frame()
for (loopcondition in 1:nbCond){
  allTau <- cbind.fill(allTau, get(paste("TauCond", loopcondition, sep = ""))) #should add NA if not the same size
}

#rename columns
for (loopcondition in 1:nbCond){
  colnames(allTau)[loopcondition] <- Condname[loopcondition]
  # colnames(allTau1)[loopcondition] <- Condname[loopcondition]
  # colnames(allTau2)[loopcondition] <- Condname[loopcondition]
}

## Plot comparison Tau----
# Tau tall
Tautoplot <- pivot_longer(data = allTau, cols = everything())

plotTau <- ggplot(Tautoplot, aes(x = name, y = value)) +
  geom_boxplot( size = 0.5, alpha = 1, fill = "dodgerblue3") +
  geom_point(size=0.8) +
  labs(x = "", y ="Tau (s)" , title="Tau", subtitle = "Single exp fit" )+
  theme_classic()

# because we only have one condition:
median(Tautoplot$value)

# Calcul diffusion coefficient  ----
# Calcul halftime for each cell of the file : halftime=ln(2)*Tau

halftime <- data_frame()
for (loopcondition in 1:nbCond) { # loop through conditions
  TauCondtemp <- get(paste("TauCond", loopcondition, sep="")) #give generical name for simplicity
  halftimetemp <- TauCondtemp * log(2)
  halftime <- cbind.fill(halftime, halftimetemp)
  assign(paste("halftime_cond", loopcondition, sep=""), halftimetemp)
}


#rename columns
for (loopcondition in 1:nbCond){
  colnames(halftime)[loopcondition] <- Condname[loopcondition]
}

#plot 
halftimetoplot <- pivot_longer(data = halftime, cols = everything() )
plothalftime <- ggplot(halftimetoplot, aes(x = name, y = value)) +
  geom_boxplot( size = 0.5, alpha = 1, fill="white") +
  geom_point(size = 0.8) +
  labs(x = "", y ="Halftime (s)" , title="Halftime", subtitle = "single exponential") +
  theme_classic()

plothalftime
# because we only have one condition:
median(halftimetoplot$value)

# Generate averages curves and average fit  ----

# Calcul averages, sd, SEM for Normalised data and fits
AverageNormCond <- Timestamps #Initialise data frame with timestamps to have time to plot
sdNormCond <- Timestamps

for (loopcondition in 1:nbCond) { #loop over conditions
  Normalisedcellstemp <- get(paste("NormalisedcellsCond", loopcondition, sep = ""))

  #Do average accross rows
  AverageNormCondtemp <- rowMeans(Normalisedcellstemp)  #the function "apply" apply the function sd, accross row (indicated by MARGIN =1)
  AverageNormCond <- cbind(AverageNormCond,AverageNormCondtemp)
  colnames(AverageNormCond)[loopcondition + 1] <- paste("Cond", loopcondition, sep = "")

  # #Do sd accross rows
  sdNormCondtemp <- apply(Normalisedcellstemp, 1, sd ) #the function "apply" apply the function sd, accross row (indicated by MARGIN =1)
  sdNormCond <- cbind(sdNormCond,sdNormCondtemp)
  colnames(sdNormCond)[loopcondition + 1] <- paste("Cond", loopcondition, sep = "")
}


# Plot averages ----
# modify data frame to plot
AverageNormtoplot <- pivot_longer(data = AverageNormCond, cols = contains("Cond"))
sdNormtoplot <- pivot_longer(data = sdNormCond, cols = contains("Cond"))

p1 <- ggplot() +
  geom_line(data = AverageNormtoplot, aes(x = `Time(s)`, y = value), colour = "#117733", alpha = 0.5) +
  geom_ribbon(data = AverageNormtoplot, aes(x = `Time(s)`, ymin = value - sdNormtoplot$value, ymax = value + sdNormtoplot$value), fill = "#117733", alpha = 0.5) +
  labs(x = "Time (s)", y ="Relative intensity") +
  lims(y = c(0,1)) +
  theme_cowplot(9) +
  theme(legend.position = "none")

p1
ggsave("FLIP_Curve.pdf", p1, path = "Output/Plots", width = 6, height = 4, units = "cm",  bg = NULL)

# remake tau plot for single exp fit in style of other figures
p2 <- ggplot(Tautoplot, aes(x = name, y = value)) +
  geom_boxplot(colour = "grey", outlier.shape = NA) +
  geom_sina(colour = "#117733", alpha = 0.5, stroke = 0) +
  ylim(c(0,NA)) +
  labs(x = "", y = "Tau (s)") +
  theme_cowplot(8) +
  theme(legend.position = "none")

p2
ggsave("FLIP_Tau.pdf", p2, path = "Output/Plots", width = 2, height = 2.5, units = "cm",  bg = NULL)


# save important df ----
write.csv(AverageNormCond, paste0("Output/Data/", "AverageNormCond.csv"), row.names = FALSE)
write.csv(sdNormCond, paste0("Output/Data/", "SDNormCond.csv"), row.names = FALSE)
write.csv(allTau, paste0("Output/Data/", "Tau_singleFit.csv"), row.names = FALSE)





