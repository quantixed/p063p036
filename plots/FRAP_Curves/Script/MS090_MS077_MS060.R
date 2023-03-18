#.--------------------------------------------------------.
# Translated code for FRAP experiment - from IgorCode (and FLIP R code) 
# This code is working from csv files obtained after the Macro ImageJ "Macro_FRAP"
# Need 2 folders in `Data`: one with FRAP measurements (called `Results`) and one with timestamps (called `Measurements`)
# Each condition is in a folder, identical naming.
# The script will load and plot all single cells individually, then do the average, then do the fit on the average
# Updated 06.04.22, 18.03.23
#.--------------------------------------------------------.

#.--------------------------------------------------------.
# Library ----
library(minpack.lm)
library(dplyr) #to use the function select, and also for filter
library(plyr) # to use rbind.fill
library(ggplot2)
library(tidyr) #to use the function pivot longer
library(patchwork) #library for plot_layout
library(ggforce) #library used for sina 
library(cowplot)


## Setup preferred directory structure in wd
if(!dir.exists("Data")) {dir.create("Data")}
if(!dir.exists("Output")) {dir.create("Output")}
if(!dir.exists("Output/Data")) {dir.create("Output/Data")}
if(!dir.exists("Output/Plots")) {dir.create("Output/Plots")}


#.--------------------------------------------------------.
# Used functions ----

# Load data
loadData <- function (nameCond, dirCond){
  #List all files
  FileList <- list.files(path = dirCond)
  
  #Load each file
  DataList <- list() # create empty list to store the files
  
  for (i in 1:length(FileList)){
    filename <- paste(nameCond[1],"_file", i, sep = "" ) 
    DataList[[filename]] <- read.csv(paste(dirCond, FileList[i], sep = "")) #append each dataframe into the list, with as a name filename
  }
  return(DataList)
}

# # cbind function with NA if different size
# cbind.fill <- function(...) {                                                                                                                                                       
#   transpoted <- lapply(list(...),t)                                                                                                                                                 
#   transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
#   return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
# } 

#.--------------------------------------------------------.
# Main function ----

load_and_process_data <- function(nameCond, myColours, myLines, suffix, err) {
  # standard
  safeNameCond <- sub(" ","",nameCond)
  nbCond <- length(nameCond)
  nbptsBeforeBleach <- 3
  startfit <- nbptsBeforeBleach + 1 # start after the bleach
  rn <- 2 # rn = radius of the bleached region in micrometers;
  re <- 2 # re = radius effective post-bleaching in micrometers. If bleaching instaneous: rn = re"
  
  #Load: loop through each condition 
  for (loopcondition in 1:nbCond){
    ### Load data of each condition ----
    ## Retrieve the list made by the function loadData
    dirCondtemp <- paste0("Data/Results/",nameCond[loopcondition],"/")
    assign(paste("DataCond", loopcondition, sep = ""),loadData(nameCond = safeNameCond[loopcondition], dirCond = dirCondtemp) )
    
    ## Extract data frame from the list  ----
    Datacondtemp <- get(paste("DataCond", loopcondition, sep = "")) # will extract a list of DatacondX
    for (i in 1:length(Datacondtemp)) {     #Loop through each file
      assign(paste("DataCond", loopcondition, "file", i, sep = ""), get(paste("DataCond", loopcondition, sep = ""))[[i]])
    }
  }
  
  ## Normalise Data with background and then to 1 ----
  
  for (loopcondition in 1:nbCond) { #loop through conditions
    Datacondtemp <- get(paste("DataCond", loopcondition, sep = "")) # will extract a list of DatacondX
    
    # Get representative timestamp
    dirTimeStamptemp <- paste0("Data/Timestamps/",nameCond[loopcondition],"/")
    FileListTimestamp <- list.files(path = dirTimeStamptemp) #List all files of this directory timestamps
    Timestamps <- read.csv(paste(dirTimeStamptemp, FileListTimestamp[1], sep = ""))  #read all the first csv file of the directory dirTimeStamp
    Timestamps <- select(Timestamps, contains("Time") ) #keep only the column with time in seconds
    Timestamps <- Timestamps - Timestamps[startfit,]
    colnames(Timestamps)[1] <- "time"
    
    Normalisedcells <- Timestamps
    
    for (i in 1:length(Datacondtemp)) { # loop through each file
      Datacondfiletemp <- get(paste("DataCond", loopcondition, "file", i,  sep = ""))
      Nbcell <- ncol(Datacondfiletemp)-3
      
      # Do a big data frame with all normalised cells - set limit on number of rows to equal timestamps
      for (j in 0:(Nbcell-1)) { #loop through each cell
        meancelltemp <- select(Datacondfiletemp, paste("Mean.cell_",j,".", sep = "")) #use dplyr to select directly a column named xx
        backgroundtemp <- select(Datacondfiletemp, "Mean.Background.")
        
        Normalisedcelltemp <- (meancelltemp - backgroundtemp) / (meancelltemp[nbptsBeforeBleach,] - backgroundtemp[nbptsBeforeBleach,])
        colnames(Normalisedcelltemp)[1] <- paste("Norm_cond", loopcondition, "_file", i, "_cell", j, sep = "")
        if(nrow(Normalisedcelltemp) > nrow(Timestamps)) {
          Normalisedcelltemp <- as.data.frame(Normalisedcelltemp[1:nrow(Timestamps),])
        }
        if(nrow(Normalisedcelltemp) < nrow(Timestamps)) {
          Normalisedcelltemp <- as.data.frame(rbind(Normalisedcelltemp[1,],rep(NA,nrow(Timestamps) - nrow(Timestamps))))
        }
        Normalisedcells <- cbind(Normalisedcells, Normalisedcelltemp)
      }
    }
    assign(paste("NormalisedcellsCond", loopcondition, sep = ""), Normalisedcells)  #Attribute condition in the name. Not clever, but I don't know how to do it.
    write.csv(Normalisedcells, paste("Output/Data/", "Normalised_cond", safeNameCond[loopcondition],".csv", sep = ""), row.names = FALSE)
  }
  
  ## One graph for all individual plots of each normalised cell ----
  
  # Need first to tidy data into long data frame to plot them all at once
  for (loopcondition in 1:nbCond){ #loop through conditions
    Datatemp <- get(paste("NormalisedcellsCond", loopcondition, sep = "")) #give generic name to be easier to work with
    Normalisedcells_tidy <- pivot_longer(data = Datatemp, cols = contains("Norm") ) #reorganise with tidyr: regroup all columns that contains "Norm" in the name
    
    # Plot on graph by condition
    plotConditiontemp <- ggplot(Normalisedcells_tidy, aes(x = time , y = value, color = name)) +
      geom_path() +
      labs(x = "Time(s)", y = "Relative intensity", title = "Allplots", subtitle = nameCond[loopcondition]) +
      lims(y = c(0,1.2)) +
      theme_cowplot(8) +
      theme(legend.position = "none")
    
    
    # save last plot
    ggsave(paste0("FRAP_Ranalysis_allCells",suffix,"_",safeNameCond[loopcondition],".pdf"), plotConditiontemp, path = "Output/Plots", width = 6, height = 4, units = "cm",  bg = NULL)
    
  }
  
  #Clean env
  rm(list = ls(pattern = "DataCond"))
  
  
  # Generate averages curves and average fit _ already tidy to fit ----
  ## Calculate averages, sd, SEM for Normalized data  ----
  #Initialize df needed after
  AverageNormCond <- data.frame()
  semNormCond <- data.frame()
  sdNormCond <- data.frame()
  
  for (loopcondition in 1:nbCond) { #loop over conditions
    Normalisedcellstemp <- get(paste("NormalisedcellsCond", loopcondition, sep = ""))
    
    # Get representative timestamp
    dirTimeStamptemp <- paste0("Data/Timestamps/",nameCond[loopcondition],"/")
    FileListTimestamp <- list.files(path = dirTimeStamptemp) #List all files of this directory timestamps
    Timestamps <- read.csv( paste(dirTimeStamptemp, FileListTimestamp[1], sep = ""))  #read all the first csv file of the directory dirTimeStamp
    Timestamps <- select(Timestamps, contains("Time") ) #keep only the column with time in seconds
    Timestamps <- Timestamps - Timestamps[startfit,]
    colnames(Timestamps)[1] <- "time"
    
    #Do average across rows of data
    Normalisedcellstemp[1] <- NULL # remove first column with time
    AverageNormCondtemp <- rowMeans(Normalisedcellstemp)
    AverageNormCondtemp <- cbind(Timestamps, AverageNormCondtemp,  paste0("Cond", loopcondition))
    colnames(AverageNormCondtemp)[2] <- "value" #rename the columns
    colnames(AverageNormCondtemp)[3] <- "name"
    AverageNormCond <- rbind(AverageNormCond,AverageNormCondtemp)
    
    #Do SEM across rows
    sd <- apply(Normalisedcellstemp,1,sd) #the function "apply" apply the function sd, accross row (indicated by MARGIN = 1)
    semNormCondtemp <- sweep( as.data.frame(sd), 1, sqrt(ncol(Normalisedcellstemp)) , "/") #the function "sweep" is a bit like apply: here it is taking sd and apply "/" sqrt(ncol()) for each row
    semNormCondtemp <- cbind(Timestamps, semNormCondtemp,  paste0("Cond", loopcondition))
    colnames(semNormCondtemp)[2] <- "value" #rename the columns
    colnames(semNormCondtemp)[3] <- "name"
    semNormCond <- rbind(semNormCond,semNormCondtemp)
    
    #Do SD across rows
    sdNormCondtemp <- apply(Normalisedcellstemp,1,sd) #the function "apply" apply the function sd, accross row (indicated by MARGIN = 1)
    sdNormCondtemp <- cbind(Timestamps, sdNormCondtemp,  paste0("Cond", loopcondition))
    colnames(sdNormCondtemp)[2] <- "value" #rename the columns
    colnames(sdNormCondtemp)[3] <- "name"
    sdNormCond <- rbind(sdNormCond,sdNormCondtemp)
  }
  
  
  # # FITS ------------------------------
  # # Do fit on average curves ----
  # ## single exponential fit: eDecay <- function(t, ampl, tau) (ampl*exp(-t/tau)) ----
  
  #initialise df needed later
  allSimplefit <- data.frame()
  TauCond <- data.frame()
  
  for (loopcondition in 1:nbCond) { # loop through conditions
    # Get representative timestamp
    dirTimeStamptemp <- paste0("Data/Timestamps/",nameCond[loopcondition],"/")
    FileListTimestamp <- list.files(path = dirTimeStamptemp) #List all files of this directory timestamps
    Timestamps <- read.csv( paste(dirTimeStamptemp, FileListTimestamp[1], sep = ""))  #read all the first csv file of the directory dirTimeStamp
    Timestamps <- select(Timestamps, contains("Time") ) #keep only the column with time in seconds
    colnames(Timestamps)[1] <- "time"
    
    #define subset of data to work with
    AverageNormCondtemp <- filter(AverageNormCond, name == paste0("Cond", loopcondition)) #filter and take only rows of the condition 1 
    AverageNormCondtemp <- select(AverageNormCondtemp, contains("value")) # take only the column corresponding to the average
    #Could write in one line: AverageNormCondtemp = select(filter(AverageNormCond, name ==  "Cond1"), contains("value"))
    sdNormCondtemp <- select(filter(sdNormCond, name == paste0("Cond", loopcondition)), contains("value")) #filter and take only rows of the condition 1 
    
    # Define values to fit
    y <- AverageNormCondtemp[startfit:nrow(AverageNormCondtemp),] #to start after the bleaching!
    Time <- Timestamps[startfit:nrow(AverageNormCondtemp),] - Timestamps[startfit,]
    w <- 1 / sdNormCondtemp[startfit:nrow(sdNormCondtemp),]
    
    # Define function
    simpleExp <- function(x, ampl, tau, plateau) ((ampl*exp(-x/tau)) +plateau) # with x is the Time here
    
    # Do the fit with nls Nonlinear Least Squares to estimates the parameters
    # The nls has this form: nls( ExpData ~ TheoryFunction, data = DataFrame, parameter initial guesses)
    # ExpData is the column y
    # Theory function where x is the Time, and myA and myT correspond to initial guesses for ampl and tau
    
    ### Initialize guess for simple exponential----
    # Guess plateau = last value of y
    # Guess Ampl = first value - last value (will be neg if increasing curve, positif if decay)
    # Guess Tau = looking for the x value corresponding to the closest y = plateau+(ampl/2)
    plateauguess <- y[length(y)]
    amplguess <- (y[1]-y[length(y)])
    halfP <- plateauguess+(amplguess/2)
    indexhalfP <- which.min(abs(y-halfP)) # index of y corresponding to the closest value of halfP
    Tauguess <- Time[indexhalfP]
    
    simpleModel <- nlsLM(y ~ simpleExp(Time,myA,myT, myP),
                         start = list(myA = amplguess, myT = Tauguess, myP = plateauguess),
                         weights = w) # we use 1/sd for weighting
    print(summary(simpleModel)) # print in the console a summary of the fit
    sumSingle <- summary(simpleModel) # get summary of the fit
    
    # Create a data.frame from the model
    fitmodelS <- data.frame(Time = Time , y = predict(simpleModel), name = paste0("Cond", loopcondition))
    allSimplefit <- rbind(allSimplefit, fitmodelS)
    
    # Plot model
    PlotSIMPLE <- ggplot() +
      geom_point(aes(x = Time, y = y)) +
      geom_line(data = fitmodelS, aes(Time, y), color = "red") +
      labs(x = "Time(s)", y = "Relative intensity", title = paste("plotsinglecond", loopcondition, sep = "" ), subtitle = nameCond[loopcondition],caption = "Normalised" )
    # print(PlotSIMPLE)
    
    # Quality of the fit : calcul of RSS
    RSS.p_s <- sum(residuals(simpleModel)^2)  # Residual sum of squares
    TSS_s <- sum((y - mean(y))^2)  # Total sum of squares
    RSM_s <- 1 - (RSS.p_s/TSS_s) # R-squared measure
    print(paste("Quality of fit_ condition", loopcondition, nameCond[loopcondition], "_cell", i, sep = ""))
    print(paste("Residual sum of square = ", RSS.p_s, SEP = ""))
    print(paste("Total sum of squares = ", TSS_s, SEP = ""))
    print(paste("R-squared measure = ", RSM_s, SEP = ""))
    
    ### Get Tau from single exp----
    ParaSingle <- sumSingle$parameters
    Tautemp <- ParaSingle[2,1]
    Tautemp <- merge(Tautemp, paste0("Cond", loopcondition))
    colnames(Tautemp)[1] <- "Tau"
    colnames(Tautemp)[2] <- "name"
    
    TauCond <- rbind(TauCond, Tautemp)
    colnames(TauCond)[1] <- "Tau"
    colnames(TauCond)[2] <- "name"
    assign(paste("TauCond", loopcondition, sep = ""), as.data.frame(Tautemp))
    
    
  }
  
  # Calcul diffusion coefficient  ----
  # Calcul halftime for each cell of the file : halftime = ln(2)*Tau
  # D = (re^2+rn^2)/(8*Halftimes)
  
  Dcond <- data.frame() #initialise data frame to store all Diffusion coefficient
  for (loopcondition in 1:nbCond) { # loop through conditions
    TauCondtemp <- get(paste("TauCond", loopcondition, sep = "")) #give generical name for simplicity
    TauCondtemp <- TauCondtemp[,1]
    
    
    halftimetemp <- as.numeric(TauCondtemp)*log(2)
    Dtemp <- (re^2+rn^2)/(8*halftimetemp)
    Dtemp <- merge(Dtemp,paste0("Cond", loopcondition))
    colnames(Dtemp)[1] <- "D"
    colnames(Dtemp)[2] <- "name"
    
    Dcond <- rbind(Dcond, Dtemp)
    assign(paste("D_cond", loopcondition, sep = ""), Dtemp)
  }
  colnames(Dcond)[1] <- "D"
  
  ## Plot averages ---- 
  ## modify data frame to plot
  AverageNormCondtoplot <- AverageNormCond
  semNormCondtoplot <- semNormCond
  sdNormCondtoplot <- sdNormCond
  colnames(allSimplefit)[2] <- "value"
  Simplefittoplot <- allSimplefit
  
  # semFitCondtoplot <- pivot_longer(data = semFitCond, cols = contains("Cond"))
  
  # create legend with name corresponding 
  nbcells <- ncol(NormalisedcellsCond1)-1
  legend <- paste(nameCond[1], " (n = ", nbcells,")", sep = "")
  for (loopcondition in 2:nbCond){
    Dtemp <- get(paste("D_cond", loopcondition, sep = ""))
    nbcells <- ncol(get(paste("NormalisedcellsCond", loopcondition, sep = "")))-1
    temp <- paste(safeNameCond[loopcondition], " (n = ", nbcells,")", sep = "")
    legend <- c(legend, temp)
  }
  
  # ggplot
  plotaverages <- ggplot() +
    geom_point(data = AverageNormCondtoplot, aes(x = time , y = value, colour = name), size = 0.8) +
    geom_errorbar(data = AverageNormCondtoplot, aes(x = time , ymin = value- semNormCondtoplot$value, ymax = value+ semNormCondtoplot$value, colour = semNormCondtoplot$name), alpha = 0.5, linewidth = 0.6, width = 0.5) +
    geom_line(data = Simplefittoplot, aes(x = Time, y = value, colour = name)) +
    lims(y = c(0,1.2)) +
    scale_color_discrete(
      name = "Conditions:",
      labels = legend) +
    labs(x = "Time (s)", y = "Relative intensity" , title = "Average FRAP", subtitle = "Single exp fit", caption = "+/-SEM") +
    theme_classic()
  
  ## Plot comparison Tau and diffusion coefficient----
  #Tau
  plotTau <- ggplot(TauCond) + 
    geom_point( aes(x = name , y = Tau, colour = name), size = 3) +
    labs(x = "Condition", y = "Tau (s)" , title = "TAU", subtitle = "Single exp fit") +
    ylim(0,NA) +
    theme_classic() +
    theme(legend.position = "none")
  
  # D
  plotD <- ggplot(Dcond) +
    geom_point(aes(x = name , y = D, colour = name), size = 3) +
    ylim(0,NA) +
    scale_color_discrete(
      name = "Conditions:",
      labels = legend) +
    labs(x = "Condition", y = expression(Diff.~Coef.~(µm^{2}/~s)) , title = "Diffusion coefficient", subtitle = "Single exp fit" ) +
    theme_classic()
  
  
  
  # Clean Data----
  # rm(list = ls(pattern = "temp"))
  
  
  #Print plots----
  
  plotaverages / (plotTau | plotD)
  
  # save last plot
  ggsave (paste0("FRAP_Ranalysis_Averages",suffix,".pdf"), path = "Output/Plots", width = 15, height = 15, units = "cm",  bg = NULL)
  
  
  # save important df 
  write.csv(Dcond, paste0("Output/Data/", "DiffusionCoefficient", suffix,".csv"), row.names = FALSE)
  write.csv(TauCond, paste0("Output/Data/", "Tau", suffix,".csv"), row.names = FALSE)
  write.csv(allSimplefit, paste0("Output/Data/", "Fit_singleExp", suffix,".csv"), row.names = FALSE)
  
  
  # ggplot for paper - legend is placed inside small plot - needs editing
  
  p1 <- ggplot() +
    geom_point(data = AverageNormCondtoplot, aes(x = time , y = value, colour = name), alpha = 0.5, size = 0.4, shape = 16)
  
  if(err == "sd") {
    p1 <-p1 + geom_errorbar(data = AverageNormCondtoplot, aes(x = time , ymin = value - sdNormCondtoplot$value, ymax = value + sdNormCondtoplot$value, colour = sdNormCondtoplot$name), alpha = 0.5, linewidth = 0.4, width = 0)
  } else {
    p1 <- p1 +  geom_errorbar(data = AverageNormCondtoplot, aes(x = time , ymin = value - semNormCondtoplot$value, ymax = value + semNormCondtoplot$value, colour = sdNormCondtoplot$name), alpha = 0.5, linewidth = 0.4, width = 0)
  }
  
  p1 <- p1 + geom_line(data = Simplefittoplot, aes(x = Time, y = value, colour = name, linetype = name)) +
    scale_color_manual(values = myColours) +
    scale_linetype_manual(values = myLines) +
    lims(y = c(0,1.2)) +
    labs(x = "Time (s)", y = "Relative intensity") +
    theme_cowplot(8) +
    theme(legend.position = c(0.8,0.2))
  
  # summarydf <- data.frame(tau = TauCond$Tau,
  #                         cond = nameCond,
  #                         halftime = log(2) * TauCond$Tau)
  
  ggsave(paste0("FRAP_R_Averages", suffix,".pdf"), p1, path = "Output/Plots", width = 5, height = 3.3, units = "cm",  bg = NULL)
}

#.--------------------------------------------------------.
# To generate plots ----

## this is the different concentrations of the zero PEG condition (1st plot)
load_and_process_data(nameCond = c("GFP woPEG", "TPD54WT woPEG", "TPD54mut woPEG"),
                      myColours = c("#88ccee", "#117733", "#999933"),
                      myLines = c(1,1,1),
                      suffix = "_noPEG",
                      err = "sem")

## this is the different concentrations of PEG (2nd plot)
load_and_process_data(nameCond = c("TPD54wt woPEG","TPD54wt PEG6","TPD54wt PEG12","TPD54mut woPEG","TPD54mut PEG6","TPD54mut PEG12"),
                      myColours = c(rep("#117733",3),rep("#999933",3)),
                      myLines = c(1,2,3,1,2,3),
                      suffix = "_PEG",
                      err = "sem")

## this is nocodazole treatment
load_and_process_data(nameCond = c("BeforeNoco","AfterNoco"),
                      myColours = c("#117733","#aa4499"),
                      myLines = c(1,1),
                      suffix = "_noco",
                      err = "sem")

## this is ATP depletion
load_and_process_data(nameCond = c("BeforeATPdepl","AfterATPdepl"),
                      myColours = c("#117733","#aa4499"),
                      myLines = c(1,1),
                      suffix = "_atp",
                      err = "sem")

## 25% PEG and washout
load_and_process_data(nameCond = c("GFP woPEG","GFP PEG25","GFP PEG25washout"),
                      myColours = c("#88ccee","#4477aa","#4477aa"),
                      myLines = c(1,2,3),
                      suffix = "_wash",
                      err = "sem")
