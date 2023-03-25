# From files pooled (manually) together after from R "MeasureRelocation_codeR"
# To pool and plot all conditions normalized together 
# Code written by Meghane Sittewelle
# edited by Stephen Royle
# there is quite a bit of redundancy in this script.

library(tidyverse)
library(cowplot)
library(minpack.lm)

#FUNCTIONS ----
LISTCOND <- function (dirCond, pattern= "*.csv"){
  #List all Conditions names----
  Condname <<- list.dirs(path=dirCond, recursive = FALSE, full.names = FALSE)
  #List all Conditions path----
  CondListPath <<- list.dirs(path=dirCond, recursive = FALSE, full.names = TRUE)
  
  #List all files for each condition----
  listfiles <- list()
  for (i in 1:length(CondListPath)){
    FileList <- list.files(path= CondListPath[i], pattern = pattern)
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


READDATA <- function (listcond, list=FALSE, prefix = ""){
  if(list == TRUE) {
    
    biglistofdf <- list()
    for(i in 1:length(listcond) ) {   #loop through each condition 
      dircond <- CondListPath[i]
      for(j in 1:length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        data_cond_test <- read_csv(paste(dircond, dirfile, sep="/"), show_col_types = FALSE )
        name <- paste0("Cond", i, "_file", j)
        biglistofdf[[name]] <- data_cond_test
      }
    } 
    return(biglistofdf)
  }
  else {
    for(i in 1:length(listcond) ) {   #loop through each condition 
      dircond <- CondListPath[i]
      for(j in 1:length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        assign(paste0(prefix,"Cond", i, "_file", j), read_csv(paste(dircond, dirfile ,sep="/"),show_col_types = FALSE ), envir = .GlobalEnv)
      }
    }
  }
}


READTIME <- function (listcond, list=FALSE, prefix = ""){
  if(list == TRUE) {
    biglistofdf <- list()
    for(i in 1:length(listcond) ) {   #loop through each condition 
      dircond <- CondListPath[i]
      for(j in 1:length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        data_cond_test <- read_table(paste(dircond, dirfile ,sep="/"), col_names = FALSE)
        name <- paste0("TimeCond", i, "_file", j)
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
        assign(paste0(prefix,"TimeCond", i, "_file", j), read_table(paste(dircond, dirfile ,sep="/"), col_names = FALSE), envir = .GlobalEnv)
      }
    }
  }
}


#### ................................... ###### 

# Load data ----

# Make a list of all conditions and files ----
datalist <- LISTCOND(dirCond = "Data", pattern = "*.csv")

# Read files ----
READDATA(datalist, list = FALSE)

#Transform them ready to plot and concatenate in one big file ----

AllCond <- data_frame()
for(i in 1:length(datalist)){
  df <- get(paste0("Cond", i, "_file1"))
  
  #change orientation to plot later
  dflong <- pivot_longer(df, !Time, values_to = "NormValues", names_to = "Cond")
  
  #Add column with only simple name cond 
  dflong$simpleCond <- Condname[i]
  
  #Concatenate in a huge df 
  AllCond <- rbind(AllCond, dflong)
}


dfSummarised <- AllCond %>%
  group_by(Time, simpleCond) %>%
  summarise(mean = mean(NormValues),
            sd = sd(NormValues),
            sem = sd(NormValues) / sqrt(length(NormValues)))


#Plot ----
plot <- ggplot(dfSummarised) + 
  geom_point(aes(x= Time, y = mean, color = simpleCond))+
  geom_errorbar(aes(x=Time, ymin = mean-sem, ymax = mean+sem), alpha=0.5)+
  labs(title = "Rerouting dynamic to mitochondria" )+
  ylab(label = "Mean fluorescence F/F0")+
  xlab(label = "Time (s)")+
  theme_classic()

plot


#Troncate R159E before bleaching
R159Efiltered <- AllCond %>%
  filter(simpleCond == "R159E") %>%
  filter(Time < 100)
allWT <- AllCond %>%
  filter(simpleCond == "WT") 

Allcrop <- rbind(allWT,R159Efiltered)
dfSummarisedcrop <- Allcrop %>%
  group_by(Time, simpleCond) %>%
  summarise(mean = mean(NormValues),
            sd = sd(NormValues),
            sem = sd(NormValues) / sqrt(length(NormValues)))
#Plot

plotcrop <- ggplot(dfSummarisedcrop) + 
  geom_point(aes(x = Time, y = mean, color = simpleCond)) +
  geom_errorbar(aes(x = Time, ymin = mean - sem, ymax = mean + sem), alpha = 0.5) +
  labs(title = "Rerouting dynamic to mitochondria") +
  ylab(label = "Mean fluorescence F/F0") +
  xlab(label = "Time (s)") +
  theme_classic()

plotcrop

# Generate averages curves and average fit _ already tidy to fit ----
## Calculate averages, sd, SEM for Normalized data  ----
#Initialize df needed after
AverageNormCond <- data.frame()
semNormCond <- data.frame()
sdNormCond <- data.frame()
nbCond = length(Condname)

#CROP R159E to get only the first 100 s post rapa 
Cond1 = Cond1_file1 %>%
  filter(Time < 100)
Cond2 = Cond2_file1


for(loopcondition in 1:nbCond) { #loop over conditions
  Normalisedcellstemp <- get(paste0("Cond", loopcondition))
  
  # Get representative timestamp
  Timestamps <- select(Normalisedcellstemp, contains("Time"))
  colnames(Timestamps)[1] <- "time"
  
  #Do average across rows of data
  Normalisedcellstemp[1] <- NULL # remove first column with time
  AverageNormCondtemp <- rowMeans(Normalisedcellstemp)
  AverageNormCondtemp <- cbind(Timestamps, AverageNormCondtemp,  paste0("Cond", loopcondition), Condname[loopcondition] )
  colnames(AverageNormCondtemp)[2] <- "value" #rename the columns
  colnames(AverageNormCondtemp)[3] <- "name"
  colnames(AverageNormCondtemp)[4] <- "Cond"
  AverageNormCond <- rbind(AverageNormCond,AverageNormCondtemp)
  
  #Do SEM across rows
  sd <- apply(Normalisedcellstemp,1,sd) #the function "apply" apply the function sd, accross row (indicated by MARGIN = 1)
  semNormCondtemp <- sweep( as.data.frame(sd), 1, sqrt(ncol(Normalisedcellstemp)) , "/") #the function "sweep" is a bit like apply: here it is taking sd and apply "/" sqrt(ncol()) for each row
  semNormCondtemp <- cbind(Timestamps, semNormCondtemp, paste0("Cond", loopcondition), Condname[loopcondition])
  colnames(semNormCondtemp)[2] <- "value" #rename the columns
  colnames(semNormCondtemp)[3] <- "name"
  colnames(semNormCondtemp)[4] <- "Cond"
  semNormCond <- rbind(semNormCond,semNormCondtemp)
  
  #Do SD across rows
  sdNormCondtemp <- apply(Normalisedcellstemp,1,sd) #the function "apply" apply the function sd, accross row (indicated by MARGIN = 1)
  sdNormCondtemp <- cbind(Timestamps, sdNormCondtemp,  paste0("Cond", loopcondition), Condname[loopcondition])
  colnames(sdNormCondtemp)[2] <- "value" #rename the columns
  colnames(sdNormCondtemp)[3] <- "name"
  colnames(sdNormCondtemp)[4] <- "Cond"
  sdNormCond <- rbind(sdNormCond,sdNormCondtemp)
}


#FITS
# FITS ------------------------------
# Do fit on average curves ----
## single exponential fit: eDecay <- function(t, ampl, tau) (ampl*exp(-t/tau)) ----

#initialise df needed later
allSimplefit <- data.frame()
TauCond <- data.frame()
bleachpoint <- 0 # start after the bleach

for(loopcondition in 1:nbCond) { # loop through conditions
  
  # Get representative timestamp 
  df <- get(paste0("Cond", loopcondition, "_file1"))
  Timestamps <- select(df, contains("Time") )
  colnames(Timestamps)[1] <- "time"
  Timestamps <- na.omit(Timestamps)
  
  #define subset of data to work with
  AverageNormCondtemp <- filter(AverageNormCond, name == paste0("Cond", loopcondition)) #filter and take only rows of the condition 1 
  AverageNormCondtemp <- na.omit(AverageNormCondtemp)
  AverageNormCondtemp <- select(AverageNormCondtemp, contains("value")) # take only the column corresponding to the average
  
  #Could write in one line: AverageNormCondtemp = select(filter(AverageNormCond, name ==  "Cond1"), contains("value"))
  sdNormCondtemp <- select(filter(sdNormCond, name == paste0("Cond", loopcondition)), contains("value")) #filter and take only rows of the condition 1 
  
  # Define values to fit
  startfitpts <- which(Timestamps == bleachpoint)
  y <- AverageNormCondtemp[startfitpts:nrow(AverageNormCondtemp),] #to start after the bleaching!
  Time <- Timestamps$time[startfitpts:nrow(AverageNormCondtemp)]
  w <- 1 / sdNormCondtemp[startfitpts:nrow(sdNormCondtemp),]
  
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
  
  # simpleModel <- nlsLM(y ~ simpleExp(Time,myA,myT, myP),
  #                      start = list(myA = amplguess, myT = Tauguess, myP = plateauguess,
  #                                   weights = w)) # we use 1/sd for weighting
  
  simpleModel <- nlsLM(y ~ simpleExp(Time,myA,myT, myP),
                       start = list(myA = amplguess, myT = Tauguess, myP = plateauguess),
                       weights = w)
  
  print(summary(simpleModel)) # print in the console a summary of the fit
  sumSingle <- summary(simpleModel) # get summary of the fit
  
  # Create a data.frame from the model
  fitmodelS <- data.frame(Time = Time , y = predict(simpleModel), name = paste0("Cond", loopcondition))
  allSimplefit <- rbind(allSimplefit, fitmodelS)
  
  # Quality of the fit : calcul of RSS
  RSS.p_s <- sum(residuals(simpleModel)^2)  # Residual sum of squares
  TSS_s <- sum((y - mean(y))^2)  # Total sum of squares
  RSM_s <- 1 - (RSS.p_s/TSS_s) # R-squared measure
  print(paste("Quality of fit_ condition", loopcondition, Condname[loopcondition], "_cell", i, sep = ""))
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


# Calcul halftime for each cell of the file : halftime = ln(2)*Tau ----

HalfTimeCond <- data.frame() #initialise data frame to store all Diffusion coefficient
for (loopcondition in 1:nbCond) { # loop through conditions
  TauCondtemp <- get(paste("TauCond", loopcondition, sep = "")) #give generical name for simplicity
  TauCondtemp <- TauCondtemp[,1]
  
  halftimetemp <- as.numeric(TauCondtemp)*log(2)
  halftimetemp <- merge(halftimetemp,paste0("Cond", loopcondition))
  colnames(halftimetemp)[1] <- "halftime"
  colnames(halftimetemp)[2] <- "name"
  
  HalfTimeCond <- rbind(HalfTimeCond, halftimetemp)
  assign(paste0("HalfTime_cond", loopcondition), halftimetemp)
}
colnames(HalfTimeCond)[1] <- "Halftime"

## Plot averages ---- 
## modify data frame to plot
AverageNormCondtoplot <- AverageNormCond
semNormCondtoplot <- semNormCond
sdNormCondtoplot <- sdNormCond
# single df for ease of plotting
dataNormCondtoplot <- AverageNormCond
dataNormCondtoplot$sd <- semNormCond$value
dataNormCondtoplot$sem <- semNormCond$value
colnames(allSimplefit)[2] <- "value"
Simplefittoplot <- allSimplefit


# create legend with name corresponding
safeNameCond <- sub(" ","",Condname)
nbcells <- ncol(Cond1)-1
legend <- paste0(Condname[1], " (n = ", nbcells,")")
for (loopcondition in 2:nbCond){
  nbcells <- ncol(get(paste0("Cond", loopcondition)))-1
  temp <- paste(safeNameCond[loopcondition], " (n = ", nbcells,")", sep = "")
  legend <- c(legend, temp)
}


# ggplot
plotaverages <- ggplot() +
  geom_point(data = AverageNormCondtoplot, aes(x = time , y = value, colour = name), size = 0.8) +
  geom_errorbar(data = AverageNormCondtoplot, aes(x = time , ymin = value- semNormCondtoplot$value, ymax = value+ semNormCondtoplot$value, colour = semNormCondtoplot$name), alpha = 0.5, linewidth = 0.6, width = 0.5) +
  geom_line(data = Simplefittoplot, aes(x = Time, y = value, colour = name)) +
  scale_color_discrete(
    name = "Conditions:",
    labels = legend) +
  labs(x = "Time (s)", y = "Relative intensity" , title = "MitoTrap", subtitle = "Single exp fit", caption = "+/-SEM") +
  theme_classic()

plotaverages


# Plot Steve's style for paper ----

# hack function for 1dp axis tick labels
scaleFUN <- function(x) sprintf("%.1f", x)

plotsForPaper <- function(nameCond, myColours, myLines, suffix, err) {
  
  # save important df 
  write.csv(HalfTimeCond, paste0("Output/Data/", "HalfTime", suffix,".csv"), row.names = FALSE)
  write.csv(TauCond, paste0("Output/Data/", "Tau", suffix,".csv"), row.names = FALSE)
  write.csv(allSimplefit, paste0("Output/Data/", "Fit_singleExp", suffix,".csv"), row.names = FALSE)
  
  # ggplot for paper - legend is placed inside small plot - needs editing
  
  p1 <- ggplot() +
    geom_line(data = AverageNormCondtoplot, aes(x = time , y = value, colour = name), alpha = 0.5, linewidth = 0.5)
  
  if(err == "sd") {
    p1 <-p1 + geom_ribbon(data = AverageNormCondtoplot, aes(x = time , ymin = value - sdNormCondtoplot$value, ymax = value + sdNormCondtoplot$value, fill = sdNormCondtoplot$name), alpha = 0.5)
  } else {
    p1 <- p1 +geom_ribbon(data = AverageNormCondtoplot, aes(x = time , ymin = value - semNormCondtoplot$value, ymax = value + semNormCondtoplot$value, fill = sdNormCondtoplot$name), alpha = 0.5)
  }
  
  p1 <- p1 + geom_line(data = Simplefittoplot, aes(x = Time, y = value, colour = name, linetype = name)) +
    scale_color_manual(values = myColours) +
    scale_fill_manual(values = myColours) +
    scale_linetype_manual(values = myLines) +
    # lims(y = c(0,1.2)) +
    labs(x = "Time (s)", y = "Relative intensity") +
    theme_cowplot(8) +
    theme(legend.position = c(0.8,0.2))
  
  #print(p1)
  ggsave(paste0("Mitotrap_R_Averages", suffix,".pdf"), p1, path = "Output/Plots", width = 5, height = 3.3, units = "cm",  bg = NULL)
  
  # individuals
  
  for(i in 1:length(nameCond)) {
    plotdata <- filter(dataNormCondtoplot, Cond == nameCond[i])
    p2 <- ggplot() +
      geom_line(data = plotdata, aes(x = time , y = value), colour = myColours[i], alpha = 0.5, linewidth = 0.5)
    
    if(err == "sd") {
      p2 <- p2 + geom_ribbon(data = plotdata, aes(x = time, ymin = value - sd, ymax = value + sd), fill = myColours[i], alpha = 0.5)
    } else {
      p2 <- p2 + geom_ribbon(data = plotdata, aes(x = time, ymin = value - sem, ymax = value + sem), fill = myColours[i], alpha = 0.5)
    }
    
    p2 <- p2 + geom_line(data = filter(Simplefittoplot, name == paste0("Cond",i)), aes(x = Time, y = value), colour = myColours[i], linetype = myLines[i]) +
      scale_color_manual(values = myColours) +
      scale_fill_manual(values = myColours) +
      scale_linetype_manual(values = myLines) +
      scale_y_continuous(labels = scaleFUN) +
      labs(x = "Time (s)", y = "Relative intensity") +
      theme_cowplot(8) +
      theme(legend.position = c(0.8,0.2))
    
    ggsave(paste0("Individual_Averages", suffix,"_", nameCond[i],".pdf"), p2, path = "Output/Plots", width = 5, height = 3.3, units = "cm",  bg = NULL)
  }
}

#.--------------------------------------------------------.
# To generate plots ----
plotsForPaper(nameCond = c("R159E","WT"),
             myColours = c("#999933","#117733"),
             myLines = c(5,5),
             suffix = "_Mitotrap",
             err = "sem")



