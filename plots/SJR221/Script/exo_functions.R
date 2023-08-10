# FUNCTIONS ----

#' DATANORM - Reshape and normalise dataframe coming from imageJ macro "ExocytosisAnalysis"
#' 
#' @param data Data frame coming from imageJ macro. Should give intensity of spots overtime. Each columns is a spot.
#' @return Reshaped and normalised dataframe.

DATANORM <- function(data) {
  # Reshape data
  data<- data %>%
    relocate(frame) #put the column fram as first column
  
  dftoplot <- pivot_longer(data = data, cols = contains("spot"))
  
  # Normalisation
  Normdata <- dftoplot %>%
    group_by(name) %>%
    mutate(norm = value - median(value))
  
  return (Normdata)
} 

#' FINDPEAK - Reshape and find peaks in dataframe coming from the function DATANORM.
#'
#' @param Normdata Data frame coming from DATANORM function. The df from DATANORM has a column: frame, name(spotsname), value(intensity), norm(intensity normalised).
#' @param minpeakheight A numeric value to determine the minimum value of the peak height. See pracma::findpeaks.
#' @param threshold A numeric value to determine a threshold for peak detection.  See pracma::findpeaks.
#' @param threshold2 A numeric value to determine a threshold for peak detection. The second time it is taking a pourcentage of the peak value.

#' @return A dataframe with peaks for each spot. It has columns: spotname = V1, val = V2, index = V3, start = V4, end = V5.

FINDPEAK <- function (Normdata, minpeakheight, threshold = 1, threshold2 = 0.65) {
  # Reshape the data frame----
  NormdataWide <- Normdata %>%
    select(frame, name, norm) %>%
    pivot_wider(names_from = name, values_from = norm)
  
  # findpeak works only on numeric class, not dataframe. So need to change the dataframe into numeric using lapply
  NumNormData <<- lapply(select(NormdataWide, !contains("frame")), as.numeric)
  
  # Find peaks ----
  allpeaksNorm <- data_frame()
  
  for(i in 1 : length(NumNormData)) {
    peakstemp1 <- findpeaks(NumNormData[[i]], nups = 1, ndowns = 1, minpeakdistance = 10, minpeakheight = minpeakheight,  threshold =  threshold)
    #Do double check of findpeak with threshold half of the value found!
    peakstemp <- findpeaks(NumNormData[[i]], nups = 1, ndowns = 1, minpeakdistance = 10, minpeakheight = minpeakheight,  threshold =peakstemp1[1]*threshold2 )
    if (!is.null(peakstemp)) {
      peakstemp <- cbind(names(NumNormData[i]), peakstemp)
    }
    allpeaksNorm <- rbind (allpeaksNorm, peakstemp)
  }
  #rename columns
  if (length(allpeaksNorm) > 0) {
    allpeaksNorm <- allpeaksNorm %>%
      rename(spotname = V1, val = V2, index = V3, start = V4, end = V5)
  }
  
  return(allpeaksNorm)
}

#' TESTFEW - Take random spots and peaks to display. 
#' 
#' @param Normdata Data frame returned by DATANORM function. The df from DATANORM has a column: frame, name(spotsname), value(intensity), norm(intensity normalised).
#' @param allpeaksNorm Data frame returned by FINDPEAK function. It has columns: spotname, val, index, start, end .
#' @param setseed Boolean value: TRUE or FALSE. Allows to have always the same set of spots/peaks or not. For reproducibility.
#' @param seed Numerical value for the seed if setseed is TRUE. 
#' @param sample Numerical value for the size of the sample. 
#' @param positiveOnly Boolean value: TRUE or FALSE. Determine if sample within spots with detected peaks or all spots. 
#' 
#' @return a plot. 

TESTFEW <- function (Normdata, allpeaksNorm, setseed = TRUE, seed = 1, sample = 5, positiveOnly = TRUE) {
  #Set seed or not ----
  if(setseed == TRUE) {
    set.seed(seed)
  }
  #Choose to display only data with peaks or not ----
  if(positiveOnly == TRUE){
    spotnameSelection <- unique(allpeaksNorm$spotname) %>%
      sample(sample)
  }
  else {
    spotnameSelection <- unique(Normdata$name) %>%
      sample(sample)
  }
  
  # Filter few ----
  FewPeaksNorm <<- allpeaksNorm %>%
    filter (spotname %in% spotnameSelection)
  FewNorm <<- Normdata  %>%
    filter (name %in% spotnameSelection)
  
  #Plot ----
  pSelectionNorm <- ggplot() +
    labs(title = "Baseline subtracted") +
    geom_line(FewNorm, mapping = aes(x = frame, y = norm, colour = name)) +
    geom_point(FewPeaksNorm, mapping = aes(x= as.numeric(index), y= as.numeric(val), colour=spotname))
  
  return (pSelectionNorm)
}

NBEXO <- function (info, allpeaksNorm) {
  #C alculate the number of events per um2
  area <- as.numeric(info[1,2])
  time <- as.numeric(info[2,2])
  Events <- length(allpeaksNorm$spotname) / area
  
  return(Events)
}

#' Function_NormPeakNbEvents - Big function that is using functions above: DATANORM, FINDPEAK and NBEXO
#'
#' @param data Data frame coming from imageJ macro "ExocytosisAnalysis". 
#' Dataframe of intensity of spot overtime. Each columns is a spot.
#' @param info Data frame coming from imageJ macro "ExocytosisAnalysis". 
#' Dataframe with information of the file as area, timestep, unit, and scale.
#' @return Nb of exocytic events per µm2/s  (or pixel2/s).

Function_NormPeakNbEvents <- function (data, info, minpeakheight = minpeakheight, threshold = threshold ){ 
  ## Reshape and normalise the data
  Normdata <<- DATANORM(data = data)
  
  ## Findpeaks
  Allpeaks <<- FINDPEAK (Normdata = Normdata, minpeakheight, threshold )
  
  ## Calculate nb of exocytic events per µm2/s  (or pixel2/s)
  NbExoEvents =  NBEXO(info=info, allpeaksNorm = Allpeaks)
  
  return(NbExoEvents)
}

LISTCOND <- function (dirCond, pattern= "*.csv"){
  #List all Conditions names
  Condname <<- list.dirs(path=dirCond, recursive = FALSE, full.names = FALSE)
  #List all Conditions path
  CondListPath <<- list.dirs(path=dirCond, recursive = FALSE, full.names = TRUE)
  
  #List all files for each condition
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


READDATA <- function (listcond, list=FALSE, prefix = ""){
  if( list == TRUE) {
    
    biglistofdf<- list()
    for (i in 1 : length(listcond) ) {   #loop through each condition 
      dircond <- CondListPath[i]
      for (j in 1 : length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        data_cond_test <- read_csv(paste(dircond, dirfile ,sep="/"),show_col_types = FALSE )
        name <- paste0("Cond", i, "_file", j)
        biglistofdf[[name]] <- data_cond_test
      }
    }
    
    return(biglistofdf)
  } else {
    for ( i in 1 : length(listcond) ) {   #loop through each condition 
      dircond <- CondListPath[i]
      for (j in 1 : length(listcond[[i]])) { #loop through each file
        dirfile <- listcond[[i]][j]
        assign(paste0(prefix,"Cond", i, "_file", j), read_csv(paste(dircond, dirfile ,sep="/"), show_col_types = FALSE ), envir = .GlobalEnv)
      }
    }
  }
}


#' average_waves - a function that is analolgous to IgorPro's average waves.
#' 
#' Resamples each "wave" at constant time steps using linear interpolation, to allow averaging.
#'
#' @param df Data frame from exo_script it needs Condname, UniqueIDspot, RelativeTime, NewNorm columns
#' @param str string to subset the data by Condname
#' @return data frame of summary data per resampled timepoint

average_waves <- function(df,str) {
  # in order to average the traces we need to array each into a column according to time
  intensities <- df %>% 
    filter(Condname == str) %>% 
    select(c(UniqueIDspot,RelativeTime,NewNorm)) %>% 
    pivot_wider(
      names_from = UniqueIDspot,
      values_from = NewNorm,
      names_glue = "int_{UniqueIDspot}"
    )
  # reorder by t as the order may be messed up during pivot_wider
  intensities <- intensities[order(intensities$RelativeTime), ]
  # make a desired length dataframe
  newdf <- data.frame(t = seq(from = -2, to = 4, length.out = 61))
  for(i in 2:ncol(intensities)) {
    subdf <- cbind(intensities[,1],intensities[,i])
    subdf <- subdf[complete.cases(subdf),]
    temp <- approx(x = subdf[,1], y = subdf[,2], xout = seq(from = -2, to = 4, length.out = 61))
    newdf <- cbind(newdf,temp[[2]])
  }
  names(newdf) <- names(intensities)
  # # average all columns except first one (t). We use an.approx from zoo to fill in gaps
  # resampled <- na.approx(intensities[, 2:ncol(intensities)])
  avg <- rowMeans(newdf[,-1], na.rm = TRUE)
  stdev <- apply(newdf[,-1], 1, sd, na.rm = TRUE)
  # count valid tracks per time point
  nv <- rowSums(!is.na(newdf[, -1]))
  # assemble simple df for plotting
  avgDF <- data.frame(t = newdf$RelativeTime,
                      avg = avg,
                      sd = stdev,
                      n = nv,
                      sem = stdev / sqrt(nv),
                      Condname = str)
  return(avgDF)
}