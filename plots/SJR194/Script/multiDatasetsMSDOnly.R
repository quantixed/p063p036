library(TrackMateR)
library(ggplot2)
library(patchwork)

multiDatasetsMSDOnly <- function() {
  
  condition <- value <- dataid <- cumulative_distance <- track_duration <- NULL
  
  if(!dir.exists("Data")) {
    # there is no cross-platform way to safely choose directory
    cat("Please organise your XML files in a folder called Data in the working directory\r")
    return(-1)
  } else {
    datadir <- "Data"
  }
  
  # loop through condition folders within data folder
  condFolderNames <- list.dirs(path = datadir, recursive = FALSE)
  # break if there were no folders in Data directory
  if(identical(condFolderNames, character(0)) == TRUE) {
    return(-1)
  }
  
  for(i in 1:length(condFolderNames)) {
    condFolderPath <- condFolderNames[i]
    condFolderName <- basename(condFolderPath)
    allTrackMateFiles <- list.files(condFolderPath, pattern = "*.xml")
    # skip if there were no XML files in this folder
    if(identical(allTrackMateFiles, character(0)) == TRUE) {
      next
    }
    # check to see if a calibration file is present
    calibrationFiles <- list.files(condFolderPath, pattern = "*.csv")
    calibrationFile <- paste0(condFolderPath,"/",calibrationFiles[1])
    if(identical(calibrationFiles, character(0)) == TRUE) {
      calibrationXY <- 1
      calibrationT <- 1
      calibrate <- FALSE
    } else {
      # load calibration file and store calibrations
      calibDF <- read.csv(calibrationFile)
      calibrate <- TRUE
    }
    cat(paste0("\n","Processing ",condFolderName,"\n"))
    for(j in 1:length(allTrackMateFiles)) {
      fileName <- allTrackMateFiles[j]
      thisFilePath <- paste0(condFolderPath, "/", fileName)
      # read dataset
      tmObj <- readTrackMateXML(XMLpath = thisFilePath)
      # scale dataset if required
      if(calibrate) {
        calibrationDF <- tmObj[[2]]
        # scalar for conversion is new / old (units not relevant)
        calibrationXY <- calibDF[1,1] / calibrationDF[1,1]
        calibrationT <- calibDF[2,1] / calibrationDF[2,1]
        # 0 in calibDF indicates no scaling is to be done
        calibrationXY <- ifelse(calibrationXY == 0, 1, calibrationXY)
        calibrationT <- ifelse(calibrationT == 0, 1, calibrationT)
        # ignore an error of 2.5%
        calibrationXY <- ifelse(calibrationXY < 1.025 & calibrationXY > 0.975, 1, calibrationXY)
        calibrationT <- ifelse(calibrationT < 1.025 & calibrationT > 0.975, 1, calibrationT)
        if(calibrationXY != 1 & calibrationT != 1) {
          tmObj <- correctTrackMateData(dataList = tmObj, xyscalar = calibrationXY, tscalar = calibrationT, xyunit = calibDF[1,2], tunit = calibDF[2,2])
        } else if(calibrationXY != 1 & calibrationT == 1) {
          tmObj <- correctTrackMateData(dataList = tmObj, xyscalar = calibrationXY, xyunit = calibDF[1,2])
        } else if(calibrationXY == 1 & calibrationT != 1) {
          tmObj <- correctTrackMateData(dataList = tmObj, tscalar = calibrationT, tunit = calibDF[2,2])
        } else {
          # the final possibility is nothing needs scaling but units need to change.
          # do not test if they are the same just flush the units with the values in the csv file
          tmObj <- correctTrackMateData(dataList = tmObj, xyunit = calibDF[1,2], tunit = calibDF[2,2])
        }
      }
      tmDF <- tmObj[[1]]
      calibrationDF <- tmObj[[2]]
      p_allTracks <- plot_tm_allTracks(input = tmObj, summary = FALSE, alphaLevel = 0.5)
      # take the units
      units <- calibrationDF$unit[1:2]
      # we need to combine data frames
      # first add a column to id the data
      thisdataid <- paste0(condFolderName,"_",as.character(j))
      tmDF$dataid <- thisdataid
      # calculate MSD
      msdObj <- calculateMSD(tmDF, N = 2, short = 0)
      msdDF <- msdObj[[1]]
      alphaDF <- msdObj[[2]]
      # same here, combine msd summary and alpha summary
      msdDF$dataid <- thisdataid
      alphaDF$dataid <- thisdataid
      # plot MSD
      msdreturn <- plot_tm_MSD(msdDF, units, auto = TRUE)
      p_msd <- msdreturn[[1]]
      dee <- msdreturn[[2]]
      # plot alpha
      alphas <- msdObj[[2]]
      alphas <- na.omit(alphas)
      # within a sensible range (log 2)
      identify <- alphas$alpha <= 4 & alphas$alpha >= -4
      # we will take log2
      # alphas <- data.frame(alpha = alphas[identify,])
      alphas <- subset(alphas, identify)
      # convert back to real numbers
      median_alpha <- 2^(median(alphas$alpha, na.rm = TRUE))
      p_alpha <- plot_tm_alpha(df = alphas, median_alpha = median_alpha)
      # create the report for this dataset
      fileName <- tools::file_path_sans_ext(basename(thisFilePath))
      
      df_report <- data.frame(dataid = thisdataid,
                              filename = fileName,
                              dee = dee,
                              alpha = median_alpha)
      
      r_report <- p_allTracks / (p_msd + p_alpha)
      r_report <- r_report + plot_annotation(title = condFolderName, subtitle = fileName)
      
      destinationDir <- paste0("Output/Plots/", condFolderName)
      setupOutputPath(destinationDir)
      filePath <- paste0(destinationDir, "/report_",as.character(j),".pdf")
      ggsave(filePath, plot = r_report, width = 25, height = 19, units = "cm")
      if(i == 1 & j == 1) {
        megareport <- df_report
      } else if(!exists("megareport")) {
        megareport <- df_report
      } else {
        megareport <- rbind(megareport,df_report)
      }
    }
  }
  
  # save summary data as csv
  destinationDir <- "Output/Data"
  write.csv(megareport, paste0(destinationDir, "/allComparison.csv"), row.names = FALSE)
}

setupOutputPath <- function(fpath) {
  # example "Output/Data/foo/bar"
  path_components <- unlist(strsplit(fpath, split = "/"))
  
  for(i in 1:length(path_components)){
    test_path <- paste(path_components[1:i], collapse = "/")
    ifelse(!dir.exists(test_path), dir.create(test_path),FALSE)
  }
}
