
# --- AUXILIARY CODE used in "Clasificación de tendencias de series de tiempo de 
# --- NDVI de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica, submitted by June 7, 2022

# --- Required R packages

library(trend)
library(Kendall)
library(geoTS)
library(forecast)
library(doParallel)
library(imputeTS)
library(bfast)

library(gtools)
library(geoTS)

library(raster)
library(RStoolbox)

library(knitr)
library(sp)
library(gapfill)
library(RColorBrewer)


# --- Auxiliay functions ---

#' Function that allows to open RData to current environment
#' RData is a character with RData filename
#' 
LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  env
}

#' Function that allows to retrieve a time series from database (df) by
#' specifying pixel coordinates (toPlot)
#' 
#' toPlot is a list with pixel coordinates ($x, $y) 
#'     df is a numeric matrix, first 2 columns are pixel coordinates remainder columns
#'        must contain time series values
#'           
get_timeSeries_byClicking <- function(toPlot, df){
  nRow <- length(unlist(toPlot)) / 2
  
  mat_toPlot <- matrix(as.numeric(unlist(toPlot)), nrow = nRow)
  
  dX <- matrix(nrow = nrow(df))
  
  dY <- matrix(nrow = nrow(df))
  
  aproxX <- numeric(nRow)
  
  aproxY <- numeric(nRow)
  
  dX <- sapply(1:nRow, function(s) abs(df[,1] - mat_toPlot[s,1]))
  
  aproxX <- sapply(1:nRow, function(s) df[which.min(dX[,s]),1] )
  
  dY <- sapply(1:nRow, function(s) abs(df[,2] - mat_toPlot[s,2]))
  
  aproxY <- sapply(1:nRow, function(s) df[which.min(dY[,s]),2] )
  
  toExtract <- matrix(nrow = nRow, ncol = 2)
  
  toExtract[,1] <- aproxX
  toExtract[,2] <- aproxY
  #
  IND <- 1:length(df)
  xTemp <- which(df[,1] == toExtract[1,1])
  yTemp <- which(df[xTemp,2] == toExtract[1,2])
  #
  xyRow <- xTemp[yTemp] # df[xTemp[yTemp],1:2]
  
  list(coord = xyRow)
  # xyRow
}
# ---

# =============================================================================
# --- Functions used for pre-processing Landsat-7 and Landsat 8 OLI
# =============================================================================
# --- Set of functions employed for radiometric correction, cloud masking, cropping
# --- and calculation of NDVI based on scene of Landsat 8 images.

#' Computes spectral index
#' 
#' STACK is a RasterStack
#'     i is an integer greater than or equal to 1 but lesser than or equal to 12
#'     j is an integer greater than or equal to 1 but lesser than or equal to 12
#' 
getINDEX <- function(STACK,j,k){
  (STACK[[j]]-STACK[[k]])/(STACK[[k]]+STACK[[j]])
}

#' Computes NDVI and bands 6 and 7 from Landsat8 scene
#' 
#'        PATH is a character with the directory path containing Landsat 8 files;
#'             this directory must contain MTL and ANG files
#' whereToSave is a character with a directory path where the processed .tif will
#'             be stored
#'    cloudThr is a integer indicating valid threshold used in cloud masking; 
#'             2800 for this study
#'      EXTENT is an extent object to be used for cropping the original scene
#'       LABEL is character or integer vector; object used to add extra identification 
#'             to output products.
#'
get_radCor_clouds_NDVI_crop <- function(PATH,whereToSave,cloudThr,EXTENT,LABEL){
  
  listFILEStxt <- list.files(path=PATH, pattern=".txt", full.names=TRUE)
  
  MTL <- readMeta(listFILEStxt[2])
  
  LANDSAT <- stackMeta(MTL, category = "all")
  
  LANDSAT_radCal <- radCor(img=LANDSAT, metaData=MTL, 
                           bandSet=c("B4_dn","B5_dn","B6_dn","B7_dn"),
                           atmosphere="veryClear",
                           verbose=TRUE, method="dos")
  
  qaBand <- subset(LANDSAT,11)
  
  clouds_shadows <- qaBand
  clouds_shadows[qaBand >= cloudThr] <- 0
  clouds_shadows[ clouds_shadows != 0 ] <- 1
  
  NDVI_clean <- getINDEX(STACK=LANDSAT_radCal,2,1) #NDVI_mio
  NDVI_clean[ clouds_shadows == 0 ] <- NA
  
  # EXTENT <- extent(c(118035, 198135, 2001465, 2110995))
  NDVI_crop <- crop(NDVI_clean, EXTENT)
  
  aux <- unlist(strsplit(MTL$DATA$FILES[1], "_"))[-c(5:8)]
  
  baseNAME <- aux[1]
  for(k in 2:4){
    baseNAME <- paste0(baseNAME, "_", aux[k])
  }
  
  cat("---Saving NDVI product in ", whereToSave, "\n")
  writeRaster(x=NDVI_crop,
              filename = paste0(whereToSave,"/NDVI_", baseNAME, "_", LABEL),
              format = "GTiff", datatype="FLT4S",overwrite=TRUE)
  cat("---Saving B6_sre product in ", whereToSave, "\n")
  writeRaster(x=subset(LANDSAT,3),
              filename = paste0(whereToSave,"/B6_sre_", baseNAME, "_", LABEL),
              format = "GTiff", datatype="FLT4S",overwrite=TRUE)
  cat("---Saving B7_sre product in ", whereToSave, "\n")
  writeRaster(x=subset(LANDSAT,4),
              filename = paste0(whereToSave,"/B7_sre_", baseNAME, "_", LABEL),
              format = "GTiff", datatype="FLT4S",overwrite=TRUE)
}
# ---

# =============================================================================
# --- Functions used for applying gapfill() to data cube
# =============================================================================

#' Applies list.files and mixedsort to get efficient access to files
#' 
getListFiles <- function(pattern, ...){
  # myFiles <- list.files(pattern = pattern, ...)
  # mixedsort(myFiles)
  mixedsort(list.files(pattern = pattern, ...))
}

#' Gets a 3D array needed by get4D_array 
#' path is a character with the directory path to .tif files to be processed
#' 
get3D_array <- function(path){
  STACK <- stack(path)
  ARRAY <- array(NA, dim = c(nrow(STACK), ncol(STACK), 2))
  for( i in 1:2 ){
    TEMP <- getValues(subset(STACK, i))
    ARRAY[,,i] <- TEMP
  }
  ARRAY
}

#' Gets 4D array needed by Gapfill
#' 
#' listPath a character vector with names of .tif files on which gapfill will be applied
#' i integer indicating number of listPath's file to be processed
#' longitude numeric vector of length ncol of RasterStack listed in listPath
#' latitude numeric vector of length nrow of RasterStack listed in listPath
#' days numeric vector of length 2
#' years numeric vector of length 2
#' 
get4D_array <- function(listPath, i, longitude, latitude, days, years){
  layer1 <- get3D_array(path = listPath[[1]][i])
  layer2 <- get3D_array(path = listPath[[2]][i])
  
  ARRAY_LAT_LONG_DAYS_YEARS <- array(NA,
                                     dim = c(length(longitude), length(latitude), 2, 2),
                                     dimnames = list(longitude, latitude, days, years))
  
  ARRAY_LAT_LONG_DAYS_YEARS[,,1,1] <- layer1[,,1]
  ARRAY_LAT_LONG_DAYS_YEARS[,,2,1] <- layer1[,,2]
  
  ARRAY_LAT_LONG_DAYS_YEARS[,,1,2] <- layer2[,,1]
  ARRAY_LAT_LONG_DAYS_YEARS[,,2,2] <- layer2[,,2]
  ARRAY_LAT_LONG_DAYS_YEARS
}

#' Applies function raster::mosaic
#' mosaic1 list containing RasterLayer to be glued together
#' 
mosaicking <- function(mosaic1){
  mosaic1$fun <- max
  do.call(mosaic, mosaic1)
}

#' Saves RasterLayers to .tif files. These RasterLayers are the results of 
#' applying Gapfill in the context of this project.
#' 
#' originalRaster is a RasterLayer used to get raster::names
#' mosaic is the RasterLayer to be saved
#' whereToSave character with directory path where mosaic will be save
#' 
save_mosaic <- function(originalRaster, mosaic, whereToSave){
  writeRaster(mosaic,
              paste0( whereToSave, "/", names(originalRaster), "_gapfill.tif"),
              format = "GTiff", overwrite = T, datatype = "FLT4S")
}
# ---

# =============================================================================
# --- Functions used for applying interpolation hybrid method
# =============================================================================

# --- Function that transforms a pixel into a matrix

#'         x is a numeric vector containing values of a time series
#' lenPeriod is a numeric with the number of observations per period/season/year
#' 
#' NOTE:
#' This function returns a matrix with nrow equal to number of periods/seasons/years
#' and ncol equal to number of observations per period.
#' 
get_pixel_matrix <- function(x,lenPeriod){
  output <- matrix(nrow=length(x)/lenPeriod, ncol=lenPeriod)
  
  for(i in seq_len(nrow(output))){
    output[i,] <- x[((i-1) * lenPeriod + 1):(i * lenPeriod)]
  }
  output
}

#' This function gets the climatology curve of a pixel/time-series
#' 
#' x is a numeric vector containing values of a time series
#' nPeriods is a numeric given the number of periods/season/years spanned by length(x)
#' lenPeriod is a numeric with the number of observations per period
#' 
#' NOTE: for this to work, length(x) must be a multiple of lenPeriod 
#' 
climatology <- function(x, nPeriods, lenPeriod){
  MAT <- get_pixel_matrix(x=x, lenPeriod=lenPeriod)
  
  BOXPLOT <- boxplot(MAT, plot=FALSE)
  
  list(matrix=MAT, boxplot=BOXPLOT)
}

#' This functions uses climatology to fill out the first missing value appearing 
#' in the group DoY.
#' 
#' climatology is a list, the output of climatology function
#'         box is a character specifying what quantile of climatology curve to use 
#'             for prediction of missing value
#' 
gapfill_climatology <- function(climatology, box=c("lower", "median", "upper")){
  MAT <- climatology$matrix
  
  box <- match.arg(box)
  
  quant <- ifelse(box=="lower", 2, ifelse(box=="median", 3, 4))
  
  for(i in 1:ncol(MAT)){
    indNA <- (1:nrow(MAT))[is.na(MAT[,i])]
    MAT[indNA[1],i] <- climatology$boxplot$stats[quant,i]
  }
  MAT
}

# =============================================================================
# --- Functions used for break-points estimation ---
# =============================================================================

#' Applies bfast01 to data to get a breakpoint estimate
#' 
#' start     numeric specifying starting year of data
#' end       numeric specifying ending year of data
#' frequency numeric givin number of observations per year
#' data      numeric vector, the data to abalyze
#' 
#' Details. This functions assumes that data has been regularly sampled. No missing
#' value is allowed - a priori. It is assumed that the first observation has been 
#' acquired on the first day of the start year.
#' 
#' Value
#' 
#' A list with objects
#' 
#' bPs          numeric vector with the estimated breakpoints
#' type         numeric vector specifying which type of breakpoint has been detected
#' significance numeric
#' stability    numeric
#' 
#' For further details see bfast01
#' 
getBreak <- function(data, start=2000, end=2018, frequency=23, bw=0.15){
  output <- NA
  breakType <- NA
  significance <- NA
  stability <- NA
  
  dataTS <- ts(data, start=c(start, 1), end=c(end, frequency), 
               frequency=frequency)
  
  getBFAST <- bfast01(data=dataTS, bandwidth=bw)
  
  # if(getBFAST$breaks == 1){
  output <- getBFAST$breakpoints
  temp <- bfast01classify(getBFAST)
  breakType <- as.numeric(temp[1])
  significance <- as.numeric(temp[2])
  stability <- as.numeric(temp[3])
  # }
  
  list(bPs=output, type=breakType, significance=significance, 
       stability=stability)
}

#' Calculates the year on which a breakpoint has occurred as a function of
#' start and end years of a time series, as well as the estimated breakpoint
#' and the number of observations per year.
#' 
#' start numeric giving starting year of analyzed period
#' end   numeric giving ending year of analyzed period
#' bp    numeric, a breakpoint as estimated by bfast01
#' freq  numeric giving the number of observations pear year
#' 
#' Value
#' 
#' A numeric 
#' 
getYear <- function(start=2000, end=2018, bp, freq=23){
  period <- start:end
  totalDays <- c(0, freq * 1:length(start:end))
  year <- period[sum( totalDays - bp < 0 )]
  year  
}
# ---