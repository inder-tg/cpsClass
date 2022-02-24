
library(gtools)

#' Applies list.files and mixedsort
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


LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  env
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
              # paste0( getwd(), "/gapFill_eighteenthBatch/dataBloque69/filled/", names(originalRaster), "_gapfill.tif"),
              paste0( whereToSave, "/", names(originalRaster), "_gapfill.tif"),
              format = "GTiff", overwrite = T, datatype = "FLT4S")
  
}
