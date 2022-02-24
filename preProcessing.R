
#' Set of functions employed for radiometric correction, cloud masking, cropping
#' and calculation of NDVI based on scene of Landsat 8 images.

#' Computes spectral index
#' 
#' STACK must be a RasterStack
#' j and i must be integer between 1 and 12
#' 
getINDEX <- function(STACK,j,k){
  (STACK[[j]]-STACK[[k]])/(STACK[[k]]+STACK[[j]])
}

#' Computes NDVI and bands 6 and 7 from Landsat8 scene
#' PATH is a character with the directory path containing Landsat 8 files;
#' this directory must contain MTL and ANG files
#' whereToSave is a character with a directory path where the processed .tif will
#' be stored
#' cloudThr a integer indicating valid threshold used in cloud masking; 2800 for this study
#' EXTENT a extent object to be used for cropping the original scene
#' LABEL character or integer vector; object used to add extra identification to
#' output products.
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
