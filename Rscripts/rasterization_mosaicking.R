
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- RASTERIZE matrices which are the output of bfast01_analysis.R and then create
# --- mosaics from these files
# =============================================================================

# --- NOTE
# --- This script cover the case 
# --- Complementary gapfilling method: LINEAR interpolation
# --- bfast01 parameters: bandwidth=0.15

# --- In order for this script to cover other cases, the corresponding
# --- folders must be created.

# --- REQUIRED folders

# --- /RData/bw0p15 

# --- /bw0p15/hybrid, 
# --- /bw0p15/interpol 
# --- /bw0p15/rawData

# --- /hybrid/TYPE, 
# --- /hybrid/YEARS 
# --- /hybrid/SIGN  in these folders the rasterized and mosaicked version
# ---               of the output of bfast01_analysis.R are stored.

# -----------------------------------------------------------------------------

source( paste0(getwd(), "/Rscripts/myFunctions.R") )

# dirSNA contains the full path to a folder with a cropped NDVI image without NAs 
dirSNA <- "C:/Users/inder/OneDrive/Desktop/cpsClassification/data/SNA"

listTIFFiles <- mixedsort(list.files(path=dirSNA,
                                     pattern=".tif",
                                     full.names=TRUE))

listTYPEFiles <- mixedsort(list.files(path=paste0(getwd(),"/RData/bw0p23/rawData/TYPE"),
                                    pattern=".RData",
                                    full.names=TRUE))

# listYEARSFiles <- mixedsort(list.files(path=paste0(getwd(),"/RData/bw0p23/rawData/YEARS"),
#                                       pattern=".RData",
#                                       full.names=TRUE))

# listSIGNFiles <- mixedsort(list.files(path=paste0(getwd(),"/RData/bw0p23/rawData/SIGN"),
#                                       pattern=".RData", 
#                                       full.names=TRUE))

# --- PARALLEL computing for rasterization

dirBASE <- "D:/Desktop_Huawei_2022/changePointsClassification/data/bfast_analysis"

DIRS <- list.dirs(path=dirBASE)

dirOUTPUT <- DIRS[25]

progressReportFile <- paste0( getwd(), "/RData/progressReports/progressReport_TYPEmap_bw0p15_interpol.txt")
file.create(path=progressReportFile, showWarnings=F)

# progressReportFile <- paste0( getwd(), "/RData/progressReports/progressReport_YEARSmap_bw0p15_hybrid.txt")
# file.create(path=progressReportFile, showWarnings=F)

# progressReportFile <- paste0( getwd(), "/RData/progressReports/progressReport_SIGNmap_bw0p15_rawData.txt")
# file.create(path=progressReportFile, showWarnings=F)

write( "===Rasterization of TYPE matrices began at===", 
       file=progressReportFile, append=T)
write( as.character(Sys.time()[1]), file=progressReportFile, 
       append=T)

# --- parallel processing
kluster <- parallel::makeCluster(5, outfile="")
registerDoParallel(kluster)

output <- foreach(i=1:length(listTYPEFiles), .packages="raster") %dopar% {
  dataTEST <- LoadToEnvironment(listTYPEFiles[i])
  # dataTEST <- LoadToEnvironment(listYEARSFiles[i])
  # dataTEST <- LoadToEnvironment(listSIGNFiles[i])
  
  projTEMP <- raster(listTIFFiles[i])
  
  PROYECCION <- raster::projection(projTEMP)
  
  raster_df <- matrixToRaster_test(matrix=dataTEST$df_TYPE, 
                                   projection=PROYECCION)
  
  if(i %% 250==0){
    text <- paste0("Rasterizing cell: ", i)
    write(text, file=progressReportFile, append=TRUE)
  }
  
  writeRaster(raster_df,
              filename=paste0(dirOUTPUT, "/TYPE_cell_", i),
              format="GTiff", overwrite=TRUE, datatype="INT2S")
  
  # writeRaster(raster_df, 
  #             filename=paste0(dirOUTPUT, "/SIGN_cell_", i), 
  #             format="GTiff", overwrite=TRUE, datatype="INT2S")
  
  s <- TRUE
  return(s)
}
stopCluster(kluster)
# --- END parallel processing
write( as.character(Sys.time()[1]), file=progressReportFile, append=T)
write( "===Rasterization ended here===", file=progressReportFile, append=T)

# --- mosaicking 

listFilesTYPE <- mixedsort(list.files(path=dirOUTPUT,
                                    pattern=".tif",
                                    full.names=TRUE))

# listFilesYEARS <- mixedsort(list.files(path=dirOUTPUT,
#                                       pattern=".tif",
#                                       full.names=TRUE))

# listFilesSIGN <- mixedsort(list.files(path=dirOUTPUT,
#                                       pattern=".tif",
#                                       full.names=TRUE))

MOSAICO <- list()

for(i in 1:length(listFilesTYPE)){
  TEMP <- raster(listFilesTYPE[i])
  MOSAICO[[i]] <- TEMP
}

# for(i in 1:length(listFilesYEARS)){
#   TEMP <- raster(listFilesYEARS[i])
#   MOSAICO[[i]] <- TEMP
# }

# for(i in 1:length(listFilesSIGN)){
#   TEMP <- raster(listFilesSIGN[i])
#   MOSAICO[[i]] <- TEMP
# }

MOSAICO$fun <- max

MOSAICO_TYPE <- do.call(what=mosaic, args=MOSAICO)
# MOSAICO_YEARS <- do.call(what=mosaic, args=MOSAICO)
# MOSAICO_SIGN <- do.call(what=mosaic, args=MOSAICO)

# par(mfrow=c(1,1))
# plot(MOSAICO_TYPE, main="0p15_interpol_TYPEmap")

writeRaster(MOSAICO_TYPE,
            filename=paste0(getwd(), "/TIF/bw0p15/interpol/TYPEmap"),
            format="GTiff", datatype="INT2S", overwrite=TRUE)
