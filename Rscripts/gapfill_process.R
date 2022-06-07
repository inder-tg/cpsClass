# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- APPLIES functions of gapfill R package to impute missing values in NDVI Landsat 
# --- images used in this project
# =============================================================================

# --- NOTE
# --- Before using this script, we have
# 1. Created a directory called /gapfill_block1/data. In here we have stored 4 
# cropped NDVI files that forms one processing block. In example below, we have
# ndvi_51.tif, ndvi_52.tif, ndvi_73.tif and ndvi_74.tif.

# 2. In the directory created above, we have created a folder called "filled"
# where the final output of this script -a set of .tifs- will be saved.

# 3. In the directory created in 1, we have created a folder called "output"
# where the ouptut of gapfill() will be saved -a set of RData files.

# 4. In the directory created in 1, we have created a folder called "splits".
# In this directory, we have created folders "2016", "2017" and "SNA". In these
# folders we stored sub-sets of the original cropped NDVI files mentioned in 1.
# "/data/splits/2016" will contained the subsets of those images spanning year 2016.
# In example below, ndvi_52 and ndvi_52 correspond to images take durin 2016.
# "/data/splits/SNA" stands for "without NA", in here we must store subsets of
# an image with no missing values, this sub images will help to rasterize the output
# matrices of gapfill. In our project, ndvi_52.tif is such an image
#
# Finally, to monitor the report of this process we used files
# "/data/output/parallel_gapfill_progress.txt"
# "/data/filled/parallel_filled_progress.txt"
# which are created automatically during the process

# =============================================================================
# --- Object names used below reflect the use given in this project but must be 
# --- changed accordingly to fulfill users needs
# =============================================================================

# --- Preparing auxiliary folders and files

source("myFunctions.R")

####-----Images 2x2------####  split_replace

ImagenesC2X2<- mixedsort(list.files(path =paste0(getwd(), "/gapFill_block1/data"), pattern = ".tif",
                                    full.names = T))

ImagenSinNA<- mixedsort(list.files(path =paste0(getwd(), "/gapFill_block1/data"), pattern = ".tif",
                                   full.names = T)) 

stack2017 <- stack(ImagenesC2X2[1:2])
stack2018 <- stack(ImagenesC2X2[3:4])
stackSinNA<-stack(ImagenSinNA[2]) 

#2016
outputpath <- paste0(getwd(), "/gapFill_block1/data/splits/2016")
name <- 2016
split_replace(raster=stack2017, v=52, h=35, outputPath = outputpath, name = name, numCores = 23)

#2017
outputpath1 <- paste0(getwd(), "/gapFill_block1/data/splits/2017")
name <- 2017
split_replace(raster=stack2018, v=52, h=35, outputPath = outputpath1, name = name, numCores = 23)

# Image without NAs
outputpath2 <- paste0(getwd(), "/gapFill_block1/data/splits/SNA")
name <- "SinNA"
split_replace(raster=stackSinNA, v=52, h=35, outputPath = outputpath2, name = name, numCores = 23)

# ---

ListFiles2016 <- getListFiles(pattern = ".tif", path = outputpath, 
                             full.names = T)

ListFiles2017 <- getListFiles(pattern = ".tif", path = outputpath1,
                              full.names = T)

ListFilesSNA <- getListFiles(pattern = ".tif", path = outputpath2,
                             full.names = T)

ListFilesYears <- list(ListFiles2016, ListFiles2017)

Prueba <- stack(ListFiles2016[84])

#Longitude and latitude vectors:
longitude <- as.character( seq(xmin(Prueba), xmax(Prueba), by = 30) )

latitude <- as.character( seq(ymin(Prueba), ymax(Prueba), by = 30))

days <- c(51,73)
years <- 2016:2017

# --- PARALLEL computing for Gapfill()

file.create(paste0(getwd(),"/gapFill_block1/data/output/parallel_gapfill_progress.txt"))
numcores <- detectCores()

cluster <- parallel::makeCluster(numcores-1, outfile="")
registerDoParallel(cluster)

name_RData_output <- paste0(getwd(), "/gapFill_block1/data/output/gapfill_output_cell_") #"E:/Gapfill/Im?genes2x2/output/gapfill_output_cell_"
write(as.character(Sys.time()[1]),
      file = paste0(getwd(),"/gapFill_block1/data/output/parallel_gapfill_progress.txt"), append = T)
output <- foreach( i = 1:length(ListFiles2016), .packages = c("gapfill", "raster") ) %dopar% {
  ARRAY_LAT_LONG_DAYS_YEARS <- get4D_array(listPath = ListFilesYears, i = i,
                                           
                                           latitude = latitude[1:52], longitude = longitude[1:35],
                                           days = days, years = years)
  
  output_gapFill <- Gapfill(data = ARRAY_LAT_LONG_DAYS_YEARS, clipRange = c(-1,1))
  save(output_gapFill, file = paste0(name_RData_output, i, ".RData"))
  if(i %% 50 == 0){
    text <- paste0("Working on cell ", i)
    write(text, file = paste0(getwd(),"/gapFill_block1/data/output/parallel_gapfill_progress.txt"),
          append = T)
  }
}
stopCluster(cluster)
write( as.character(Sys.time()[1]),
       file = paste0(getwd(),"/gapFill_block1/data/output/parallel_gapfill_progress.txt"), 
       append = TRUE)

#####--- Mosaicking after Gapfill ---##### 

# --- PARALLEL computing for mosaicking

file.create(paste0(getwd(),"/gapFill_block1/data/filled/parallel_filled_progress.txt"))

cluster <- parallel::makeCluster(numcores-1, outfile = "")

registerDoParallel(cluster)

write( as.character(Sys.time()[1]),
       file = paste0(getwd(),"/gapFill_block1/data/filled/parallel_filled_progress.txt"), 
       append = TRUE )
output_filled <- foreach( i = 1:length(ListFiles2017), .packages = c("raster", "geoTS") ) %dopar% {
  TEMP <- LoadToEnvironment(paste0( getwd(), "/gapFill_block1/data/output/gapfill_output_cell_", i, ".RData"))
  MASTER <- raster(ListFilesSNA[i])
  
  TEMP_51_2016 <- matrixToRaster(TEMP$output_gapFill$fill[,,1,1], MASTER)
  TEMP_52_2016 <- matrixToRaster(TEMP$output_gapFill$fill[,,2,1], MASTER)
  
  TEMP_73_2017 <- matrixToRaster(TEMP$output_gapFill$fill[,,1,2], MASTER)
  TEMP_74_2017 <- matrixToRaster(TEMP$output_gapFill$fill[,,2,2], MASTER)
  
  if(i %% 100 == 0){
    text <- paste0("Working on cell ", i)
    write(text, file = paste0(getwd(),"/gapFill_block1/data/filled/parallel_filled_progress.txt"),
          append = T)
  }
  return(list(TEMP_51_2016, TEMP_52_2016, TEMP_73_2017, TEMP_74_2017))
  
}
stopCluster(cluster)
write( as.character(Sys.time()[1]),
       file = paste0(getwd(),"/gapFill_block1/data/filled/parallel_filled_progress.txt"), 
       append = TRUE )

# Mosaic for image_51 
MOSAIC_51_2016 <- list()
for(i in 1:length(ListFiles2016)){
  MOSAIC_51_2016[i] <- (output_filled[[i]][1])
}
MOSAIC_51_2016<- mosaicking(MOSAIC_51_2016)
original_51_2016 <- raster(ImagenesC2X2[1])
save_mosaic(original_51_2016, MOSAIC_51_2016)

# Mosaic for image_52
MOSAIC_52_2016<- list()
for(i in 1:length(ListFiles2016)){
  MOSAIC_52_2016[i] <- (output_filled[[i]][2])
}
MOSAIC_52_2016<- mosaicking(MOSAIC_52_2016)
original_52_2016 <- raster(ImagenesC2X2[2])
save_mosaic(original_52_2016, MOSAIC_52_2016)


# Mosaic for image_73
MOSAIC_73_2017 <- list()
for(i in 1:length(ListFiles2018)){
  MOSAIC_73_2017[i] <- (output_filled[[i]][3])
}
MOSAIC_73_2017<- mosaicking(MOSAIC_73_2017)
original_73_2017 <- raster(ImagenesC2X2[3])
save_mosaic(original_73_2017, MOSAIC_73_2017)

# Mosaic for image_74
MOSAIC_74_2017 <- list()
for(i in 1:length(ListFiles2018)){
  MOSAIC_74_2017[i] <- (output_filled[[i]][4])
}
MOSAIC_74_2017 <- mosaicking(MOSAIC_74_2017)
original_74_2017 <- raster(ImagenesC2X2[4])
save_mosaic(original_74_2017, MOSAIC_74_2017)
