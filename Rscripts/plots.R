
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- MAKES plots shown in Section 4.3.2 "bfast01classify: clasificando tendencias"
# =============================================================================

# --- Required FILES

# --- dirINPUT is a character with the full path to directory containing RDatas
# ---          to analize. Below is an example showing ndvi_gapfill_cells_rasterToPoints,
# ---          this folder contains 2714 RData files where NDVI Landsat-7 and Landsat 8
# ---          OLI (previously) processed images were stored. No interpolation was 
# ---          applied to the files stored in these RData files.

dirTIF_bw0p15 <- list.files(path=paste0(getwd(), "/TIF/bw0p15"),
                            full.names=TRUE)

listTIF_bw0p15_interpol <- list.files(path=dirTIF_bw0p15[2], 
                                    pattern=".tif",
                                    full.names=TRUE)

stackSIGN <- raster(listTIF_bw0p15_interpol[1])

stackTYPE <- raster(listTIF_bw0p15_interpol[2])
  
stackYEARS <- raster(listTIF_bw0p15_interpol[3])

# dirINPUT is an auxiliary directory where 
dirINPUT <- "D:/Desktop_Huawei_2022/changePointsClassification/data/ndvi_gapfill_cells_rasterToPoints"
cellsFILEs <- mixedsort(list.files(path=dirINPUT,
                                   pattern=".RData",
                                   full.names=TRUE))
# ---

TYPE6_SIGN0_interpol <- stackTYPE
TYPE6_SIGN0_interpol[ stackSIGN !=0 ] <- NA
TYPE6_SIGN0_interpol[ stackTYPE !=6 ] <- NA

xy$x <- 175023.3; xy$y <- 2092906  

CELL_TYPE6 <- 46*7+42

cell <- LoadToEnvironment(cellsFILEs[CELL_TYPE6])$df

xyCELL <- get_timeSeries_byClicking(c(xy$x, xy$y), 
                                    df=cell)

xy
cell[xyCELL$coord,1:2]

pixel_type6_sign0 <- cell[xyCELL$coord,3:ncol(cell)]

clima <- climatology(pixel_type6_sign0,7,23)
MAT_gapfill_median <- gapfill_climatology(climatology = clima, box="median")
pixel_median <- c(t(MAT_gapfill_median))
pixel_median_interpol <- na_interpolation(pixel_median)
pixel_median_ts <- ts(pixel_median_interpol, start = c(2014,1), 
                      end = c(2020,23), frequency = 23)

pixel_interpol <- na_interpolation(pixel_type6_sign0)

pixel_ts <- ts(pixel_type6_sign0, start = c(2014,1), end = c(2020,23), 
               frequency = 23)

pixel_interpol_ts <- ts(pixel_interpol, start = c(2014,1), end = c(2020,23), 
                        frequency = 23)

bf <- bfast01(pixel_interpol_ts)

# interruption: decrease with positive break
# --- to SAVE
# --- took bfast on pixel interpolated
par(mfrow=c(1,1), mar=c(4,4,2,1))
plot(bf)


# ---

TYPE8_SIGN2_interpol <- stackTYPE
TYPE8_SIGN2_interpol[ stackSIGN !=2 ] <- NA
TYPE8_SIGN2_interpol[ stackTYPE !=8 ] <- NA

xy$x <- 158683.4; xy$y <- 2047435

CELL_TYPE8 <- 46*36+26

cell <- LoadToEnvironment(cellsFILEs[CELL_TYPE8])$df

xyCELL <- get_timeSeries_byClicking(c(xy$x, xy$y), 
                                    df=cell)

xy
cell[xyCELL$coord,1:2]

pixel_type8_sign2 <- cell[xyCELL$coord,3:ncol(cell)]

clima <- climatology(pixel_type8_sign2,7,23)
MAT_gapfill_median <- gapfill_climatology(climatology = clima, box="median")
pixel_median <- c(t(MAT_gapfill_median))
pixel_median_interpol <- na_interpolation(pixel_median)
pixel_median_ts <- ts(pixel_median_interpol, start = c(2014,1), 
                      end = c(2020,23), frequency = 23)

pixel_interpol <- na_interpolation(pixel_type8_sign2)

pixel_ts <- ts(pixel_type8_sign2, start = c(2014,1), end = c(2020,23), 
               frequency = 23)

pixel_interpol_ts <- ts(pixel_interpol, start = c(2014,1), end = c(2020,23), 
                        frequency = 23)

bf_median <- bfast01(pixel_median_ts)

# reversal: decrease to increase
# --- to SAVE
# --- took bfast on pixel interpolated
par(mfrow=c(1,1), mar=c(4,4,2,1))
plot(bf_median)


# ---

TYPE7_SIGN1_interpol <- stackTYPE
TYPE7_SIGN1_interpol[ stackSIGN !=1 ] <- NA
TYPE7_SIGN1_interpol[ stackTYPE !=7 ] <- NA

xy$x <- 142978.5; xy$y <- 2036252

CELL_TYPE7 <- 46*43+11

cell <- LoadToEnvironment(cellsFILEs[CELL_TYPE7])$df

xyCELL <- get_timeSeries_byClicking(c(xy$x, xy$y), 
                                    df=cell)

xy
cell[xyCELL$coord,1:2]

pixel_type7_sign1 <- cell[xyCELL$coord,3:ncol(cell)]

clima <- climatology(pixel_type7_sign1,7,23)
MAT_gapfill_median <- gapfill_climatology(climatology = clima, box="median")
pixel_median <- c(t(MAT_gapfill_median))
pixel_median_interpol <- na_interpolation(pixel_median)
pixel_median_ts <- ts(pixel_median_interpol, start = c(2014,1), 
                      end = c(2020,23), frequency = 23)

pixel_interpol <- na_interpolation(pixel_type7_sign1)

pixel_ts <- ts(pixel_type7_sign1, start = c(2014,1), end = c(2020,23), 
               frequency = 23)

pixel_interpol_ts <- ts(pixel_interpol, start = c(2014,1), end = c(2020,23), 
                        frequency = 23)

bf_0p23 <- bfast01(pixel_interpol_ts, bandwidth = 0.23)

# interruption: increase with negative break
# --- to SAVE
# --- took bfast on pixel interpolated
par(mfrow=c(1,1), mar=c(4,4,2,1))
plot(bf_0p23)

# ---

TYPE5_SIGN0_interpol <- stackTYPE
TYPE5_SIGN0_interpol[ stackSIGN != 0 ] <- NA
TYPE5_SIGN0_interpol[ stackTYPE != 5 ] <- NA

# xy <- locator() # 
xy$x <- 137823.7; xy$y <- 2069144

CELL_TYPE5 <- 46*22+6

cell <- LoadToEnvironment(cellsFILEs[CELL_TYPE5])$df

xyCELL <- get_timeSeries_byClicking(c(xy$x, xy$y), 
                                    df=cell)

xy
cell[xyCELL$coord,1:2]

pixel_type5_sign0 <- cell[xyCELL$coord,3:ncol(cell)]

clima <- climatology(pixel_type5_sign0,7,23)
MAT_gapfill_median <- gapfill_climatology(climatology = clima, box="median")
pixel_median <- c(t(MAT_gapfill_median))
pixel_median_interpol <- na_interpolation(pixel_median)
pixel_median_ts <- ts(pixel_median_interpol, start = c(2014,1), 
                      end = c(2020,23), frequency = 23)

pixel_interpol <- na_interpolation(pixel_type5_sign0)

pixel_ts <- ts(pixel_type5_sign0, start = c(2014,1), end = c(2020,23), 
               frequency = 23)

pixel_interpol_ts <- ts(pixel_interpol, start = c(2014,1), end = c(2020,23), 
                        frequency = 23)

bf_median <- bfast01(pixel_median_ts)

# interruption: increase with negative break
# --- to SAVE
par(mfrow=c(1,1), mar=c(4,4,2,1))
plot(bf_median)

# ---

save(pixel_type6_sign0, 
     pixel_type8_sign2, 
     pixel_type7_sign1, 
     pixel_type5_sign0,
     file = "C:/Users/inder/OneDrive/Desktop/cpsClassification/RData/pixels2plot.RData")

