
library(imputeTS)
library(bfast)

LoadToEnvironment <- function(RData, env = new.env()){
  load(RData, env)
  env
}

get_pixel_matrix <- function(x,lenPeriod){
  output <- matrix(nrow=length(x)/lenPeriod, ncol=lenPeriod)
  
  for(i in seq_len(nrow(output))){
    output[i,] <- x[((i-1) * lenPeriod + 1):(i * lenPeriod)]
  }
  output
}

# NOTE: for this to work, length(x) must be a multiple of lenPeriod
climatology <- function(x, nPeriods, lenPeriod){
  MAT <- get_pixel_matrix(x=x, lenPeriod=lenPeriod)
  
  BOXPLOT <- boxplot(MAT, plot=FALSE)
  
  list(matrix=MAT, boxplot=BOXPLOT)
}

# This functions uses climatology to fill out the 
# first missing value appearing in the group DoY.
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


# dirDATA <- "D:/Desktop_Huawei_2022/changePointsClassification/data/ndvi_gapfill_cells_RData"
dirDATA <- paste0(getwd(),"/RData/ndvi_gapfill_cells")
dataTEST <- LoadToEnvironment(paste0(dirDATA, "/ndvi_gapfill_2000.RData"))$index_array

# --- Estas fueron primeras pruebas

# prueba <- dataTEST[1,1,]
# 
# nYears <- 7
# lengthPeriod <- 23
# 
# matDist <- matrix(nrow = nYears, ncol = lengthPeriod) # years en renglones
# 
# for(i in 1:ncol(matDist)){
#   indices <- (1:nrow(matDist)-1) * ncol(matDist) + i
#   
#   matDist[,i] <- prueba[indices]
# }
# 
# plot(prueba, type="p", pch=8)
# for(i in 1:ncol(matDist)){
#   
#   indices <- (1:nrow(matDist)-1) * ncol(matDist) + i
#   points(indices, matDist[,i], col=i+1, pch=8)
# }
# 
# pruebaBOXPLOT <- boxplot(matDist, col=2:8)

# --- selecciona un píxel al azar

COL <- sample(1:ncol(dataTEST),1)
ROW <- sample(1:nrow(dataTEST),1)

pixel <- dataTEST[ROW,COL,]

climaTEST <- climatology(pixel,7,23)

climaTEST$matrix
boxplot(climaTEST$matrix, col=2:8)
boxplot(climaTEST$boxplot$stats, col=2:8)

# --- rellenado de datos faltantes usando gapfill_climatology

MAT_gapfill_lower <- gapfill_climatology(climatology = climaTEST)
MAT_gapfill_median <- gapfill_climatology(climatology = climaTEST, box="median")
MAT_gapfill_upper <- gapfill_climatology(climatology = climaTEST, box="upper")


# --- verificando visualmente el resultado de llenar datos faltantes

pixel_interpol <- na_interpolation(pixel)
pixel_clim_lower <- c(t(MAT_gapfill_lower)) # c() concatena por columnas
pixel_clim_median <- c(t(MAT_gapfill_median))
pixel_clim_upper <- c(t(MAT_gapfill_upper))

plot(pixel, type="p", pch=8)
indNA <- is.na(pixel)
points((1:length(pixel))[indNA], pixel_interpol[indNA], pch=8, col="red")
# points((1:length(pixel)), pixel_interpol, pch=8, col="red")

plot(pixel, type="p", pch=8)
points((1:length(pixel))[indNA], pixel_clim_lower[indNA], pch=8, col="green")
points((1:length(pixel)), pixel_clim_lower, pch=8, col="green")

plot(pixel, type="p", pch=8)
points((1:length(pixel))[indNA], pixel_clim_median[indNA], pch=8, col="blue")
points((1:length(pixel)), pixel_clim_median, pch=8, col="blue")

plot(pixel, type="p", pch=8)
points((1:length(pixel))[indNA], pixel_clim_upper[indNA], pch=8, col="orange")
points((1:length(pixel)), pixel_clim_upper, pch=8, col="orange")

# --- Aplicando interpol lineal para completar el rellenado

pixel_clim_lower_interpol <- na_interpolation(pixel_clim_lower)
pixel_clim_median_interpol <- na_interpolation(pixel_clim_median)
pixel_clim_upper_interpol <- na_interpolation(pixel_clim_upper)


pixel_ts <- ts(pixel, start = c(2014,1), end = c(2020,23), frequency = 23)
pixel_interpol_ts <- ts(pixel_interpol, start = c(2014,1), end = c(2020,23), frequency = 23)
pixel_clim_lower_ts <- ts(pixel_clim_lower_interpol, start = c(2014,1), end = c(2020,23), frequency = 23)
pixel_clim_median_ts <- ts(pixel_clim_median_interpol, start = c(2014,1), end = c(2020,23), frequency = 23)
pixel_clim_upper_ts <- ts(pixel_clim_upper_interpol, start = c(2014,1), end = c(2020,23), frequency = 23)

plot(pixel_ts, lwd=4)
par(new=TRUE)
plot(pixel_interpol_ts,col="red")

plot(pixel_ts, lwd=4)
par(new=TRUE)
plot(pixel_clim_lower_ts,col="green")

plot(pixel_ts, lwd=4)
par(new=TRUE)
plot(pixel_clim_median_ts,col="blue")

plot(pixel_ts, lwd=4)
par(new=TRUE)
plot(pixel_clim_upper_ts,col="orange")

# --- Aplicando BFAST-01

bf_orig <- bfast01(data=pixel_ts)
plot(bf_orig)

bf <- bfast01(data=pixel_interpol_ts)
# str(bf)
plot(bf)

# --- getting a sense of the dataset
# --- bfast01 can be applied to raw data -with NAs
# --- here we provide a visual comparison of applying
# --- bfast01 to raw pixel vs interpolated pixel

dataTEST <- LoadToEnvironment(paste0(dirDATA, "/ndvi_gapfill_15.RData"))$index_array

for(sim in 1:10){
  COL <- sample(1:ncol(dataTEST),1)
  ROW <- sample(1:nrow(dataTEST),1)
  
  pixel <- dataTEST[ROW,COL,]
  
  clima <- climatology(pixel,7,23)
  MAT_gapfill_median <- gapfill_climatology(climatology = clima, box="median")
  pixel_median <- c(t(MAT_gapfill_median))
  pixel_median_interpol <- na_interpolation(pixel_median)
  pixel_median_ts <- ts(pixel_median_interpol, start = c(2014,1), 
                             end = c(2020,23), frequency = 23)
  
  pixel_interpol <- na_interpolation(pixel)
  
  pixel_ts <- ts(pixel, start = c(2014,1), end = c(2020,23), 
                 frequency = 23)
  
  pixel_interpol_ts <- ts(pixel_interpol, start = c(2014,1), end = c(2020,23), 
                 frequency = 23)


  bf_orig <- bfast01(pixel_ts)
  bf <- bfast01(pixel_interpol_ts)
  bf_median <- bfast01(pixel_median_ts)
    
  bf_orig_0p23 <- bfast01(pixel_ts, bandwidth = 0.23)
  bf_0p23 <- bfast01(pixel_interpol_ts, bandwidth = 0.23)
  bf_median_0p23 <- bfast01(pixel_median_ts, bandwidth = 0.23)
  
  par(mfrow=c(2,3))
  plot(bf_orig, 
       main=paste0("ROW: ", ROW, " COL: ", COL, " breaks: ", bf_orig$breaks))
  plot(bf, 
       main=paste0("ROW: ", ROW, " COL: ", COL, " breaks: ", bf$breaks))
  plot(bf_median, 
       main=paste0("ROW: ", ROW, " COL: ", COL, " breaks: ", bf_median$breaks))
  
  plot(bf_orig_0p23, 
       main=paste0("ROW: ", ROW, " COL: ", COL, " breaks: ", bf_orig_0p23$breaks))
  plot(bf_0p23, 
       main=paste0("ROW: ", ROW, " COL: ", COL, " breaks: ", bf_0p23$breaks))
  plot(bf_median_0p23, 
       main=paste0("ROW: ", ROW, " COL: ", COL, " breaks: ", bf_median_0p23$breaks))
}

# ---
