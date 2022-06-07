
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- APPLIES bfast01 to pixels of dataset using parallel coding to speed-up
# =============================================================================

# --- REQUIRED Files and folders ---

# --- dirDATA is a character with the full path to directory containing RDatas
# ---         to analize. Below there is an example showing ndvi_gapfill_cells_interpol_rasterToPoints,
# ---         this folder contains 2714 RData files where NDVI Landsat-7 and Landsat 8
# ---         OLI processed images were stored.

# ---  /RData is a folder containing RData files.
# ---  /progressReport is a folder where reports of different process are saved.
# ---  /bw_0p15/interpol/CP
# ---  /bw_0p15/interpol/SIGN
# ---  /bw_0p15/interpol/STABLE
# ---  /bw_0p15/interpol/TYPE
# ---  /bw_0p15/interpol/YEARS are directories to save the output of this script.
# ---                          More precisely, CP, SIGN, STABLE, TYPE and YEARS
# ---                          will store change-point estimate, significance of
# ---                          estimated trends (post and pre-change point), stability
# ---                          of estimated trends, trend class and years with change-point,
# ---                          respectively. 

# --- NOTE ---

# --- Example below shows application of this script with bandwidth=0.15 (bfast01) and
# --- interpol (method complementary to gapfill). Should required different bandwidth
# --- or hybrid complementary gap-filling method, then bw parameter must be changed 
# --- in getBreak() or lines 92-95 must be uncommented below, respectively; additionally,
# --- new and corresponding directories must be created to save output.
# --- 

source("myFunctions.R")

dirDATA <- "D:/Desktop_Huawei_2022/changePointsClassification/data/ndvi_gapfill_cells_interpol_rasterToPoints"

listRDataFiles <- mixedsort(list.files(path=dirDATA, pattern=".RData", 
                                       full.names=TRUE))
# ---

progressReportFile <- paste0( getwd(), 
                              "/RData/progressReports/progressReportBFAST_interpol_bw0p15.txt")
file.create(path = progressReportFile, showWarnings = F )

write( "===BFAST01 analysis began at===", 
       file=progressReportFile, append=T)
write( as.character(Sys.time()[1]), file=progressReportFile, 
       append=T)

for(i in 1:length(listRDataFiles)){ # 1:10
  dataTEST <- LoadToEnvironment(listRDataFiles[i])$df
  
  if(i %% 100 == 0){
    write( paste0("--- Working on cell ", i, " ---"), 
           file=progressReportFile, append=T )
  }
  # ---
  df_YEARS <- matrix(nrow=nrow(dataTEST), ncol=3)
  df_YEARS[,1:2] <- dataTEST[,1:2]
  
  df_CP <- matrix(nrow=nrow(dataTEST), ncol=3)
  df_CP[,1:2] <- dataTEST[,1:2]
  
  df_TYPE <- matrix(nrow=nrow(dataTEST), ncol=3)
  df_TYPE[,1:2] <- dataTEST[,1:2]
  
  df_SIGN <- matrix(nrow=nrow(dataTEST), ncol=3)
  df_SIGN[,1:2] <- dataTEST[,1:2]
  
  df_STABLE <- matrix(nrow=nrow(dataTEST), ncol=3)
  df_STABLE[,1:2] <- dataTEST[,1:2]
  # ---
  
  # --- parallel processing
  kluster <- parallel::makeCluster(6, outfile="")
  registerDoParallel(kluster)
  
  output <- foreach(j=1:nrow(dataTEST), .combine="rbind",
                    .packages=c("bfast", "imputeTS")) %dopar% {
                      # ---
                      pixel <- dataTEST[j,3:ncol(dataTEST)]
                      
                      # pixel_interpol <- na_interpolation(pixel)
                      # 
                      # clima <- climatology(pixel,7,23)
                      # MAT_gapfill_median <- gapfill_climatology(climatology=clima, box="median")
                      # pixel_median <- c(t(MAT_gapfill_median))
                      # pixel_median_interpol <- na_interpolation(pixel_median)
                      # ---
                      BREAKS <- getBreak(data=pixel_interpol, start=2014, end=2020)
                      
                      YEARS <- getYear(bp=BREAKS$bPs, start=2014, end=2020)
                      # ---
                      s <- c(YEARS, BREAKS$bPs, BREAKS$type, 
                             BREAKS$significance, BREAKS$stability)
                      
                      if(i %% 100==0){
                        if(j %% 250==0){
                          text <- paste0("Working on ROW: ", j)
                          write(text, file=progressReportFile, append=TRUE)
                        }
                      }
                      
                      return(s)
                    }
  stopCluster(kluster)
  # --- END parallel processing
  
  # ---
  df_YEARS[,3] <- output[,1]
  df_CP[,3] <- output[,2]
  df_TYPE[,3] <- output[,3]
  df_SIGN[,3] <- output[,4]
  df_STABLE[,3] <- output[,5]
  # ---
  
  save(df_YEARS, file=paste0(getwd(),"/RData/bw_0p15/interpol/YEARS/cell_", i, ".RData"))
  save(df_CP, file=paste0(getwd(),"/RData/bw_0p15/interpol/CP/cell_", i, ".RData"))
  save(df_TYPE, file=paste0(getwd(),"/RData/bw_0p15/interpol/TYPE/cell_", i, ".RData"))
  save(df_SIGN, file=paste0(getwd(),"/RData/bw_0p15/interpol/SIGN/cell_", i, ".RData"))
  save(df_STABLE, file=paste0(getwd(),"/RData/bw_0p15/interpol/STABLE/cell_", i, ".RData"))
  
  rm(dataTEST, df_YEARS, df_CP, df_TYPE, df_SIGN, df_STABLE)
  
  if(i %% 100 ==0){
    write( "------------------------ END", 
           file=progressReportFile, append=TRUE)
  }
  
}

write( as.character(Sys.time()[1]), file=progressReportFile, append=T)
write( "===BFAST01 analysis ended===", file=progressReportFile, append=T)


