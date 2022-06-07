
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- APPLIES functions of RStoolbox to Landsat 8 OLI scenes
# =============================================================================

# --- NOTE
# --- Each Landsat scene must be stored in a separate folder

# --- REQUIRED files and folders ---

#'  inputDIR is a directory's name containing all folders with Landsat scenes. 
#'           Below is an example showing "/IMAGENES LANDSAT8" created on an external 
#'           unit storage used in this project.
#'          
#' outputDIR is a directory's name where the output of get_radCor_clouds_NDVI_crop()
#'           is be saved. Below is an example showing "NDVIS_25092021/" created on
#'           an external unit storage used in this project.
#'           
#'    LABELS is a vector with characters or integers used to create filename
#'           of get_radCor_clouds_NDVI_crop() output. See comments in the example 
#'           below.
#'         
      
# --- auxiliary functions

addid <- function(id){
  zeros <- ifelse(nchar(id)==1, "00", 
                  ifelse(nchar(id)==2, "0", ""))
  paste0(zeros, id)
}

# ---

inputDIR <- "D:/LANDSAT8_PROCESO/IMAGENES LANDSAT8" # change accordingly

outputDIR <- "D:/LANDSAT8_PROCESO/NDVIS_25092021/" # change accordingly

listDIRS <- list.dirs(path = mainDIR,
                      full.names = TRUE)
listDIRS <- listDIRS[-1] # this removes the root directory from list

LABELS <- seq(1,161,1)[-c(20,57,94)] # our project had 161 scenes to process

for(i in 1:length(listDIRS)){
  
  iD <- addid(LABELS[i])
  
  get_radCor_clouds_NDVI_crop(PATH=listDIRS[i],
                              whereToSave=outputDIR,
                              cloudThr=2800,
                              EXTENT=extent(c(118035, 198135, 2001465, 2110995)),
                              LABEL=iD)
}
