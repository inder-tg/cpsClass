Data and code used in the paper "Clasificación de tendencias de series de tiempo de NDVI de imágenes  Landsat-7 y Landsat 8 OLI en una zona de la Península de Yucatán, México de 2014 a 2020." Joint work with Alfonso Carbajal-Domínguez and Valeria Montesinos-Chica.

# INSTRUCTIONS

In ```/Rscripts``` are the following files (the order here reflect the order in which the scripts should be applied):

  1. ```preProcessing_landsatScenes.R``` applies some functions of
  [RStoolbox](https://cran.r-project.org/package=RStoolbox) to Landsat 8 OLI scenes. Allows the application of 
  a radiometric correction, clouds and cloud shadows mask creation, NDVI calculation and image cropping.
  
  2. ```gapfill_process.R``` applies some functions of [gapfill](https://cran.r-project.org/package=gapfill) 
  to impute missing values in NDVI Landsat images used in this project. Auxiliary folders must be created, 
  see instructions in preamble.
  
  3. ```bfast01_analysis.R``` applies ```bfast01()``` of [bfast](https://cran.r-project.org/package=bfast) 
  to pixels of NDVI time series using parallel coding to speed-up. Auxiliary folders must be created, see
  instructions in preamble.
  
  4. ```rasterization_mosaicking.R``` rasterizes matrices which are output of ```bfast01_analysis.R``` and 
  then create mosaics from these files. See details in the preamble.
  
  5. ```plots.R``` makes plots shown in _Section "bfast01classify: clasificando tendencias"_. Should users 
  desire to replicate paper's output, some files are available upon request. Should users want to utilize this
  script for their own purposes then some files must be created along the way. See instructions in preamble.
  
  6. ```maps.R``` makes visualizations shown in _Section "RESULTADOS"_. See further instructions in preamble.
  
# DISCLAIMER

Object names used in these scripts made sense for us and our analysis, but must be changed accordingly
to fulfill users needs.



