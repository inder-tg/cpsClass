
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- MAKES plots shown in Section "bfast01classify: clasificando tendencias"
# =============================================================================

source( paste0(getwd(), "/Rscripts/myFunctions.R") )
load( paste0( getwd(), "/RData/pixels2plot.RData") )

# --- type6
pixel_type6_sign0_interpol <- na_interpolation(pixel_type6_sign0)

pixel_type6_sign0_interpol_ts <- ts(pixel_type6_sign0_interpol, start = c(2014,1), 
                                    end = c(2020,23), frequency = 23)
bf <- bfast01(pixel_type6_sign0_interpol_ts)

# --- type8
clima_type8_sign2 <- climatology(pixel_type8_sign2,7,23)
MAT_gapfill_median_type8_sign2 <- gapfill_climatology(climatology = clima_type8_sign2,
                                                      box="median")
pixel_type8_sign2_median <- c(t(MAT_gapfill_median_type8_sign2))
pixel_type8_sign2_median_interpol <- na_interpolation(pixel_type8_sign2_median)

pixel_type8_sign2_median_ts <- ts(pixel_type8_sign2_median_interpol, 
                                  start = c(2014,1), end = c(2020,23), 
                                  frequency = 23)

bf_median <- bfast01(pixel_type8_sign2_median_ts)

# --- type7

pixel_type7_sign1_interpol <- na_interpolation(pixel_type7_sign1)
pixel_type7_sign1_interpol_ts <- ts(pixel_type7_sign1_interpol, start = c(2014,1), 
                                    end = c(2020,23), frequency = 23)

bf_0p23 <- bfast01(pixel_type7_sign1_interpol_ts, bandwidth = 0.23)

# --- type5

pixel <- pixel_type5_sign0
clima <- climatology(pixel,7,23)
MAT_gapfill_median <- gapfill_climatology(climatology = clima, box="median")
pixel_median <- c(t(MAT_gapfill_median))
pixel_median_interpol <- na_interpolation(pixel_median)
pixel_median_ts <- ts(pixel_median_interpol, start = c(2014,1), 
                      end = c(2020,23), frequency = 23)

bf_median_0p23 <- bfast01(pixel_median_ts, bandwidth = 0.23)

par(mfrow=c(2,2), mar=c(2,2,1,1), adj = 0)
plot(bf, ylab="", xlab="", main="A")
plot(bf_median, ylab="", xlab="", main="B")
plot(bf_median_0p23, ylab="", xlab="", main="C")
plot(bf_0p23, ylab="", xlab="", main="D")
