
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- MAKES visualizations shown in Section "RESULTADOS"
# =============================================================================

# --- NOTE
# --- Auxiliary files are in /RData

source( paste0(getwd(), "/Rscripts/myFunctions.R") )
load( paste0( getwd(), "/RData/SHP.RData") )

# --- mapA
load( paste0( getwd(), "/RData/mapA.RData") )

shp_main_A <- tm_shape(SHP) +
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "", 
            # outer.margins = c(B,L,T,R)
            outer.margins = c(0.015,0.015,0,0.015),
            inner.margins = c(0,0.015,0,0.45),
            main.title.size = 1)

shp_selvaBaja <- tm_shape(SHP_selvaBaja) +
  tm_borders(lwd = 1, col=usv_COLORS[6]) +
  tm_add_legend("line", 
                labels=usv_names_shorter[4], 
                lwd=4,
                col=usv_COLORS[6]) +
  tm_layout(legend.outside=FALSE,
            # fontface=2,
            legend.text.size = 1,
            legend.position = c("right", "top"),
            legend.bg.alpha = 1)

shp_selvaMediana <- tm_shape(SHP_selvaMediana) +
  tm_borders(lwd = 1, col=usv_COLORS[7]) +
  tm_add_legend("line", 
                labels=usv_names_shorter[5], 
                lwd=4,
                col=usv_COLORS[7]) +
  tm_layout(legend.outside=FALSE,
            # fontface=2,
            legend.text.size = 1,
            legend.position = c("right", "top"),
            legend.bg.alpha = 1)

shp_VSArboreaSelvaMediana <- tm_shape(SHP_VSArboreaSelvaMediana) +
  tm_borders(lwd = 1, col=usv_COLORS[10]) +
  tm_add_legend("line", 
                labels=usv_names_shorter[10], 
                lwd=4,
                col=usv_COLORS[10]) +
  tm_layout(legend.outside=FALSE,
            # fontface=2,
            legend.text.size = 1,
            legend.position = c("right", "top"),
            legend.bg.alpha = 1)

shp_pastizal <- tm_shape(SHP_Pastizal) +
  tm_borders(lwd = 1, col=usv_COLORS[4]) +
  tm_add_legend("line", 
                labels=usv_names_shorter[2], 
                lwd=4,
                col=usv_COLORS[4]) +
  tm_layout(legend.outside=FALSE,
            # fontface=2,
            legend.text.size = 1,
            legend.position = c("right", "top"),
            legend.bg.alpha = 1)

shp_tular <- tm_shape(SHP_Tular) +
  tm_borders(lwd = 1, col=usv_COLORS[8]) +
  tm_add_legend("line", 
                labels=usv_names_shorter[12], 
                lwd=4,
                col=usv_COLORS[8]) +
  tm_layout(legend.outside=FALSE,
            # fontface=2,
            legend.text.size = 1,
            legend.position = c("right", "top"),
            legend.bg.alpha = 1)

mapA <- shp_main_A + shp_selvaBaja + shp_selvaMediana + 
  shp_VSArboreaSelvaMediana +
  shp_pastizal + shp_tular + 
  map_TYPE6_selvaMediana_A +
  map_TYPE6_selvaBaja +
  map_TYPE6_pastizal +
  map_TYPE6_tular

mapA

# --- mapB

load( paste0( getwd(), "/RData/mapB.RData") )

shp_main_B <- tm_shape(SHP) +
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "B", 
            main.title.fontface = 2,
            outer.margins = c(0.015,0.015,0,0.015),
            # inner.margins = c(0,0.015,0,0.25),
            inner.margins = c(0,0.015,0,0.45),
            main.title.size = 1)

mapB <- shp_main_B +
  shp_selvaMediana +
  shp_VSArboreaSelvaMediana +
  shp_pastizal +
  map_TYPE8_selvaMediana +
  map_TYPE8_VSArboreaSelvaMediana +
  map_TYPE8_pastizal_B

mapB

# --- mapC

load( paste0( getwd(), "/RData/mapC.RData") )

shp_main_C <- tm_shape(SHP) +
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "C", 
            main.title.fontface = 2,
            outer.margins = c(0.015,0.015,0,0.015),
            # inner.margins = c(0,0.15,0,0.015),
            inner.margins = c(0,0.45,0,0.015),
            main.title.size = 1)

mapC <- shp_main_C +
  shp_selvaMediana +
  shp_selvaBaja +
  map_TYPE5_selvaMediana +
  map_TYPE5_selvaBaja_C

mapC

# --- mapD

load( paste0( getwd(), "/RData/mapD.RData") )

shp_main_D <- tm_shape(SHP) +
  tm_borders(lwd = 2) + 
  tm_layout(main.title = "D", 
            main.title.fontface = 2,
            outer.margins = c(0.015,0.015,0,0.015),
            inner.margins = c(0,0.015,0,0.3),
            main.title.size = 1)

shp_tular_D <- tm_shape(SHP_Tular) +
  tm_borders(lwd = 1, col=usv_COLORS[8])

mapD <- shp_main_D +
  shp_selvaBaja +
  shp_tular +
  map_TYPE7_tular +
  map_TYPE7_selvaBaja_D

mapD
