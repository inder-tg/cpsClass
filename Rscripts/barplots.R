
# --- CODE used in "Clasificación de tendencias de series de tiempo de NDVI 
# --- de imágenes Landsat-7 y Landsat 8 OLI en una zona de la Península de 
# --- Yucatán, México de 2014 a 2020" by Inder Tecuapetla-Gómez, Alfonso 
# --- Carbajal-Domínguez and Valeria Montesinos-Chica.

# =============================================================================
# --- MAKES bar plots shown in Section "RESULTADOS"
# =============================================================================

# --- NOTE
# --- Auxiliary files are in /RData

# ---
usv_names_shorter <- c("Bosque cultivado",
                       "Pastizal cultivado",
                       "Agricultura riego",
                       "Selva baja",
                       "Selva mediana",
                       "Agricultura temporal",
                       "Sabana",
                       "V.S. Arbustiva S. baja",
                       "V.S. Arbórea S. baja",
                       "V.S. Arbustiva S. mediana",
                       "V.S. Arbórea S. mediana",
                       "Tular")

usv_COLORS <- c(rgb(121,130,200, maxColorValue = 255), # riego
                rgb(131,164,240, maxColorValue = 255), # temporal
                rgb(161,229,165, maxColorValue = 255), # bosque
                rgb(190,193,10, maxColorValue = 255), # pastizal
                rgb(1, 0.94,0, maxColorValue = 1), # SABANA
                rgb(0.71,0.2,0.54, maxColorValue = 1), # Selva baja FANDANGO
                rgb(0.93,0.23,0.51, maxColorValue = 1), # Selva mediana CERISE PINK
                rgb(23,185,180, maxColorValue = 255), # Tular
                rgb(0.96,0,0.63, maxColorValue = 1), # V.S. arborea S baja FASHION FUSCHIA
                rgb(1,0.08,0.58, maxColorValue = 1), # V.S. arborea S mediana Deep PINK
                rgb(0.99,0.56,0.67, maxColorValue = 1), # V.S. arbustiva S baja FLAMINGO PINK
                rgb(1,0.74,0.85, maxColorValue = 1)) # V.S. arbustiva S mediana COTTON CANDY


anyos <- c("2015", "2016", "2017", "2018", "2019")

SEGMENTOS <- c("Ambos", "Anterior", "Posterior", "Ninguno")

source( paste0(getwd(), "/Rscripts/myFunctions.R") )

# ---

# --- Clasificación de tendencias de NDVI por tipo de uso de suelo y vegetación

load( paste0( getwd(), "/RData/bars_TYPE_USV.RData") )

df_percent_usv_trendType <-  data.frame(types=rep(as.character(1:8), each=12),
                                        usv=rep(usv_names_shorter,by=4),
                                        porcentaje=c(table_perc_usv_trendType$`1`,
                                                     table_perc_usv_trendType$`2`,
                                                     table_perc_usv_trendType$`3`,
                                                     table_perc_usv_trendType$`4`,
                                                     table_perc_usv_trendType$`5`,
                                                     table_perc_usv_trendType$`6`,
                                                     table_perc_usv_trendType$`7`,
                                                     table_perc_usv_trendType$`8`))

barTypes <- ggplot(data=df_percent_usv_trendType,
                   aes(x=types,
                       y=porcentaje, #))+
                       fill=usv)) +
  geom_bar(stat="identity", 
           width=1,
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="Clases de tendencias") + 
  theme(legend.title = element_text(face="bold"),
        legend.text = element_text(size=14),
        axis.text.x = element_text(size=16, face="bold"),
        axis.text.y = element_text(size=16, face="bold"),
        axis.title.y = element_text(size=24, face="bold"),
        axis.title.x = element_text(size=20, face="bold"))

ggarrange(barTypes, ncol=1, nrow=1, legend="top")

# --- Pardeamiento demorado

load( paste0(getwd(), "/RData/bars_TYPE6_SIGN_USV.RData") )

load( paste0(getwd(), "/RData/bars_TYPE6_YEARS_USV.RData") )

df_percent_type6_sign <-  data.frame(segmentos_relevantes=rep(SEGMENTOS, each=12),
                                     usv=rep(usv_names_shorter, by=4),
                                     porcentaje=c(table_perc_usv_TYPE6_SIGN$`0`,
                                                  table_perc_usv_TYPE6_SIGN$`1`,
                                                  table_perc_usv_TYPE6_SIGN$`2`,
                                                  table_perc_usv_TYPE6_SIGN$`3`))

p <- ggplot(data=df_percent_type6_sign,
            aes(x=segmentos_relevantes,
                y=porcentaje, #))+
                fill=usv)) +
  geom_bar(stat="identity", width=0.85,
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

df_percent_type6_years <- data.frame(years=rep(anyos,each=12),
                                     usv=rep(usv_names_shorter,by=4),
                                     porcentaje=c(table_perc_usv_TYPE6_YEARS$`2015`,
                                                  table_perc_usv_TYPE6_YEARS$`2016`,
                                                  table_perc_usv_TYPE6_YEARS$`2017`,
                                                  table_perc_usv_TYPE6_YEARS$`2018`,
                                                  table_perc_usv_TYPE6_YEARS$`2019`))

q <- ggplot(data=df_percent_type6_years,
            aes(x=years,
                y=porcentaje, #))+
                fill=usv)) +
  geom_bar(stat="identity", width=0.85, 
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

ggarrange(p, q, 
          labels = c("A", "B"), ncol=1, nrow=2, 
          common.legend = TRUE, legend = "top")

# --- Pardeamiento a Enverdecimiento

load( paste0( getwd(), "/RData/bars_TYPE8_SIGN_USV.RData" ) )

load( paste0( getwd(), "/RData/bars_TYPE8_YEARS_USV.RData" ) )

df_percent_type8_sign <-  data.frame(segmentos_relevantes=rep(SEGMENTOS, each=12),
                                     usv=rep(usv_names_shorter, by=4),
                                     porcentaje=c(table_perc_usv_TYPE8_SIGN$`0`,
                                                  table_perc_usv_TYPE8_SIGN$`1`,
                                                  table_perc_usv_TYPE8_SIGN$`2`,
                                                  table_perc_usv_TYPE8_SIGN$`3`))

p8 <- ggplot(data=df_percent_type8_sign,
             aes(x=segmentos_relevantes,
                 y=porcentaje, #))+
                 fill=usv)) +
  geom_bar(stat="identity", width=0.85,
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

df_percent_type8_years <- data.frame(years=rep(anyos,each=12),
                                     usv=rep(usv_names_shorter,by=4),
                                     porcentaje=c(table_perc_usv_TYPE8_YEARS$`2015`,
                                                  table_perc_usv_TYPE8_YEARS$`2016`,
                                                  table_perc_usv_TYPE8_YEARS$`2017`,
                                                  table_perc_usv_TYPE8_YEARS$`2018`,
                                                  table_perc_usv_TYPE8_YEARS$`2019`))

q8 <- ggplot(data=df_percent_type8_years,
             aes(x=years,
                 y=porcentaje,
                 fill=usv)) +
  geom_bar(stat="identity", width=0.85, 
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

ggarrange(p8, q8, 
          labels = c("A", "B"), ncol=1, nrow=2, 
          common.legend = TRUE, legend = "top")


# --- Enverdecimiento demorado

load( paste0(getwd(), "/RData/bars_TYPE5_SIGN_USV.RData") )

load( paste0(getwd(), "/RData/bars_TYPE5_YEARS_USV.RData") )

df_percent_type5_sign <- data.frame(segmentos_relevantes=rep(SEGMENTOS, each=12),
                                    usv=rep(usv_names_shorter, by=4),
                                    porcentaje=c(table_perc_usv_TYPE5_SIGN$`0`,
                                                 table_perc_usv_TYPE5_SIGN$`1`,
                                                 table_perc_usv_TYPE5_SIGN$`2`,
                                                 table_perc_usv_TYPE5_SIGN$`3`))

p5 <- ggplot(data=df_percent_type5_sign,
             aes(x=segmentos_relevantes,
                 y=porcentaje, #))+
                 fill=usv)) +
  geom_bar(stat="identity", width=0.85,
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

df_percent_type5_years <- data.frame(years=rep(anyos,each=12),
                                     usv=rep(usv_names_shorter,by=4),
                                     porcentaje=c(table_perc_usv_TYPE5_YEARS$`2015`,
                                                  table_perc_usv_TYPE5_YEARS$`2016`,
                                                  table_perc_usv_TYPE5_YEARS$`2017`,
                                                  table_perc_usv_TYPE5_YEARS$`2018`,
                                                  table_perc_usv_TYPE5_YEARS$`2019`))

q5 <- ggplot(data=df_percent_type5_years,
             aes(x=years,
                 y=porcentaje, #))+
                 fill=usv)) +
  geom_bar(stat="identity", width=0.85, 
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

ggarrange(p5, q5, 
          labels = c("A", "B"), ncol=1, nrow=2, 
          common.legend = TRUE, legend = "top")

# --- Enverdecimiento a Pardeamiento

load( paste0(getwd(), "/RData/bars_TYPE7_SIGN_USV.RData" ) )

load( paste0( getwd(), "/RData/bars_TYPE7_YEARS_USV.RData" ) )

df_percent_type7_sign <-  data.frame(segmentos_relevantes=rep(SEGMENTOS, each=12),
                                     usv=rep(usv_names_shorter, by=4),
                                     porcentaje=c(table_perc_usv_TYPE7_SIGN$`0`,
                                                  table_perc_usv_TYPE7_SIGN$`1`,
                                                  table_perc_usv_TYPE7_SIGN$`2`,
                                                  table_perc_usv_TYPE7_SIGN$`3`))

p7 <- ggplot(data=df_percent_type7_sign,
             aes(x=segmentos_relevantes,
                 y=porcentaje, #))+
                 fill=usv)) +
  geom_bar(stat="identity", width=0.85,
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

df_percent_type7_years <- data.frame(years=rep(anyos,each=12),
                                     usv=rep(usv_names_shorter,by=4),
                                     porcentaje=c(table_perc_usv_TYPE7_YEARS$`2015`,
                                                  table_perc_usv_TYPE7_YEARS$`2016`,
                                                  table_perc_usv_TYPE7_YEARS$`2017`,
                                                  table_perc_usv_TYPE7_YEARS$`2018`,
                                                  table_perc_usv_TYPE7_YEARS$`2019`))

q7 <- ggplot(data=df_percent_type7_years,
             aes(x=years,
                 y=porcentaje, #))+
                 fill=usv)) +
  geom_bar(stat="identity", width=0.85, 
           position=position_dodge2()) +
  scale_fill_manual(values=usv_COLORS,
                    aesthetics = "fill",
                    name="USV") +
  labs(y="%", x="") + 
  theme(legend.title = element_text(size=10, face="bold"),
        legend.key.size = unit(0.25, 'cm'),
        legend.text = element_text(size=8),
        axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(size=12, face="bold"),
        axis.title.y = element_text(face="bold"))

ggarrange(p7, q7, 
          labels = c("A", "B"), ncol=1, nrow=2, 
          common.legend = TRUE, legend = "top")

