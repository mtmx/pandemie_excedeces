library(tidyverse)
library(data.table)
library(stringi)
library(stringr)
library(lubridate)
library(scales)
library(sf)
library(magick)
library(COGugaison)
library(areal)
library(cartogram)
library(hrbrthemes)

# récupération mailles administratives

library(CARTElette)
COMM_FRMETDOM_2016 <- charger_carte(COG=2016,nivsupra="COM")
DEP_FRMETDOM_2016 <- charger_carte(COG=2016,nivsupra="DEP")
library(rmapshaper)
DEP_FRMETDOM_2016.s <- DEP_FRMETDOM_2016 %>% ms_simplify(keep = 0.02)

# récupération fichier décès insee

tmp <- tempdir()
tmp <- str_replace_all(tmp,"//","/")

if (dir.exists(paste0(tmp,"/2020-04-24_detail"))) {
  insee_deces <- fread( paste0(tmp, "/2020-04-24_detail/DC_jan2018-avr2020_det.csv"))
  
} else {
  download.file("https://www.insee.fr/fr/statistiques/fichier/4470857/2020-04-24_detail.zip",paste0(tmp,"/2020-04-24_detail.zip"))
  unzip(paste0(tmp,"/2020-04-24_detail.zip"), exdir = paste0(tmp,"/2020-04-24_detail")) 
  file.remove(paste0(tmp,"/2020-04-24_detail.zip")) 
  insee_deces <- fread( paste0(tmp, "/2020-04-24_detail/DC_jan2018-avr2020_det.csv"))
  
}

# format date
insee_deces <- insee_deces %>%
  mutate(MDEC = str_pad(MDEC, width = 2,side = "left", pad = "0"),
         JDEC = str_pad(JDEC, width = 2,side = "left", pad = "0")) %>%
  mutate(MNAIS = str_pad(MNAIS, width = 2,side = "left", pad = "0"),
         JNAIS = str_pad(JNAIS, width = 2,side = "left", pad = "0")) %>%
  mutate(date_deces_fmt = as.Date(paste(ADEC,"-",MDEC,"-",JDEC,sep=""),"%Y-%m-%d")) %>%
  mutate(date_naissance_fmt = as.Date(paste(ANAIS,"-",MNAIS,"-",JNAIS,sep=""),"%Y-%m-%d"))  %>%
  as.data.frame()  %>% 
  mutate(age = as.numeric(ADEC) - as.numeric(ANAIS))


#### fonction pour calculer et sortir une carte par jour

f_out_img_dor <- function(jour){
  
  
  COMM_deces_jour_j_2019 <- insee_deces %>% 
    as.data.frame() %>%
    filter(date_deces_fmt >= as.Date(jour, "%Y-%m-%d")-365-3 & date_deces_fmt <= as.Date(jour, "%Y-%m-%d")-365+3|date_deces_fmt >= as.Date(jour, "%Y-%m-%d")-730-3 & date_deces_fmt <= as.Date(jour, "%Y-%m-%d")-730+3) %>%
    mutate(cpt = 1) %>%
    mutate(date_deces_fmt_2020 = as.Date(paste(2020,"-",MDEC,"-",JDEC,sep=""),"%Y-%m-%d")) %>%
    mutate(COMDEC = case_when(substr(COMDEC,1,2) %in% '75' ~ "75056",
                              substr(COMDEC,1,3) %in% '132' ~ "13055",
                              substr(COMDEC,1,4) %in% '6938' ~ "69123",
                              TRUE ~ COMDEC)) %>%
    group_by(COMDEC ) %>%
    summarize(nb = sum(cpt) ) %>%
    mutate(nb = nb /2)
  
  COMM_deces_jour_j_2020 <- insee_deces %>% 
    as.data.frame() %>%
    filter(date_deces_fmt >= as.Date(jour, "%Y-%m-%d")-3 & date_deces_fmt <= as.Date(jour, "%Y-%m-%d")+3) %>%
    mutate(cpt = 1) %>%
    mutate(date_deces_fmt_2020 = as.Date(paste(2020,"-",MDEC,"-",JDEC,sep=""),"%Y-%m-%d")) %>%
    mutate(COMDEC = case_when(substr(COMDEC,1,2) %in% '75' ~ "75056",
                              substr(COMDEC,1,3) %in% '132' ~ "13055",
                              substr(COMDEC,1,4) %in% '6938' ~ "69123",
                              TRUE ~ COMDEC)) %>%
    group_by(COMDEC ) %>%
    summarize(nb = sum(cpt) )
  
  
  COMM_deces_jour_j <-
    table_supracom_2016 %>%
    select(CODGEO) %>%
    left_join(COMM_deces_jour_j_2020 %>%
                rename(nb_2020 = nb),
              by = c("CODGEO" = "COMDEC" )) %>%
    left_join(COMM_deces_jour_j_2019 %>%
                rename(nb_2019 = nb), 
              by = c("CODGEO" = "COMDEC")) %>%
    mutate(nb_2020 = replace_na(nb_2020, 0),
           nb_2019 = replace_na(nb_2019, 0)) %>%
    mutate(nb_2020 = nb_2020 / 7,
           nb_2019 = nb_2019 / 7)
  
  ##### interpolation via areal
  
  fr_grid.hex_stats.sf.jour_j <- COMM_FRMETDOM_2016 %>%
    select(INSEE_COM ,NOM_COM) %>%
    left_join(COMM_deces_jour_j ,
              by = c("INSEE_COM" = "CODGEO")) %>%
    select(nb_2020, nb_2019) %>%
    st_interpolate_aw(to = fr_grid.hex, extensive = TRUE) 
  
  fr_grid.hex_stats.sf.jour_j <- fr_grid.hex_stats.sf.jour_j %>% 
    mutate(diff_nb = nb_2020 - nb_2019) %>%
    mutate(ratio_nb = nb_2020 / nb_2019,
           taux_evol = ((nb_2020-nb_2019) / nb_2019)*100,
           ratio_nb_0 = ratio_nb - 1) %>%
    mutate(diff_nb_abs = abs(diff_nb)) %>%
    mutate(diff_nb_signe = case_when(diff_nb>0 ~ "+",diff_nb<0 ~ "-", TRUE ~ "="))
  
  fr_grid.hex_stats.fr.jour_j <- st_intersection(fr_grid.hex_stats.sf.jour_j,
                                                 DEP_FRMETDOM_2016 %>% summarise()) 
  
  
  # centroides xy sf
  fr_grid.hex_stats.xy.jour_j <- fr_grid.hex_stats.fr.jour_j %>%
    st_centroid(.,of_largest_polygon = T)
  
  fr_grid.hex_stats.xy.ctr.jour_j <- fr_grid.hex_stats.xy.jour_j  %>%
    mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
    st_drop_geometry() %>%
    as.data.frame() 
  
  
  # k fonction de max de diff_nb_abs
  k_val <- fr_grid.hex_stats.xy.jour_j %>% st_drop_geometry() %>% select(diff_nb_abs) %>% pull() %>% as.vector() %>% max() / 450
  
  # cartogramme de dorling
  fr_grid.hex_stats.xy.dor.jour_j <- cartogram_dorling(fr_grid.hex_stats.xy.jour_j ,
                                                       k = k_val,
                                                       itermax = 100,
                                                       weight = "diff_nb_abs")
  
  fr_grid.hex_stats.xy.dor.ctr.jour_j <- fr_grid.hex_stats.xy.dor.jour_j  %>%
    mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
           y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    st_drop_geometry() %>%
    as.data.frame()
  
  
  ### carto
  
  ggplot() +
    geom_sf(data = DEP_FRMETDOM_2016, 
            fill="grey92", color = "grey80", lwd = 0.3 ) +
    geom_point(data = fr_grid.hex_stats.xy.dor.ctr.jour_j  %>%
                 mutate(taux_evol.cl = cut(taux_evol, c(-101,-50,0,50,100,200,300,Inf))) %>%
                 mutate(taux_evol.cl = recode_factor(taux_evol.cl, '(-101,-50]' = '< -50%', 
                                                     '(-50,0]' = "-50% / 0%",
                                                     '(0,50]' = "0% / +50%",
                                                     '(50,100]' = "+50% / +100%", 
                                                     '(100,200]' = "+100% / +200%", 
                                                     '(200,300]' = "+200% / +300%",
                                                     '(300,Inf]' = "> +300%")) %>%
                 mutate(taux_evol.cl = factor(taux_evol.cl, levels=c('< -50%', "-50% / 0%","0% / +50%","+50% / +100%","+100% / +200%","+200% / +300%","> +300%"))) ,
               aes(x= x_ctr, y = y_ctr,
                   size = diff_nb_abs,
                   fill = taux_evol.cl),
               color = "white",
               stroke = 0.3,
               shape = 21,
               show.legend = TRUE) +
    scale_alpha_continuous(range = c(0.6,1),
                           guide=FALSE) +
    scale_fill_manual( name = "Différence en\npourcentage de décès",
                       values = c("#72aed2", 
                                  "#b8d6e8",
                                  "#fcbba1",
                                  "#fc9272",
                                  "#fb6a4a",
                                  "#de2d26",
                                  "#a50f15")) +
    # legende hommes / femmes
    annotate("text",
             x = 220000,
             y = 7050000,
             label = paste0(day(as.Date(jour)) , " ",months(as.Date(jour))) ,
             color="black",
             size=6 , angle=0, fontface="bold") +
    scale_size_continuous(#trans = "sqrt", 
      name ="Différence en\nnombre de décès",
      breaks = c(10,50,100),
      limits = c(0,180),
      labels = function(x) format(x, big.mark = " ",scientific = FALSE),
      range = c(0.1,18)) +
    scale_x_continuous(expand = c(0,0), name = "") +
    scale_y_continuous(expand = c(0,0), name = "") +
    theme_ipsum() +
    theme(axis.text = element_blank(), axis.title  = element_blank(), axis.ticks  = element_blank()) +
    labs(
      title = "Différentiel de décès pendant la pandémie Covid-19",
      subtitle = "Par jour, nombre de décès en 2020 versus les années précédentes (moyenne sur 2018 et 2019).
    Localisation par commune de décès, agrégation par carreau de 30 km de côté, lissage sur une semaine glissante",
      caption = "Sources : Insee / Fichier individuel comportant des informations sur chaque décès / 24/04/2020"
    ) +
    theme(legend.position = c(1,0.65),
          legend.background = element_rect(fill = "grey92", color = "grey85", size = 0.2)) +
    coord_sf(crs = st_crs(2154), datum = NA)
  
  # export png
  ggsave(
    filename = paste0("./img_anim/carto_grid_jour_dor_",jour,".png"),
    device = 'png', 
    width = 8.5,height = 8.5)
  
}

# sortie pour tous lesj ours
seq(as.Date("2020-03-03"),as.Date("2020-04-10"), by = 1) %>% map(f_out_img_dor)

# création du gif

list.files(path = "./img_anim", 
           pattern = "carto_grid_jour_dor_", full.names = TRUE) %>%
  sort() %>% 
  purrr::map(image_read) %>% 
  image_join() %>% 
  image_scale("x850") %>%
  image_animate(fps=4, loop = 1) %>%
  image_write("./gif/carto_grid_jour_dor_v4.gif") 
