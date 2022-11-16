rm(list=ls())
#remotes::install_github("antuki/CARTElette/CARTElette@RPackage")
#install.packages("magick")
#install.packages("rsvg")
#remotes::install_github('coolbutuseless/svgparser')
library(svgparser)
library(CARTElette)
library(sf)
library(dplyr)
library(magick)
library(ggplot2)
#remotes::install_github("crazycapivara/h3-r")
library(h3)
library(R.utils)
#install.packages("btb")
library(btb)

###### couches carto
dep <- charger_carte(COG=2021,nivsupra="DEP") %>% filter(substr(DEP,1,2)!="97")
fr_metro <- st_union(dep)
epci <- charger_carte(COG=2021,nivsupra="EPCI", geometrie_simplifiee = TRUE)
epci <- epci[do.call(c,lapply(st_intersects(epci,fr_metro),function(x){ifelse(length(x)>0,TRUE,FALSE)})),]
bv <- charger_carte(COG=2021,nivsupra="BV2012", geometrie_simplifiee = TRUE)
bv <- bv[do.call(c,lapply(st_intersects(bv,fr_metro),function(x){ifelse(length(x)>0,TRUE,FALSE)})),]
cv <- charger_carte(COG=2021,nivsupra="CV", geometrie_simplifiee = TRUE)
cv <- cv[do.call(c,lapply(st_intersects(cv,fr_metro),function(x){ifelse(length(x)>0,TRUE,FALSE)})),]



# 10 November 2022 "A Bad Map"
bbox <- st_bbox(fr_metro)
bulle <- image_read('https://www.pngmart.com/files/17/Think-Bubble-PNG-Transparent-Image.png')
ronds <- st_as_sf(st_sample(fr_metro, size=100)) %>%  mutate(size=sample(0:10000,100, replace=TRUE))

fig <- image_graph(width = 800, height = 800, res = 96)
ggplot()+ geom_sf(data=fr_metro) + geom_sf(data=ronds, pch=21, aes(size=size, fill=size),col="pink") +
  scale_size(range = c(18, 20))+  theme_minimal() + theme(legend.position = "none")+
  xlim(c(bbox$xmin,2*bbox$xmax))

out <-image_composite(fig, image_scale(bulle, "x400"), offset = "+400+0")
print(out)
img <- image_draw(bulle)
dev.off()

# 11 November 2022 "Colour Friday : Red"
tiger_filename <- "mouth.svg"
tiger_grob <- svgparser::read_svg(tiger_filename)
tiger_df <- svgparser::read_svg(tiger_filename, obj_type = 'data.frame')

toto <- tiger_df %>% filter(name=="bezier3") %>% select(x,y) %>% as.matrix()
toto <- rbind(toto, toto[1,])
toto <- st_sfc(st_polygon(list(toto)),crs=4326)
toto <- st_as_sf(toto)
sf_use_s2(FALSE)
ecart = st_coordinates(st_centroid(fr_metro)) - st_coordinates(st_centroid(toto))
toto <- CARTElette:::transformation_shp(toto,rot=180, scale=0.016, shift=ecart)

epci_bouche <- epci[do.call(c,lapply(st_intersects(epci,toto), function(x){ifelse(length(x)>0,TRUE,FALSE)})),]

ggplot() + geom_sf(data=epci, col="lightgrey") + geom_sf(data=epci_bouche, fill="red", col="pink")  + theme_void() + ggtitle("France métropolitaine des intercommunalités")


# 17 November 2022 "A map without a computer"
tiger_filename <- "computer.svg"
tiger_grob <- svgparser::read_svg(tiger_filename)
tiger_df <- svgparser::read_svg(tiger_filename, obj_type = 'data.frame')

r <- as.numeric(c(1,row.names(tiger_df[which(tiger_df$name=="close_path"),])))
toto1 <- st_as_sf(st_sfc(st_polygon(list(tiger_df %>% slice(r[1]:r[2])  %>% select(x,y) %>% as.matrix() )),crs=4326))
toto2 <- st_as_sf(st_sfc(st_polygon(list(tiger_df %>% slice((r[2]+1):r[3])  %>% select(x,y) %>% as.matrix() )),crs=4326))
toto <- st_difference(toto1,toto2)
ecart = st_coordinates(st_centroid(fr_metro)) - st_coordinates(st_centroid(st_union(toto)))
toto <- CARTElette:::transformation_shp(toto,rot=0, scale=0.0005, shift=ecart+c(0,-1))
cv_ordi <- cv[do.call(c,lapply(st_intersects(cv,toto), function(x){ifelse(length(x)>0,TRUE,FALSE)})),]
cv_ordi <- st_union(cv_ordi)
ggplot() + geom_sf(data=cv, col="lightgrey") + geom_sf(data=cv_ordi, fill="white", col="darkgrey")  + theme_void() + ggtitle("France métropolitaine des cantons-villes")


# 18 November 2022 "Colour Friday: Blue 	Map containing blue colour."

tiger_filename <- "chaussure.svg"
tiger_grob <- svgparser::read_svg(tiger_filename)
tiger_df <- svgparser::read_svg(tiger_filename, obj_type = 'data.frame')

r <- as.numeric(c(1,row.names(tiger_df[which(tiger_df$name=="close_path"),])))
toto1 <- st_as_sf(st_sfc(st_polygon(list(tiger_df %>% slice(r[1]:r[2])  %>% select(x,y) %>% as.matrix() )),crs=4326))
toto2 <- st_as_sf(st_sfc(st_polygon(list(tiger_df %>% slice((r[2]+1):r[3])  %>% select(x,y) %>% as.matrix() )),crs=4326))
toto3 <- st_as_sf(st_sfc(st_polygon(list(tiger_df %>% slice((r[3]+1):r[4])  %>% select(x,y) %>% as.matrix() )),crs=4326))
toto <- rbind(toto1, toto3)
sf_use_s2(FALSE)
ecart = st_coordinates(st_centroid(fr_metro)) - st_coordinates(st_centroid(st_union(toto)))
toto <- CARTElette:::transformation_shp(toto,rot=180, scale=0.012, shift=ecart+c(1,0))

bv_chaussure <- bv[do.call(c,lapply(st_intersects(bv,toto), function(x){ifelse(length(x)>0,TRUE,FALSE)})),]

ggplot() + geom_sf(data=bv, col="lightgrey") + geom_sf(data=bv_chaussure, fill="blue", col="lightblue")  + theme_void() + ggtitle("France métropolitaine des bassins de vie")


# Mariages

options(timeout = max(800, getOption("timeout")))
url <- "https://www.insee.fr/fr/statistiques/fichier/5347431/etatcivil2019_mar2019_csv.zip"
tmp <- "etatcivil2019_mar2019_csv.zip"
download.file(url,tmp)
unzip(tmp)

libelles <- read.csv("varmod_MAR_2019.csv", sep=";")
donnees <- read.csv("FD_MAR_2019.csv", sep=";", colClasses=c("MMAR"="character"))

dep_idf <- c("75","92","93","94")
DEP_sf_idf <- dep %>% filter(DEP%in%dep_idf) %>% CARTElette:::transformation_shp(scale=4,shift = c(3,2))
DEP_sf <- rbind(dep %>% filter(!DEP%in%dep_idf), DEP_sf_idf)
DEP_sf <- cbind(DEP_sf, st_coordinates(st_centroid(DEP_sf)))

seuil=0.1
analyse_mois <- donnees %>% select(DEPMAR, MMAR) %>%
  mutate(SMAR= case_when(
    MMAR%in%c("01", "02","03", "04","05","10","11","12") ~ "Rest of the year",
    MMAR%in%c("06","07","08","09") ~ "June to September",
  )) %>% 
  group_by(DEPMAR) %>% 
  count(SMAR) %>% 
  mutate(freq = 100*n / sum(n)) %>% 
  #left_join(libelles %>% filter(COD_VAR=="MMAR") %>% select(COD_MOD, LIB_MOD), by=c("MMAR"="COD_MOD")) %>% 
  select(DEPMAR, SMAR, freq) %>% 
  right_join(DEP_sf %>% st_drop_geometry() %>%  select(-nom), by=c("DEPMAR"="DEP")) %>% 
  mutate(X_dec= case_when(
    SMAR=="June to September" ~ -0.12,
    SMAR=="Rest of the year" ~ -0.12+seuil
  )) 

analyse_jour <- donnees %>% select(DEPMAR, JSEMAINE) %>%
  mutate(JMAR= case_when(
    JSEMAINE%in%c(1:5) ~ "Rest of the week",
    JSEMAINE%in%c(6:7) ~ "Week-end",
  )) %>% 
  group_by(DEPMAR) %>% 
  count(JMAR) %>% 
  mutate(freq = 100*n / sum(n)) %>% 
  select(DEPMAR, JMAR, freq) %>% 
  right_join(DEP_sf %>% st_drop_geometry() %>%  select(-nom), by=c("DEPMAR"="DEP")) %>% 
  mutate(X_dec= case_when(
    JMAR=="Week-end" ~ 0.12,
    JMAR=="Rest of the week" ~ 0.12+seuil
    
  )) 

DEP_sf <- analyse_mois %>% 
  group_by(DEPMAR) %>% 
  mutate(typo1 = SMAR[freq==max(freq)]) %>% 
  mutate(typo1 = ifelse(typo1=="June to September",NA,"Majority of weddings\nduring the rest of the year")) %>% 
  select(DEPMAR,typo1) %>% 
  distinct(DEPMAR, .keep_all = TRUE) %>% 
  right_join(DEP_sf, c("DEPMAR"="DEP")) %>% st_as_sf()
DEP_sf <- analyse_jour %>% 
  group_by(DEPMAR) %>% 
  mutate(typo2 = JMAR[freq==max(freq)]) %>% 
  mutate(typo2 = ifelse(typo2=="Week-end",NA,"Majority of weddings\nduring the rest of the week")) %>% 
  select(DEPMAR,typo2) %>% 
  distinct(DEPMAR, .keep_all = TRUE) %>% 
  right_join(DEP_sf, c("DEPMAR"="DEPMAR")) %>% st_as_sf()
DEP_sf <- DEP_sf %>% ungroup() %>%  mutate(typo3 = 
                                                                    case_when(
  typo1=="Majority of weddings\nduring the rest of the year" & typo2=="Majority of weddings\nduring the rest of the week" ~ "Majority of weddings\nduring the rest of the week AND year",
  typo1=="Majority of weddings\nduring the rest of the year" & is.na(typo2)~ "Majority of weddings\nduring the rest of the year only",
  typo2=="Majority of weddings\nduring the rest of the week" & is.na(typo1)~ "Majority of weddings\nduring the rest of the week only"
))

p <- ggplot() +
  geom_sf(data=DEP_sf, aes(fill=factor(typo3))) +
  geom_linerange(data = analyse_mois, 
                aes(x = X+X_dec,ymin=Y,ymax=Y+freq/140, color=SMAR), size=1) +
  geom_linerange(data = analyse_jour, 
                 aes(x = X+X_dec,ymin=Y,ymax=Y+freq/140, color=JMAR), size=1) +
  labs(title="Wedding seasons in French départements",
      color = "Percentages of weddings\naccording time of year or week",
       fill=NULL,
      caption = "Source: Insee, statistiques de l'état civil 2019. by @antuki13") +
  scale_fill_manual(values=c("lightgrey","darkgrey"),na.translate = FALSE,
                    breaks=c("Majority of weddings\nduring the rest of the year only","Majority of weddings\nduring the rest of the week AND year")
                    ) +
  scale_color_manual(values=c("#2171B5", "#CB181D","#6BAED6","#FB6A4A"),
                      
                     breaks=c( "June to September","Rest of the year","Week-end","Rest of the week")
                     ) +
  theme_void() +
  guides(fill = guide_legend(order = 2),
         color = guide_legend(order = 1))+ 
  theme(plot.title = element_text(hjust = 0.5))  
p


# Kontur

options(timeout = max(800, getOption("timeout")))
url <- "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_20220630.gpkg.gz"
tmp <- "kontur_population_20220630.gpkg.gz"
download.file(url,tmp)
R.utils::gunzip(tmp)
indices <- k_ring(geo_to_h3(c(49.6578844,0.7337508), 8), 100)
hn <- DEP_sf  %>% filter(DEPMAR=="76") %>% st_transform(3857) 
kontur <- st_read("kontur_population_20220630.gpkg", query = paste0("SELECT * from population WHERE h3 in (", toString(sprintf("'%s'", indices)),")"))
t <- st_intersects(kontur,hn)
kontur <- kontur[do.call(c,lapply(t,function(x){length(x)>0})),]
classes <- c(0,classInt::classIntervals(kontur$population, 8,style="quantile")$brks)
kontur$population2 <- cut(kontur$population,classes)
p <- ggplot()+
  geom_sf(data=kontur,aes(fill=population2), color=NA,show.legend=TRUE)+
  # scale_fill_stepsn(  colours=RColorBrewer::brewer.pal(5, "YlOrRd"),
  #                     breaks=classes[-1],values = scales::rescale(classes[-1]),
  #                     labels=scales::label_number(accuracy=1),
  #                     name="Population") +
  scale_fill_manual(values=RColorBrewer::brewer.pal(9, "YlOrRd"))+
  geom_sf(data=DEP_sf, fill=NA)+
  labs(title="Kontur population in Seine-Maritime, France",
       subtitle="not smoothed",
       caption="Kontur Population Dataset. Smoothing by @antuki13 with btb R Package")+
  xlim(c(7300.911,199348.295)) + ylim(c(6317500.502,6458789.652))+
  theme_void()


cellule = 1200
#kontur_gridded <- btb::btb_ptsToGrid(btb_add_centroids(st_centroid(kontur) %>% select(population),iCellSize = cellule) %>% st_drop_geometry,iCellSize = cellule)
#plot(st_geometry(kontur_gridded))
kontur_smoothed <- btb::btb_smooth(st_centroid(kontur) %>% select(population),
                                   sEPSG=3857, iCellSize = 1/2*cellule, iBandwidth = 3*cellule)
kontur_smoothed$population2 <- cut(kontur_smoothed$population,classes)

q <- ggplot()+
  geom_sf(data=kontur_smoothed,aes(fill=population2), color=NA)+
  scale_fill_manual(values=RColorBrewer::brewer.pal(9, "YlOrRd"), name="Kontur population")+
  geom_sf(data=DEP_sf, fill=NA)+
  labs(subtitle="smoothed (3.6 km)")+
  xlim(c(7300.911,199348.295)) + ylim(c(6317500.502,6458789.652))+
  theme_void()

require(gridExtra)
grid.arrange(p, q, ncol=2)


### 5 minutes map 

library(tictoc)
tic()
remotes::install_github(
  "antuki/CARTElette/CARTElette@RPackage")
toc()
# installation : 38.901 sec
library(CARTElette)
?CARTElette:charger_carte
# reading documentation : 260 sec
DEP_sf <- charger_carte(COG=2021, nivsupra = "DEP") 
# downloading map layer : 5.816 sec elapsed
300-260-38.901+5.816
#[1] 5.816 seconds remaining
#plot(st_geometry(DEP_
# Game over !!!!!!!!!




