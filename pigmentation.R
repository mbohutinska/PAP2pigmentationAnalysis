setwd("/home/aa/pigmentation/GWAScolor/")

# === 1. LIBRARIES ===
library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(ggplot2)
library(scatterpie)

# === 2. LOAD DATA ===
df4 <- read.csv("iNaturalistColors.csv", header = TRUE)
df5 <- read.delim("popInfo19JunFinalColor.txt", header = TRUE)
df3 <- read.csv("finalDF.csv")

df5 <- subset(df5, !df5$color %in% ";;")

# === 3. DEFINE BBOX ===
bbox <- c(
  xmin = min(df3$longit) - 1.5,
  xmax = max(df3$longit) + 1.5,
  ymin = min(df3$latit) - 1.5,
  ymax = max(df3$latit) + 1.5
)
bbox_sf <- st_as_sf(data.frame(
  lon = c(bbox["xmin"], bbox["xmax"]),
  lat = c(bbox["ymin"], bbox["ymax"])
), coords = c("lon", "lat"), crs = 4326)

# === 4. GET ELEVATION DATA ===
elev_raster <- get_elev_raster(locations = bbox_sf, z = 6, src = "aws")
elev_raster_cropped <- crop(elev_raster, raster::extent(bbox))
elev_df <- as.data.frame(rasterToPoints(elev_raster_cropped))
colnames(elev_df)[3] <- "elevation"
elev_df$elevation <- as.numeric(elev_df$elevation)

# === 5. FORMAT PIE CHART DATA ===
df5_pie <- df5 %>%
  separate(color, into = c("white", "plum", "darkorchid"), sep = ";", convert = TRUE) %>%
  mutate(radius = 0.1) %>%
  dplyr::select(lon, lat, white, plum, darkorchid, radius)

# === 6. PLOT ===
ggplot() +
  # Elevation background
  geom_tile(data = elev_df, aes(x = x, y = y, color = elevation), alpha = 0.85) +
  scale_color_gradientn(colors = gray.colors(100, start = 0.9, end = 0), name = "Elevation") +
  
  # Points from df4
  geom_point(data = df4, aes(x = lon, y = lat, fill = color),
             shape = 21, size = 4.5, color = scales::alpha("white", 0.01), stroke = 0.0001, alpha = 0.8) +
  scale_fill_manual(values = c("white" = "white", "plum" = "plum", "darkorchid" = "darkorchid"),
                    name = "iNaturalist color") +
  
  # Pie charts (on top)
  geom_scatterpie(data = df5_pie, aes(x = lon, y = lat, r = 0.1),
                  cols = c("white", "plum", "darkorchid"),
                  color = NA, alpha = 0.85) +
  
  coord_equal(xlim = c(bbox["xmin"], bbox["xmax"]),
              ylim = c(bbox["ymin"], bbox["ymax"]),
              expand = FALSE) +
  theme_minimal() +
  labs(title = "Pigmentation distribution and elevation",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right")

# === 7. SAVE PLOT ===
ggsave("Pigmentation_Elevation_Map.pdf", width = 10, height = 9)

### BACKGROUND separate
elev_df$logelevation<-log(as.numeric(elev_df$elevation+100))
ggplot() +
  geom_raster(data = elev_df, aes(x = x, y = y, fill = logelevation)) +
  scale_fill_gradientn(colors = gray.colors(100, start = 1, end = 0), name = "Elevation") +
  coord_equal(xlim = c(bbox["xmin"], bbox["xmax"]),
              ylim = c(bbox["ymin"], bbox["ymax"]),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

xrange <- bbox["xmax"] - bbox["xmin"]
yrange <- bbox["ymax"] - bbox["ymin"]

# zvolíme jednotkové dpi = 100 px / 1° pro rozumný rozměr
width_inch <- as.numeric(c(xrange))  # podle xrange
height_inch <- as.numeric(c(yrange))   # podle yrange

ggsave("Layer_Elevation_Background_Vggplot2.2.png",
       width = width_inch,
       height = height_inch,
       dpi = 300,
       bg = "transparent")

ggsave("Layer_Elevation_Background_Vggplot2.2.pdf", width = width_inch, height = height_inch)


### POINTS/Pie charts
ggplot() +
  # Points from df4, with no visible outline
  geom_point(data = df4, aes(x = lon, y = lat, fill = color),
             shape = 21, size = 6.5, color = "white", stroke = 0, alpha = 0.8) +
  scale_fill_manual(values = c("white" = "white", "plum" = "plum", "darkorchid" = "darkorchid"),
                    name = "iNaturalist color") +
  
  # Pie charts
  geom_scatterpie(data = df5_pie, aes(x = lon, y = lat, r = 0.1),
                  cols = c("white", "plum", "darkorchid"),
                  color = NA, alpha = 0.85) +
  
  coord_equal(xlim = c(bbox["xmin"], bbox["xmax"]),
              ylim = c(bbox["ymin"], bbox["ymax"]),
              expand = FALSE) +
  theme_void() +
  theme(legend.position = "none") 
ggsave("Layer_Points_PieCharts.pdf", width = width_inch, height = height_inch)






setwd("/home/aa/pigmentation/GWAScolor/")

# === 1. KNIHOVNY ===
library(tidyverse)
library(sf)
library(elevatr)
library(raster)
library(ggplot2)
library(scatterpie)

# === 2. NAČTI DATA ===
df4 <- read.csv("iNaturalistColors.csv", header = TRUE)
df5 <- read.delim("popInfo19JunFinalColor.txt", header = TRUE)
df3 <- read.csv("finalDF.csv")

df5 <- subset(df5, !df5$color %in% ";;")

# --- 1. Sloučení všech souřadnic ---
coords_all <- rbind(
  df4 %>% dplyr::select(lon, lat),
  df5 %>% dplyr::select(lon, lat)
)

# --- 2. Bounding box s malou rezervou (0.5° okraj) ---
bbox <- c(
  xmin = min(df3$longit) - 1,
  xmax = max(df3$longit) + 1,
  ymin = min(df3$latit) - 1,
  ymax = max(df3$latit) + 1
)

# --- 3. Převod bbox na sf objekt ---
bbox_sf <- st_as_sf(data.frame(
  lon = c(bbox["xmin"], bbox["xmax"]),
  lat = c(bbox["ymin"], bbox["ymax"])
), coords = c("lon", "lat"), crs = 4326)

# --- 4. Stáhni raster z AWS ---
elev_raster <- get_elev_raster(locations = bbox_sf, z = 6, src = "aws")

# --- 5. Ořízni raster na přesný rozsah ---
elev_raster_cropped <- crop(elev_raster, raster::extent(bbox))

# --- 6. Převod na data frame ---
elev_df <- as.data.frame(rasterToPoints(elev_raster_cropped))
colnames(elev_df)[3] <- "elevation"
elev_df$elevation <- as.numeric(elev_df$elevation)  # pro jistotu


# === 5. ROZPARSOVÁNÍ PIE DAT ===
df5_pie <- df5 %>%
  separate(color, into = c("white", "plum", "darkorchid"), sep = ";", convert = TRUE) %>%
  mutate(radius = 0.3) %>%
  dplyr::select(lon, lat, white, plum, darkorchid, radius)

# === 6. PLOT ===
ggplot() +
  # Podklad s altitudou
  geom_tile(data = elev_df, aes(x = x, y = y, color = elevation), alpha = 0.85) +
  scale_color_gradientn(colors = gray.colors(100, start = 1, end = 0), name = "Elevation") +
  
  # Pie charty (df5)
  geom_scatterpie(data = df5_pie, aes(x = lon, y = lat, r = radius),
                  cols = c("white", "plum", "darkorchid"),
                  color = NA) +
  
  # Body z df4 jako tečky, použijeme aes(color = ...)
  geom_point(data = df4, aes(x = lon, y = lat, fill = color),
             shape = 21, size = 2.5, color = "black", stroke = 0.5, alpha = 0.8) +
  scale_fill_manual(values = c("white" = "white", "plum" = "plum", "darkorchid" = "darkorchid"),
                    name = "iNaturalist color") +
  
  # Koordináty a vzhled
  coord_equal(xlim = c(bbox["xmin"], bbox["xmax"]),
              ylim = c(bbox["ymin"], bbox["ymax"]),
              expand = FALSE) +
  theme_minimal() +
  labs(title = "Pigmentation distribution and elevation",
       x = "Longitude", y = "Latitude") +
  theme(legend.position = "right")


# === 7. ULOŽ PDF ===
ggsave("Pigmentation_Elevation_Map.pdf", width = 10, height = 9)





library(tidyverse)
library(ggplot2)
library(sf)
library(elevatr)
library(raster)
library(RColorBrewer)
library(scatterpie)

# === 1. NAČTI DATA ===
df4 <- read.csv("iNaturalistColors.csv", header = TRUE)
df5 <- read.delim("popInfo19JunFinalColor.txt", header = TRUE)

# Vyčisti df5 (odstraň prázdné barvy)
df5 <- subset(df5, !df5$color %in% ";;")

# === 2. VYTVOŘENÍ SOUBORNÉHO DATAFRAMEU PRO PIE ===
# Rozparsuj složený color zápis
df5_pie <- df5 %>%
  separate(color, into = c("white", "plum", "darkorchid"), sep = ";", convert = TRUE) %>%
  mutate(radius = 0.3) %>%  # jednotná velikost všech výsečí
  dplyr::select(lon, lat, white, plum, darkorchid, radius)

# === 3. ELEVACE ===
all_coords <- rbind(df4 %>% dplyr::select(lat = datetime, lon = id) %>% mutate(across(everything(), as.numeric)),
                    df5 %>% dplyr::select(lat, lon))

coords_sf <- st_as_sf(all_coords, coords = c("lon", "lat"), crs = 4326)
bbox <- st_bbox(coords_sf)

elev_raster <- get_elev_raster(locations = coords_sf, z = 6, src = "aws")  # vhodná úroveň detailu
elev_df <- as.data.frame(rasterToPoints(elev_raster))
colnames(elev_df)[3] <- "elevation"

# === 4. VYKRESLENÍ MAPY S PIE CHARTY ===
ggplot() +
  # Altituda podklad
  geom_tile(data = elev_df, aes(x = x, y = y, fill = elevation)) +
  scale_fill_gradientn(colors = gray.colors(100, start = 1, end = 0), name = "Elevation") +
  
  # Pie charty
  geom_scatterpie(data = df5_pie,
                  aes(x = lon, y = lat, r = radius),
                  cols = c("white", "plum", "darkorchid"),
                  color = NA) +  # žádný okraj
  
  # Legenda a vzhled
  coord_equal(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax), expand = FALSE) +
  theme_minimal() +
  theme(legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", title = "Pigmentation in Arabidopsis arenosa")

# === 5. EXPORT PDF ===
ggsave("Pigmentation_Map.pdf", width = 10, height = 9)



### PIGMENTATION
### Pie chart

library(data.table)
library(dplyr)
library(data.table)
library(dplyr)
library(ggmap)
library(argosfilter)
library(mapplots)
library(RgoogleMaps)
library(RCurl) 
#name="ASY1"
contr<-c("white","pink","purple")
data_plants<-read.csv("/home/aa/BetyProject/pigmentation/GWAScolor/petalColorFromNatureClean.csv",h=T)
geogr<-read.csv("/home/aa/BetyProject/pigmentation/forStudents/Arabidopsis_sampling_2011-2023 - AA (1).csv")
popInfo<-fread("file:///home/aa/BetyProject/envAssoc/Data/popInfo.txt", h=T)

geogr1<-subset(geogr,substr(geogr$code,1,3) %in% popInfo$pop)

#popInfo<-subset(popInfo,popInfo$Altitude > 400)
#popInfo1<-subset(popInfo,!popInfo$pop %in% substr(landscape$newname2,1,3))
#write.table(landscape1,"/home/aa/BetyProject/claraMS/genomicPops.txt",quote = F,row.names = F,sep = "\t")
#table(landscape1$pop)

aaa<-subset(popInfo, popInfo$AA_code %in% pops$Var1)
aaaa<-subset(popInfo, !popInfo$AA_code %in% pops$Var1)


# pl <- "AA065"
file.remove("colorsNV.txt")
pops<-as.data.frame(table(substr(data_plants$pop,1,5)))
for (pl in pops$Var1) {
setwd("/home/aa/BetyProject/pigmentation/forStudents/")
df <- data_plants[substr(data_plants$pop, 1, 5) == pl,]
nrow(df)
#colSums(df[c("petalColour")] == 0)
## percentage of white petals
w<-colSums(df[c("petalColour")] == 0)*100/nrow(df)
## percentage of pinkish petals
pi<-colSums(df[c("petalColour")] == 1)*100/nrow(df)
## percentage of purple petals
pu<-colSums(df[c("petalColour")] == 2)*100/nrow(df)
write.table(t(as.data.frame(c(pl,w,pi,pu))),"colorsNV.txt",append = T,quote = F,sep = "\t",col.names = F,row.names = F)
print(c(pl,w,pi,pu))
}
data_plants<-read.table("colorsNV.txt",h=F)
colnames(data_plants)<-c("pops","w","pi","pu")
geogr$tr<-substr(geogr$tr,1,5)
geogr1<-subset(geogr, geogr$tr %in% data_plants$pops)

geogr1<-geogr1[ order(geogr1[,"tr"]), ]
data_plants<-data_plants[ order(data_plants[,1]), ]
geogr1$tr
data_plants$pops
finalDF<-cbind(data_plants,geogr1)
lat <- c(min(as.numeric(finalDF$latit))-1,max(as.numeric(finalDF$latit))+1) #define our map's ylim
lon <- c(min(as.numeric(finalDF$longit))-1,max(as.numeric(finalDF$longit))+1) #define our map's xlim
write.csv(finalDF,"finalDF.csv")

finalDF$latit<-as.numeric(finalDF$latit)
finalDF$longit<-as.numeric(finalDF$longit)

############################ Hannes
library("readxl")
df1 <- read_excel("finalDF1 (1).xlsx")
df3 <- read.csv("finalDF.csv")
df3$colorText<-""
df3$offsetx<-0
df3$offsety<-0
df3$pos<-4
df3$pop<-paste0(substr(df3$ploidy,1,2),substr(df3$tr,3,5))
### adding colors to high altitude and lowland plants (above 1000m?m and below)
for (i in 1:nrow(df3)) { # i=1
  if (df3$altit[i]>1400) {
    df3$colorText[i]<-"purple"
  } else{df3$colorText[i]<-"black"}
  df2<-subset(df1,df1$Code %in% df3$tr[i])
  if (nrow(df2)>0) {
  df3$offsetx[i]<-df2$offsetx
  df3$offsety[i]<-df2$offsety
  df3$pos[i]<-df2$pos
  } else{}  }

colors<-c("white", "plum", "darkorchid")
df4<-read.csv("file:///home/aa/pigmentation/forStudents/iNaturalistColors.csv",h=T)


latR=c(min(df3$latit)-1,max(df3$latit)+1)
lonR=c(min(df3$longit)-1,max(df3$longit)+1)

terrmap<-GetMap.bbox(latR=c(min(finalDF$latit)-1,max(finalDF$latit)+1),lonR=c(min(finalDF$longit)-1,max(finalDF$longit)+1), maptype = "terrain", destfile = "terrain1.png",path = "&style=feature:all|element:labels|visibility:off", MINIMUMSIZE = T,GRAYSCALE=T)
size<-terrmap$size
pdf(file=paste("NVpigmentation1.pdf", sep=""),width = size[1]/72, height = size[2]/72,pointsize = 15)
#terrmap<-GetMap.bbox(latR=c(min(cor_Ceur$lat)-1,max(cor_Ceur$lat)+1),lonR=c(min(cor_Ceur$lon)-1,max(cor_Ceur$lon)+1), maptype = "terrain", destfile = "terrain1.png",path = "&style=feature:all|element:labels|visibility:off", MINIMUMSIZE = T,GRAYSCALE=T)
size<-terrmap$size
PlotOnStaticMap(terrmap)


for (i in 1:nrow(df4)){ # i=1
  d1<-distance(df4$lat[i], df4$lat[i], lon[1], lon[2]) 
  d2<-distance(df4$lat[i], df4$lat[i], lon[1], df4$lon[i])
  a1<-size[1]/d1
  a2<-d2*a1
  a3<- -(size[1]/2)+a2
  b1<-size[2]/(lat[2]-lat[1])
  b2<-(df4$lat[i]-lat[1])*b1
  b3<- -(size[2]/2)+b2
  terrmap<-points(x=a3,y=b3, pch=20, cex=1.8, col=df4$color[i])}

for (i in 1:nrow(df3)){ # i=3
  d1<-distance(df3$latit[i], df3$latit[i], lon[1], lon[2]) 
  d2<-distance(df3$latit[i], df3$latit[i], lon[1], df3$longit[i])
  a1<-size[1]/d1
  a2<-d2*a1
  a3<- -(size[1]/2)+a2+df3$offsetx[i]
  b1<-size[2]/(lat[2]-lat[1])
  b2<-(df3$latit[i]-lat[1])*b1
  b3<- -(size[2]/2)+b2+df3$offsety[i]
  terrmap<-add.pie(z=c(df3$w[i],df3$pi[i],df3$pu[i]), x=a3, y=b3, radius=8, col = colors, labels=NA)
  terrmap<-text(x=a3, y=b3, labels = df3$pop[i], pos = df3$pos[i], col = df3$colorText[i], offset = 0.45,cex = 0.55)
}

dev.off()






################ 6 Aug 2023
############################ Hannes
library(data.table)
library(dplyr)
library(data.table)
library(dplyr)
library(ggmap)
library(argosfilter)
library(mapplots)
library(RgoogleMaps)
library(RCurl) 

library("readxl")
setwd("/home/aa/pigmentation/forStudents/")
df1 <- read_excel("finalDF1 (1).xlsx")
df3 <- read.csv("finalDF.csv")
#df3 <- read.csv("finalDF.csv")
df3$colorText<-""
df3$offsetx<-0
df3$offsety<-0
df3$pos<-4
df3$pop<-paste0(substr(df3$ploidy,1,2),substr(df3$tr,3,5))
### adding colors to high altitude and lowland plants (above 1000m?m and below)
for (i in 1:nrow(df3)) { # i=1
  if (df3$altit[i]>1400) {
    df3$colorText[i]<-"purple"
  } else{df3$colorText[i]<-"black"}
  df2<-subset(df1,df1$Code %in% df3$tr[i])
  if (nrow(df2)>0) {
    df3$offsetx[i]<-df2$offsetx/2
    df3$offsety[i]<-df2$offsety/2
    df3$pos[i]<-df2$pos
  } else{}  }


############################ ASI TADY ZACIT?
colors<-c("white", "plum", "darkorchid")
df4<-read.csv("file:///home/aa/pigmentation/forStudents/iNaturalistColors.csv",h=T)
df5<-fread("file:///home/aa/pigmentation/GWAScolor/popInfo19JunFinalColor.txt",h=T)
df5<-subset(df5, !df5$color %in% ";;")
df5$offsetx<-0
df5$offsety<-0
df5$pos<-4

terrmap<-GetMap.bbox(latR=c(min(df3$latit)-1,max(df3$latit)+1),lonR=c(min(df3$longit)-1,max(df3$longit)+1), maptype = "terrain", destfile = "terrain1.png",path = "&style=feature:all|element:labels|visibility:off", MINIMUMSIZE = T,GRAYSCALE=T)
size<-terrmap$size
lat <- c(min(as.numeric(df3$latit))-1,max(as.numeric(df3$latit))+1) #define our map's ylim
lon <- c(min(as.numeric(df3$longit))-1,max(as.numeric(df3$longit))+1) #define our map's xlim


pdf(file=paste("NVpigmentation6Aug23.pdf", sep=""),width = size[1]/72, height = size[2]/72,pointsize = 15)
#terrmap<-GetMap.bbox(latR=c(min(cor_Ceur$lat)-1,max(cor_Ceur$lat)+1),lonR=c(min(cor_Ceur$lon)-1,max(cor_Ceur$lon)+1), maptype = "terrain", destfile = "terrain1.png",path = "&style=feature:all|element:labels|visibility:off", MINIMUMSIZE = T,GRAYSCALE=T)
size<-terrmap$size
PlotOnStaticMap(terrmap)


for (i in 1:nrow(df4)){ # i=1
  d1<-distance(df4$lat[i], df4$lat[i], lon[1], lon[2]) 
  d2<-distance(df4$lat[i], df4$lat[i], lon[1], df4$lon[i])
  a1<-size[1]/d1
  a2<-d2*a1
  a3<- -(size[1]/2)+a2
  b1<-size[2]/(lat[2]-lat[1])
  b2<-(df4$lat[i]-lat[1])*b1
  b3<- -(size[2]/2)+b2
  terrmap<-points(x=a3,y=b3, pch=20, cex=2, col=df4$color[i])}

for (i in 1:nrow(df3)){ # i=3
  d1<-distance(df3$latit[i], df3$latit[i], lon[1], lon[2]) 
  d2<-distance(df3$latit[i], df3$latit[i], lon[1], df3$longit[i])
  a1<-size[1]/d1
  a2<-d2*a1
  a3<- -(size[1]/2)+a2+df3$offsetx[i]
  b1<-size[2]/(lat[2]-lat[1])
  b2<-(df3$latit[i]-lat[1])*b1
  b3<- -(size[2]/2)+b2+df3$offsety[i]
  terrmap<-add.pie(z=c(df3$w[i],df3$pi[i],df3$pu[i]), x=a3, y=b3, radius=5, col = colors, labels=NA)
#  terrmap<-text(x=a3, y=b3, labels = df3$pop[i], pos = df3$pos[3i], col = df3$colorText[i], offset = 0.45,cex = 0.55)
}

### GWAS pops
for (i in 1:nrow(df5)){ # i=3
  d1<-distance(df5$lat[i], df5$lat[i], lon[1], lon[2]) 
  d2<-distance(df5$lat[i], df5$lat[i], lon[1], df5$lon[i])
  a1<-size[1]/d1
  a2<-d2*a1
  a3<- -(size[1]/2)+a2+df5$offsetx[i]
  b1<-size[2]/(lat[2]-lat[1])
  b2<-(df5$lat[i]-lat[1])*b1
  b3<- -(size[2]/2)+b2+df5$offsety[i]
  terrmap<-add.pie(z=c(as.numeric(unlist(strsplit(df5$color[i], ";"))[1]),as.numeric(unlist(strsplit(df5$color[i], ";"))[2]),as.numeric(unlist(strsplit(df5$color[i], ";"))[3])), x=a3, y=b3, radius=8, col = colors, labels=NA)
  terrmap<-text(x=a3, y=b3, labels = df5$pop[i], pos = df5$pos[i], col = df5$colorText[i], offset = 0.45,cex = 1)
  
  }

dev.off()










################## iNaturalist info ######################
library(rinat)
bounds <- c(44.60200, 13.00491, 50.41173, 25.88677)
aa<-get_inat_obs(query = "Arabidopsis arenosa", bounds = bounds,meta=F,maxresults = 1500)
dir.create("imagesINAT")
write.table(paste0("wget ",aa$image_url," -O ",aa$id,".jpg"),"imagesINAT/images.txt",col.names = F,row.names = F,quote = F)
### cd to imagesINAT, run the script which this command makes (you can change medium to large)
write.table(aa,"iNatData.txt",col.names = T,row.names = F,quote = F,sep = "\t")
#



############# GWAS ##############
############## For Making the GWAS list
library(data.table)
setwd("/home/aa/BetyProject/pigmentation/GWAScolor/")

popInfo<-fread("file:///home/aa/BetyProject/envAssoc/Data/popInfo.txt", h=T)
landscape<-fread("/home/aa/BetyProject/pigmentation/GWAScolor/Arabidopsis_landscape_genomics - Sheet1.csv")
landscape1<-subset(landscape,substr(landscape$pop,1,3) %in% popInfo$pop)
data_plants<-read.csv("/home/aa/BetyProject/pigmentation/GWAScolor/petalColorFromNatureClean.csv",h=T)
pops<-as.data.frame(table(substr(data_plants$pop,1,5)))


file.remove("colorsNV.txt")
for (pl in pops$Var1) { # pl = "AA171"
  df <- data_plants[substr(data_plants$pop, 1, 5) == pl,]
  nrow(df)
  #colSums(df[c("petalColour")] == 0)
  ## percentage of white petals
  w<-colSums(df[c("petalColour")] == 0)/nrow(df)
  ## percentage of pinkish petals
  pi<-colSums(df[c("petalColour")] == 1)/nrow(df)
  ## percentage of purple petals
  pu<-colSums(df[c("petalColour")] == 2)/nrow(df)
  write.table(t(as.data.frame(c(pl,w,pi,pu))),"colorsNV.txt",append = T,quote = F,sep = "\t",col.names = F,row.names = F)
  print(c(pl,w,pi,pu))
}
#data_plants<-read.table("colorsNV.txt",h=F)
prop<-read.table("colorsNV.txt",h=F)
#fill in samples
for (i in 1:nrow(landscape1)) { #  i=1
  if (substr(landscape1$colour,2,2)[i]!=';') {
    prop1<-subset(prop,prop$V1 %in% landscape1$AA_code[i])
    landscape1$colour[i]<- paste0(prop1$V2,";",prop1$V3,";",prop1$V4)
  } else {print(landscape1$colour[i])}
}
landscape1$colorNature<-""
for (i in 1:nrow(landscape1)) { #  i=1
  
  prop1<-subset(prop,prop$V1 %in% landscape1$AA_code[i])
  landscape1$colorNature[i]<- paste0(prop1$V2,";",prop1$V3,";",prop1$V4)
}

write.csv(file = "landscapeColor.csv",x = landscape1)

#MANUAL processing

aaa<-fread("colorGWASfinal.txt",h=T,sep = "\t")
aaa$colComp<-""
for (i in 1:nrow(aaa)) { # i=1
  w <-as.numeric(unlist(strsplit(aaa$colour[i], ";"))[1])
  pi<-as.numeric(unlist(strsplit(aaa$colour[i], ";"))[2])
  pu<-as.numeric(unlist(strsplit(aaa$colour[i], ";"))[3])
  colComp<-0*w + 1*pi + 2*pu
  aaa$colComp[i]<-colComp
}
hist(as.numeric(na.omit(aaa$colComp)),breaks = 100)
aaa<-aaa[ order(aaa$colComp,decreasing = T)]

aaa$colProp<-""
for (i in 1:nrow(aaa)) { # i=1
  w <-as.numeric(unlist(strsplit(aaa$colour[i], ";"))[1])
  pi<-as.numeric(unlist(strsplit(aaa$colour[i], ";"))[2])
  pu<-as.numeric(unlist(strsplit(aaa$colour[i], ";"))[3])
  colProp<-pi + pu
  aaa$colProp[i]<-colProp
}
hist(as.numeric(na.omit(aaa$colProp)),breaks = 100)
aaa<-aaa[ order(aaa$colProp,decreasing = T)]



aaa<-aaa[ order(aaa$pop,decreasing = F)]
popInfo<-popInfo[ order(popInfo$pop,decreasing = F)]
popInfo$colComp<-aaa$colComp
popInfo$colProp<-aaa$colProp
popInfo$color<-aaa$colour
popInfo$evidenceColor<-aaa$`ColorEst(1:Nature, 2: memories/photos, 3: iNat around, 4: very old memories`
write.table(popInfo,"/home/aa/BetyProject/envAssoc/Data/popInfo14Jun.txt",row.names = F,quote = F,sep = "\t")
write.table(popInfo,"/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/popInfo14Jun.txt",row.names = F,quote = F,sep = "\t")

###





## RDA
library(pegas)
library(ggplot2)
library(raster)
library(rgdal)
library(LEA)
library(rnaturalearth)
library(rnaturalearthdata)
library(RColorBrewer)
library(ggpubr)
library(vegan)
library(qvalue) #missing
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("qvalue")
library(robust)
library(WMDB) #missing
library(ggVennDiagram)
library(cowplot)
library(corrplot)
library(rgeos)
library(data.table)

######### Genotype-Environment Associations: identifying loci under selection: Standalone
setwd("/home/aa/BetyProject/envAssoc")
totAF<-fread("Data/WGDataAF.txt",h=T)
nons<-subset(totAF, totAF$ann %in% "missense_variant" | totAF$ann %like% "upstream")
nons<-subset(totAF, !totAF$ann %in% "intergenic_region" & !totAF$ann %like% "synonymous_variant")

nons<-na.omit(nons) ## alternatively run the script bellow on cluster
syn<-subset(totAF, totAF$ann %in% "synonymous_variant")
synSet <- na.omit(syn[runif(n = 20000, min = 1, max = nrow(syn)),]) #This is still filtered for MAF, which may be problematic
rm(totAF)
rm(syn)


#accunt for missing data: remove sites with, > 5 pops, the rest exchange by pop medians: killing my RAM!!!
#na_pop <- apply(AllFreq[,-1], 2, function(x) sum(is.na(x)))
#AllFreq <- AllFreq[,(which(na_pop<5)+1)]
#for (i in 1:ncol(AllFreq))
#{AllFreq[which(is.na(AllFreq[,i])),i] <- median(AllFreq[-which(is.na(AllFreq[,i])),i], na.rm=TRUE)}
Env<-fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/popInfo19Jun.txt")#new
Env<-subset(Env, !Env$colComp %in% NA & !Env$suspiciousColor %in% 1)
#Env<-subset(Env, Env$Lineage %in% "WCarpathian") #### COMMENT LATER

hist(Env$colProp,breaks = 100)
hist(Env$colComp,breaks = 100)


rownames(Env)<-Env$pop
colnamesAllFreq<-paste(nons$ID,nons$aas,nons$start,sep=".")
nons <- nons %>% dplyr:: select(rownames(Env))
AllFreq<-as.data.frame(t(nons[,1:ncol(nons)]))
colnames(AllFreq)<-colnamesAllFreq
AllFreq<-as.data.frame(AllFreq[ order(row.names(AllFreq)), ,drop=F])
Coordinates<-Env[,c("lon","lat","Altitude")]
Env<-Env[,c("colComp","colProp")] #new


## Standardization of the variables
Env <- scale(Env, center=TRUE, scale=TRUE) # center=TRUE, scale=TRUE are the defaults for scale()
## Recovering scaling coefficients
scale_env <- attr(Env, 'scaled:scale')
center_env <- attr(Env, 'scaled:center')
## Climatic table
Env <- as.data.frame(Env)
row.names(Env) <- row.names(AllFreq)
Variables <- data.frame(Coordinates, Env)
aaa<-colnames(Variables)
aaa<-aaa[!aaa %in% c("V1","lon","lat","Altitude")]
paste0(aaa, collapse=" + ")

#totAF<-fread("Data/WGDataAF.txt",h=T)
pca <- rda(t(synSet[,-1:-4]), scale=T) 
#screeplot(pca, type = "barplot", npcs=10, main="PCA Eigenvalues")
#biplot(pca,type = c("text","points"))
## Neutral population structure table
PCs <- scores(pca, choices=c(1:3), display="sites", scaling=0)
PopStruct <- data.frame(Population = colnames(synSet[,-1:-4]), PCs)
#rm(syn)
colnames(PopStruct) <- c("Population", "PC1", "PC2", "PC3")
PopStruct<-subset(PopStruct,PopStruct$Population %in% rownames(AllFreq))
Variables <- data.frame(Variables, PopStruct[,-1])



RDA_env <- rda(AllFreq ~ colComp + colProp + Condition(PC1 + PC2 + PC3),  Variables) #new
RDA_env <- rda(AllFreq ~ colComp + colProp,  Variables) #new

#RDA_env <- rda(AllFreq ~ colComp + PC1,  Variables) #new


screeplot(RDA_env, main="Eigenvalues of constrained axes")
source("./exampleOld/RDA-landscape-genomics-main/src/rdadapt.R")
rdadapt_env<-rdadapt(RDA_env,K=2)
## P-values threshold after Bonferroni correction
thres_env <- 0.0001/length(rdadapt_env$p.values)
#thres_env <- 0.0001/nrow(nons)

## Identifying the loci that are below the p-value threshold
outliers <- data.frame(Loci = colnames(AllFreq)[which(rdadapt_env$p.values<thres_env)], p.value = rdadapt_env$p.values[which(rdadapt_env$p.values<thres_env)])
outliers$contig<-substr(outliers$Loci,3,3)
outliers$gene<-substr(outliers$Loci,1,9)
write.table(outliers,"../../pigmentation/GWAScolor/outliers03Mar25.0.0001.txt")


## Top hit outlier per contig
outliers <- outliers[order(outliers$gene, outliers$p.value),]
## List of outlier gene names
outliers_rdadapt_env <- as.character(outliers$Loci[!duplicated(outliers$gene)])

## Formatting table for ggplot
locus_scores <- scores(RDA_env, choices=c(1:2), display="species", scaling="none") # vegan references "species", here these are the loci
TAB_loci <- data.frame(names = row.names(locus_scores), locus_scores)
TAB_loci$type <- "Neutral"
TAB_loci$type[TAB_loci$names%in%outliers$Loci] <- "All outliers"
TAB_loci$type[TAB_loci$names%in%outliers_rdadapt_env] <- "Top outliers"
#TAB_loci$type[TAB_loci$names %in% c("AL5G22700.Leu377Val.11157690")] <- "FAR5"  
#TAB_loci$type[TAB_loci$names %in% c("AL8G15030.Arg14Lys.3108031")] <- "MAP18"  


TAB_loci$type <- factor(TAB_loci$type, levels = c("Neutral", "All outliers", "Top outliers"))
TAB_loci <- TAB_loci[order(TAB_loci$type),]
TAB_var <- as.data.frame(scores(RDA_env, choices=c(1,2), display="bp")) # pull the biplot scores


### Manhattan plot
Outliers <- rep("Neutral", length(colnames(AllFreq)))
Outliers[colnames(AllFreq)%in%outliers$Loci] <- "All outliers"
Outliers[colnames(AllFreq)%in%outliers_rdadapt_env] <- "Top outliers"
#Outliers[colnames(AllFreq)%in%c("AL5G22700.Leu377Val.11157690")] <- "FAR5" 
#Outliers[colnames(AllFreq)%in%c("AL8G15030.Arg14Lys.3108031")] <- "MAP18"    


Outliers <- factor(Outliers, levels = c("Neutral", "All outliers", "Top outliers"))
TAB_manhatan <- data.frame(pos = 1:length(colnames(AllFreq)), 
                           pvalues = rdadapt_env$p.values, 
                           Outliers = Outliers)
TAB_manhatan <- TAB_manhatan[order(TAB_manhatan$Outliers),]
TAB_manhatan
TAB_manhatan<-subset(TAB_manhatan,TAB_manhatan$pvalues<0.0000000005)
pdf("ManhattanColorCompProp11Aug.pdf",width = 24,height = 8,pointsize = 16)
ggplot(data = TAB_manhatan) +
  geom_point(aes(x=pos, y=-log10(pvalues), col = Outliers), size=1.8) +
  scale_color_manual(values = c("gray90", "darkorchid", "darkorchid")) +
  xlab("Loci") + ylab("-log10(p.values)") +
  geom_hline(yintercept=-log10(thres_env), linetype="dashed", color = gray(.80), size=1) +
  facet_wrap(~"Manhattan plot", nrow = 3) +
  guides(color=guide_legend(title="Locus type")) +
  theme_bw(base_size = 24, base_family = "Times") +
  theme(legend.position="right", legend.background = element_blank(), panel.grid = element_blank(), legend.box.background = element_blank(), plot.background = element_blank(), panel.background = element_blank())
dev.off()

bbb<-fread("file:///home/aa/BetyProject/pigmentation/GWAScolor/outliers14Jun0.001.txt")
bbb<-subset(bbb,bbb$p.value < 0.00000000000000000001)
genes<-as.data.frame(table(bbb$gene))
genes<- subset(genes, genes$Freq >1)

write.table(as.data.frame(genes$Var1),"../claraMS/cuticle/thePipeline/geneSetGWAS19Jun.txt",col.names = F,row.names = F,quote = F)

########### PLAY WITH OUTLIERS
setwd("/home/aa/pigmentation/GWAScolor/")
library(data.table)
library(stringr)

outliers<-fread("file:///home/aa/pigmentation/GWAScolor/outliers03Mar25.0.0001.txt")
outliers$start<-as.numeric(sub(".*\\.", "", outliers$Loci))
totAF<-fread("/home/aa/BetyProject/envAssoc/Data/WGDataAF.txt",h=T)
nons <- merge(totAF, outliers, by.x = c("ID", "start"), by.y = c("gene", "start"))
fwrite(nons,"outliers03Mar25.0.0001.ann.csv")

###### Correlate outliers with altitude and colComp ####
library(data.table)
library(dplyr)
# Read in the population metadata
popInfo <- fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/popInfo19Jun.txt")
popInfo<-subset(popInfo, !popInfo$colComp %in% NA & !popInfo$suspiciousColor %in% 1)
outliers<-fread("outliers03Mar25.0.0001.ann.csv")
# Extract population names from outliers (columns after fixed columns)
# Extract population names that exist in both datasets
pop_cols <- intersect(colnames(outliers)[5:ncol(outliers)], popInfo$pop)

# Initialize results dataframe
cor_results <- data.frame(ID = outliers$ID, start = outliers$start, 
                          cor_Altitude = NA, cor_colComp = NA, 
                          p_Altitude = NA, p_colComp = NA)

# Loop through each row in outliers
for (i in 1:nrow(outliers)) { # i=1
  # Extract allele frequencies for the current SNP and subset to matching populations
  allele_freqs <- as.numeric(outliers[i, ..pop_cols])
  # Match populations with altitude and colComp
  altitude <- popInfo$Altitude[match(pop_cols, popInfo$pop)]
  colComp <- popInfo$colComp[match(pop_cols, popInfo$pop)]
  # Remove NAs before correlation calculation
  valid_alt <- complete.cases(allele_freqs, altitude)
  valid_col <- complete.cases(allele_freqs, colComp)
  # Calculate correlation and p-values
    cor_alt <- cor.test(allele_freqs[valid_alt], altitude[valid_alt], method = "pearson")
    cor_results$cor_Altitude[i] <- cor_alt$estimate
    cor_results$p_Altitude[i] <- cor_alt$p.value
    cor_col <- cor.test(allele_freqs[valid_col], colComp[valid_col], method = "pearson")
    cor_results$cor_colComp[i] <- cor_col$estimate
    cor_results$p_colComp[i] <- cor_col$p.value
}
cor_results$cor_colCompAbs<-abs(cor_results$cor_colComp)
cor_results$cor_AltitudeAbs<-abs(cor_results$cor_Altitude)
outliersAnn<-cbind(outliers,cor_results)
fwrite(outliersAnn,"outliers03Mar25.0.0001.ann.corr.csv")

### VISUALIZE
library(ggplot2)
# Select the top 10 SNPs with the highest absolute correlation to colComp
top_snps <- outliersAnn[order(-cor_colCompAbs)][1:10]
# Prepare PDF output
pdf("top10_SNPs_dotplot.pdf", width = 8, height = 6)
# Loop through each top SNP and create a dot plot
for (i in 1:nrow(top_snps)) {
  snp <- top_snps[i,]  # Select current SNP row
  allele_freqs <- as.numeric(snp[, ..pop_cols])  # Extract allele frequencies
  pop_names <- pop_cols  # Population names
  colors <- popInfo$colComp[match(pop_cols, popInfo$pop)]  # Match color composition
  # Create a data frame for plotting
  plot_data <- data.frame(Population = pop_names, AlleleFreq = allele_freqs, Color = colors)
  # Generate scatter plot with labeled points
  p <- ggplot(plot_data, aes(x = Color, y = AlleleFreq, label = Population)) +
    geom_point(color = "blue", size = 3) +  # Blue points
    geom_text(vjust = -1, size = 3) +  # Labels for each point
    labs(title = paste("SNP:", snp$ID, snp$start),
         x = "Color Composition",
         y = "Allele Frequency") +
    theme_minimal()
  print(p)  # Print plot to PDF
}
dev.off()

######### SUM 811 and 056
# Extract relevant SNPs from outliers
snp1 <- outliers[ID == "AL2G24170" & start == 11089811]
snp2 <- outliers[ID == "AL2G24190" & start == 11099056]
# Ensure populations exist in both SNPs and subset only matching populations
pop_cols <- intersect(colnames(snp1)[5:ncol(snp1)], colnames(snp2)[5:ncol(snp2)])
# Sum allele frequencies across the two SNPs
allele_freq_sum <- as.numeric(snp1[, ..pop_cols]) + as.numeric(snp2[, ..pop_cols])
# Match populations with color composition
colors <- popInfo$colComp[match(pop_cols, popInfo$pop)]
# Remove NA values before correlation calculation
valid <- complete.cases(allele_freq_sum, colors)
# Compute correlation
cor_result <- cor.test(allele_freq_sum[valid], colors[valid], method = "pearson")
# Create a data frame for plotting
plot_data <- data.frame(Population = pop_cols[valid], 
                        AlleleFreqSum = allele_freq_sum[valid], 
                        Color = colors[valid])
# Save the plot as a PDF
pdf("sum_allele_freq_vs_color.pdf", width = 8, height = 6)
# Generate scatter plot with labeled points
p <- ggplot(plot_data, aes(x = Color, y = AlleleFreqSum, label = Population)) +
  geom_point(color = "red", size = 3) +  # Red points
  geom_text(vjust = -1, size = 3) +  # Labels for each point
  labs(title = "Sum of Allele Frequencies vs Color Composition",
       subtitle = paste("Pearson r =", round(cor_result$estimate, 3), 
                        ", p-value =", signif(cor_result$p.value, 3)),
       x = "Color Composition",
       y = "Summed Allele Frequency") +
  theme_minimal()
print(p)  # Print plot to PDF
# Close PDF device
dev.off()
# Print correlation result
print(cor_result)




#1. hodnota AFD pro tom SNPy
#2. kolik z top 100 SNPu se prekryva?
#3. pokud ano, je mezi nimi nejaky zajimavy gen? (AT kody)



############### Double Manhattan Plot GWAS - selection scans
# Set working directory
setwd("/home/aa/pigmentation/GWAScolor/selectionScanAFD/")
# Load required libraries
library(data.table)
library(dplyr)
library(qqman)  # For Manhattan plot

### DEFINE PARAMETERS ###
adaptation <- "colComp"
project <- "colCompGW"
tresholdSNP <- 0.9999
#tresholdGene <- 0.5
popInfo <- fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/popInfo19Jun.txt", header = TRUE, sep = "\t")
typeOfSNPs <- c("upstream_gene_variant", "missense_variant", "downstream_gene_variant")
# Define environment groups
derivedEnv <- c("ZEP", "VEL", "LAC", "BAL", "RFT", "CHO", "HLI")
ancestralEnv <- c("BAB", "BDO", "BEL", "BIH", "FOJ", "GOR", "HMC", "HNE", "HNI", "HRA","HRN", "KAS", "KLE", "KOW", "KRM", "KZL", "MIA", "MIE", "PHD", "PHT", "PRE", "SNO", "STE", "SUB", "SWJ", "SZI", "ZID", "ZIT")
### GENOME-WIDE SELECTION SCAN ###
totAF <- fread("/home/aa/BetyProject/envAssoc/Data/WGDataAF.txt", header = TRUE)
# Filter by SNP type
if (typeOfSNPs != "all") {
  totAF <- totAF[ann %in% typeOfSNPs]
}
# Select columns for allele frequency calculations
afANC <- totAF[, ..ancestralEnv]
afDER <- totAF[, ..derivedEnv]
# Compute allele frequency difference (AFD) efficiently using rowMeans()
totAF[, ancAF := rowMeans(.SD, na.rm = TRUE), .SDcols = ancestralEnv]
totAF[, derAF := rowMeans(.SD, na.rm = TRUE), .SDcols = derivedEnv]
totAF[, AFD := abs(ancAF - derAF)]
# Histogram and summary of AFD values
hist(totAF$AFD, breaks = 100)
summary(totAF$AFD)
### FIND OUTLIERS (AFD > 0.0001) ###
AFD_threshold <- quantile(totAF$AFD, tresholdSNP, na.rm = TRUE)
AFD_outliers <- totAF[AFD > AFD_threshold, .(ID, start, AFD)]
# Export AFD outliers to file
fwrite(AFD_outliers, "AFD_outliers_0.0001.csv", sep = "\t")

### OPTIONAL: GENERATE MANHATTAN PLOT DATA ###
manhPlota <- totAF[, .(SNP = as.numeric(start),
                       CHR = as.numeric(substr(ID, 3, 3)), 
                       BP = aas, 
                       P = as.numeric(AFD))]

# Filter for values above a certain threshold
manhPlota <- manhPlota[P > 0.3]
# Export Manhattan plot data
fwrite(manhPlota, "manhattan_plot_data.csv", sep = "\t")

######### FIND OVERLAP
# Define file paths (update these as necessary)
afd_file <- "file:///home/aa/pigmentation/GWAScolor/selectionScanAFD/AFD_outliers_0.0001.csv"
gwas_file <- "/home/aa/pigmentation/GWAScolor/outliers03Mar25.0.0001.ann.corr.csv"
# Load datasets
AFD_outliers <- fread(afd_file, sep = "\t")
GWAS_outliers <- fread(gwas_file, sep = ",")

# Ensure common column names for SNP identification
setnames(AFD_outliers, c("ID", "start"), c("SNP_ID", "SNP_start"))
setnames(GWAS_outliers, c("ID", "start"), c("SNP_ID", "SNP_start"))
# Find overlapping SNPs between AFD_outliers and GWAS_outliers
common_snps <- merge(AFD_outliers, GWAS_outliers, by = c("SNP_ID", "SNP_start"))
# Export overlapping SNPs
fwrite(common_snps, "overlapping_SNPs_AFD_GWAS.csv", sep = "\t")
# Select the top 100 GWAS outliers with the lowest p-value
GWAS_top100 <- GWAS_outliers[order(p.value)][1:100]
# Find overlapping SNPs between AFD_outliers and the top 100 GWAS SNPs
common_snps_top100 <- merge(AFD_outliers, GWAS_top100, by = c("SNP_ID", "SNP_start"))
# Export overlapping SNPs for AFD and top 100 GWAS
fwrite(common_snps_top100, "overlapping_SNPs_AFD_GWAS_top100.csv", sep = "\t")
# Print results
cat("Total overlapping SNPs between AFD and GWAS:", nrow(common_snps), "\n")
cat("Total overlapping SNPs between AFD and Top 100 GWAS:", nrow(common_snps_top100), "\n")


#### ANNOTATE into A. thaliana
# Define file paths
common_snps_file <- "overlapping_SNPs_AFD_GWAS.csv"  # Adjust path if needed
alat_dict_file <- "/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt"
# Load datasets
common_snps <- fread(common_snps_file, sep = "\t")
alat_dict <- fread(alat_dict_file, sep = "\t")
# Rename columns for consistency
setnames(alat_dict, c("AL", "AT"), c("SNP_ID", "A_thaliana_gene"))
# Merge `common_snps` with `alat_dict` to get *A. thaliana* genes
mapped_snps <- merge(common_snps, alat_dict, by = "SNP_ID", all.x = TRUE)
# Ensure that missing A. thaliana genes are replaced with "nnn"
mapped_snps[is.na(A_thaliana_gene), A_thaliana_gene := "nnn"]
# Extract only the part before the dot (e.g., AT5G63930 from AT5G63930.1)
mapped_snps[, A_thaliana_gene := sub("\\..*", "", A_thaliana_gene)]
### GENES
# Count the number of SNPs per *A. thaliana* gene
gene_counts <- mapped_snps[, .N, by = A_thaliana_gene]
# Filter genes with more than 1 SNP
genes_with_multiple_snps <- gene_counts[N > 1]
# Merge back to get full data for these genes
multi_snp_genes <- merge(mapped_snps, genes_with_multiple_snps, by = "A_thaliana_gene")
# Export the results
output_file <- "A_thaliana_genes_with_multiple_SNPs.csv"
fwrite(multi_snp_genes, output_file, sep = "\t")
# Export the cleaned results
output_file <- "common_snps_with_A_thaliana_genes_cleaned.csv"
fwrite(mapped_snps, output_file, sep = "\t")


########## MANHATTAN PLOT
# Load required libraries
library(data.table)
library(ggplot2)
library(patchwork)  # For combining plots
gwas_file <- "file:///home/aa/pigmentation/GWAScolor/outliers03Mar25.0.0001.ann.csv"
afd_file <- "file:///home/aa/pigmentation/GWAScolor/selectionScanAFD/manhattan_plot_data.csv"
# Load data
gwas_data <- fread(gwas_file, sep = ",")  # GWAS data (from outliers file)
afd_data <- fread(afd_file, sep = "\t")  # AFD data (from Manhattan plot file)
# Ensure numeric types and rename chromosome column for consistency
gwas_data[, `:=`(CHR = as.numeric(contig), BP = as.numeric(start), P = -log10(as.numeric(p.value)))]
afd_data[, `:=`(CHR = as.numeric(CHR), BP = as.numeric(SNP), AFD = as.numeric(P))]

# Remove NA values
gwas_data <- na.omit(gwas_data)
afd_data <- na.omit(afd_data)
# Ensure chromosome order (1-8)
gwas_data <- gwas_data[order(CHR, BP)]
afd_data <- afd_data[order(CHR, BP)]
# Compute cumulative offsets to spread chromosomes sequentially
chr_offsets <- gwas_data[, .(CHR_MAX = max(BP)), by = CHR]  # Get max BP per chromosome
chr_offsets[, CHR_OFFSET := cumsum(shift(CHR_MAX, fill = 0))]  # Compute cumulative offsets

# Merge offsets back to the main dataset
gwas_data <- merge(gwas_data, chr_offsets[, .(CHR, CHR_OFFSET)], by = "CHR")
afd_data <- merge(afd_data, chr_offsets[, .(CHR, CHR_OFFSET)], by = "CHR")

# Compute final chromosome positions
gwas_data[, CHR_POS := BP + CHR_OFFSET]
afd_data[, CHR_POS := BP + CHR_OFFSET]

# Compute chromosome midpoints for correct x-axis labels
chr_midpoints_gwas <- gwas_data[, .(CHR_MID = mean(CHR_POS)), by = CHR]
chr_midpoints_afd <- afd_data[, .(CHR_MID = mean(CHR_POS)), by = CHR]

# Determine the 0.0001 AFD threshold
afd_threshold <- quantile(afd_data$AFD, 0.999, na.rm = TRUE)

# Manhattan plot for GWAS (top panel)
gwas_plot <- ggplot(gwas_data, aes(x = CHR_POS, y = P, color = as.factor(CHR))) +
  geom_point(size = 1.2, alpha = 0.8) +
  scale_x_continuous(breaks = chr_midpoints_gwas$CHR_MID, labels = chr_midpoints_gwas$CHR) +
  scale_color_manual(values = rep(c("black", "gray50"), length.out = max(gwas_data$CHR, na.rm = TRUE))) +
  labs(title = "GWAS Manhattan Plot", x = "Chromosome", y = expression(-log[10](italic(p)))) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Manhattan plot for AFD (bottom panel)
afd_plot <- ggplot(afd_data, aes(x = CHR_POS, y = AFD, color = as.factor(CHR))) +
  geom_point(size = 1.2, alpha = 0.8) +
  geom_hline(yintercept = 0.64, color = "red", linetype = "dashed", size = 0.5) +
  scale_x_continuous(breaks = chr_midpoints_afd$CHR_MID, labels = chr_midpoints_afd$CHR) +
  scale_color_manual(values = rep(c("black", "gray50"), length.out = max(afd_data$CHR, na.rm = TRUE))) +
  labs(title = "AFD Manhattan Plot", x = "Chromosome", y = "Allele Frequency Difference") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Export as high-resolution PDF
pdf("Manhattan_Plot_gwas.pdf", width = 10, height = 3)
print(gwas_plot)
dev.off()
pdf("Manhattan_Plot_afd.pdf", width = 10, height = 3)
print(afd_plot)
dev.off()


#

setwd("/home/aa/BetyProject/claraMS/cuticle/thePipeline/")
# Libraries
library(data.table)
library(dplyr)
library(gplots)
library(RColorBrewer)
library(qqman)
library(stringr)
### PART 0. DEFINE PARAMETERS ### 
### this part is common for all other parts
# User-supplied parameters 
adaptation<-"colComp" # select environment of interest. It has to correspond to one of columns in 'popInfo.txt'.
project<-"colCompGW" # project name will appear in the output tables name
tresholdSNP<-0.9999 # outlier SNPs above this threshold, only for Part 2 & 3
tresholdGene<-0.5 # outlier SNPs density per gene cutoff, takes top tresholdGene genes. (0.25 takes top 25%). 
popInfo<-fread("data/popInfo19Jun.txt",h=T,sep="\t") # names and environmental variables for populations
typeOfSNPs<-c("upstream_gene_variant" , "missense_variant","downstream_gene_variant") # "all" or anything else

derivedEnv<-c("ZEP","VEL","LAC","BAL", "RFT", "CHO","HLI")#populations used for selections scan. In this case alpine populaions. only for Part 2 & 3
ancestralEnv<-c("BAB", "BDO","BEL","BIH","FOJ","GOR","HMC","HNE","HNI","HRA","HRN","KAS", "KLE","KOW","KRM","KZL","MIA","MIE","PHD","PHT","PRE","SNO","STE","SUB","SWJ","SZI","ZID","ZIT") #populations used for selections scan. In this case foothill populaions. only for Part 2 & 3
#Alternatively define based on environmental values from popInfo.txt:
#derivedEnv<-subset(popInfo,popInfo$Altitude>1600)$pop
#ancestralEnv<-subset(popInfo,popInfo$Altitude<900 & popInfo$Altitude>400)$pop


### PART 4: GENOME-WIDE SELECTION SCAN ###
totAF<-fread("/home/aa/BetyProject/envAssoc/Data/WGDataAF.txt",h=T)

if (typeOfSNPs == "all") {} else
{totAF<-subset(totAF,totAF$ann %in% typeOfSNPs)}

afANC <- as.data.frame(totAF %>% dplyr:: select(starts_with(ancestralEnv)))
afDER <- as.data.frame(totAF %>% dplyr:: select(starts_with(derivedEnv)))

# Compute allele frequency difference (AFD)
totAF$ancAF<-apply(afANC,1,mean,na.rm=T)
totAF$derAF<-apply(afDER,1,mean,na.rm=T) #runs a bit (1 minute?)
totAF$AFD<-abs(totAF$ancAF-totAF$derAF)

hist(totAF$AFD,breaks = 100)
summary(totAF$AFD)

##### OPTIONAL: visualize manhattan plot
#manhPlota<-totAF[,c(totAF$start,substr(totAF$ID,3,3),totAF$aas,totAF$AFD)]
manhPlota<-as.data.frame(cbind(totAF$start,substr(totAF$ID,3,3),totAF$aas,totAF$AFD))
#manhPlota$scaff<-as.numeric(substr(manhPlota$scaff,10,11))
colnames(manhPlota)<-c("SNP","CHR","BP","P")
manhPlota$CHR<-as.numeric(manhPlota$CHR)
manhPlota$SNP<-as.numeric(manhPlota$SNP)
manhPlota$P<-as.numeric(manhPlota$P)
manhPlota<-subset(manhPlota, manhPlota$P > 0.26)
#manhPlota$SNPname<-paste(manhPlota$CHR,manhPlota$SNP,sep="_")

manhPlotb<-fread("file:///home/aa/pigmentation/GWAScolor/outliers11Aug0.0001.txt")
manhPlotb$SNP<-str_extract(manhPlotb$Loci, "(?<=\\.)\\d+$")
manhPlotb<-as.data.frame(cbind(manhPlotb$SNP,manhPlotb$contig,manhPlotb$X,manhPlotb$p.value))
colnames(manhPlotb)<-c("SNP","CHR","BP","P")
manhPlotb$CHR<-as.numeric(manhPlotb$CHR)
manhPlotb$SNP<-as.numeric(manhPlotb$SNP)
manhPlotb$P<-as.numeric(manhPlotb$P)


pdf(paste0("../../../../pigmentation/GWAScolor/manhattan_",project,adaptation,".pdf"),width = 18,height = 16, pointsize = 20)
par(mfrow=c(2,1))
manhattan(manhPlotb, chr="CHR", bp="SNP", p="P",snp = "BP",logp = T,ylab="GWAS p-value", suggestiveline=55,ylim=c(10,65))
manhattan(manhPlota, chr="CHR", bp="SNP", p="P",snp = "BP",logp = F,ylab="AFD pink - white", suggestiveline=0.75,ylim=c(0.25,0.85))#quantile(totAF$AFD,0.9999)
dev.off()





v1<-fread("/home/aa/pigmentation/tkpAnalysis/geneSet_AnthocyaninShort.txt")
v2<-fread("/home/aa/pigmentation/tkpAnalysis/genes2.txt")
ann<-fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/LyV2_TAIR11orth_des_20171231.txt") # gene annotation and description

v21<-subset(v2, !v2$AL %in% v1$AL)

vv<-rbind(v1, v21,fill=T)
vv<-vv[order(vv)]
ann1<-subset(ann,ann$AL %in% vv$AL)

vv$ann<-''
for (i in 1:nrow(vv)) {
  al<-vv$AL[i]
  ann1<-subset(ann,ann$AL %in% al)
  vv$ann[i]<-paste(ann1$annShort,ann1$annCurator,ann1$AnnComputer,sep="\t")
  
}

write.table(vv,"/home/aa/pigmentation/tkpAnalysis/anthocyaninsCombined18Aug.txt",row.names = F,quote = F,sep = "\t")






####################### LISA'S DATA
library(ggpubr)
library(ggplot2)
library(lme4)
library(stringr)
library(readr)
setwd("~/pigmentation/forStudents/LisaNew")
data_ex <- read.csv("../experimentInfoPlants.csv")
data_nat <- read.csv("../naturalPopInfo.csv")
data_meas <- read.csv("Measurements_Color.csv")


indiv<-as.data.frame(table(data_meas$label))
indiv$meanFlo<-""
indiv$meanLeaf<-""
indiv$own_estimation_flo<-""
indiv$own_estimation_leaf<-""
indiv$ecotype<-""
indiv$lineage<-""
datPop<-subset(data_nat,data_nat$Code %in% substr(indiv$Var1,4,8))

### mean of means for flowerColor
for (i in 1:nrow(indiv)) { # i=37
  aaa<-subset(data_meas, data_meas$label %in% indiv[i,"Var1"])
  indiv$meanFlo[i]<-mean(aaa$mean_flo)
  indiv$own_estimation_flo[i]<-aaa$own_estimation_flo[1]
  indiv$own_estimation_leaf[i]<-aaa$own_estimation_leaf[1]
  indiv$meanLeaf[i]<-mean(as.numeric(str_replace_all(aaa$mean_leaf,',','.')))
  if (nrow(subset(aaa,substr(aaa$label,4,8) %in% subset(datPop, datPop$altit < 1600)$Code))>1) {indiv$ecotype[i]<-"F"} else {indiv$ecotype[i]<-"A"}
  indiv$lineage[i]<-subset(datPop, datPop$Code %in% substr(aaa$label,4,8)[1])$State}
indiv$treatment<-substr(indiv$Var1,1,2)
indiv$ecoTreat<-paste0(indiv$ecotype,indiv$treatment)
indiv$sqrtPinkness<-sqrt(255-(as.numeric(indiv$meanFlo)))



#minof mins? mean of mins?
for (i in 1:nrow(indiv)) { # i=37
  aaa<-subset(data_meas, data_meas$label %in% indiv[i,"Var1"])
  indiv$meanFlo[i]<-mean(aaa$min_flo)
  indiv$own_estimation_flo[i]<-aaa$own_estimation_flo[1]
  indiv$own_estimation_leaf[i]<-aaa$own_estimation_leaf[1]
  indiv$meanLeaf[i]<-mean(as.numeric(str_replace_all(aaa$min_leaf,',','.')))
  if (nrow(subset(aaa,substr(aaa$label,4,8) %in% subset(datPop, datPop$altit < 1600)$Code))>1) {indiv$ecotype[i]<-"F"} else {indiv$ecotype[i]<-"A"}
  indiv$lineage[i]<-subset(datPop, datPop$Code %in% substr(aaa$label,4,8)[1])$State}
indiv$treatment<-substr(indiv$Var1,1,2)
indiv$ecoTreat<-paste0(indiv$ecotype,indiv$treatment)
indiv$sqrtPinkness<-sqrt(255-(as.numeric(indiv$meanFlo)))



cor.test(as.numeric(indiv$own_estimation_flo),as.numeric(indiv$meanFlo))
cor.test(as.numeric(indiv$own_estimation_flo),as.numeric(indiv$sqrtPinkness))
cor.test(as.numeric(indiv$own_estimation_leaf),as.numeric(indiv$sqrtPinkness))
cor.test(as.numeric(indiv$meanLeaf),as.numeric(indiv$meanFlo))

hist(sqrt(255-(as.numeric(indiv$meanFlo))),nclass = 100)
hist(scale(-as.numeric(indiv$meanFlo)),nclass = 100)
hist(as.numeric(indiv$meanFlo)-mean(as.numeric(indiv$meanFlo)),nclass = 100)

hist(as.numeric(indiv$meanLeaf),nclass = 100)

# look at correlations
ggplot(indiv, aes(x = own_estimation_leaf, y=as.numeric(sqrtPinkness))) + geom_boxplot(inherit.aes = T, outlier.color = "red") + geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.05, height = 0.05)) + labs(x = "leafColor", y = "flowerColor")

anova(lmer(as.numeric(indiv$sqrtPinkness)~indiv$own_estimation_leaf+(1|indiv$ecoTreat)))
mean(as.numeric(subset(indiv,indiv$own_estimation_leaf %in% "0")$meanFlo))
mean(as.numeric(subset(indiv,!indiv$own_estimation_leaf %in% "0")$meanFlo))



indiv2<-subset(indiv, !indiv$ecotype %in% "A")
ggplot(indiv2, aes(x = own_estimation_leaf, y=as.numeric(sqrtPinkness))) + geom_boxplot(inherit.aes = T, outlier.color = "red") + geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.05, height = 0.05)) + labs(x = "leafColor", y = "flowerColor")


### Visualize and see differences sqrtPinkness
ggplot(indiv, aes(x = ecoTreat, y=(as.numeric(sqrtPinkness)))) + geom_boxplot(inherit.aes = T, outlier.color = "red") + geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.05, height = 0.05)) + labs(x = "ecotype, treatment", y = "flowerColor")
wilcox.test(subset(indiv,indiv$ecotype %in% "A")$sqrtPinkness,subset(indiv,indiv$ecotype %in% "F")$sqrtPinkness)
summary(lm(indiv$sqrtPinkness~indiv$ecotype))

mean(as.numeric(subset(indiv,indiv$ecotype %in% "A")$meanFlo))
mean(as.numeric(subset(indiv,indiv$ecotype %in% "F")$meanFlo))

library(lmerTest)
anova(lmer(indiv$sqrtPinkness~indiv$ecotype+(1|indiv$treatment)))



## color based on lineage and treatment
indiv1<-subset(indiv, !indiv$lineage %in% "AT")
anova(lmer(indiv1$sqrtPinkness~indiv1$ecotype+(1|indiv1$treatment)))



indiv1<-subset(indiv, indiv$treatment %in% "LH") 
indiv1<-subset(indiv, indiv$treatment %in% "HL") 
#!indiv$lineage %in% "AT" &

#!indiv$lineage %in% "AT" &
#indiv1$pinkness<-250-as.numeric(indiv1$meanFlo)
ggplot(indiv1, aes(x = ecoTreat, y=(as.numeric(sqrtPinkness)))) + geom_boxplot(inherit.aes = T, outlier.color = "red") + geom_jitter(shape = 15, color = "black", position = position_jitter(width = 0.05, height = 0.05)) + labs(x = "ecotype, treatment", y = "flowerColor")

wilcox.test(subset(indiv1,indiv1$ecotype %in% "A")$sqrtPinkness,subset(indiv1,indiv1$ecotype %in% "F")$sqrtPinkness)
summary(lm(indiv1$sqrtPinkness~indiv1$ecotype))

mean(as.numeric(subset(indiv1,indiv1$ecotype %in% "A")$meanFlo))
mean(as.numeric(subset(indiv1,indiv1$ecotype %in% "F")$meanFlo))

############### INDUCTION BY DROUGHT
# Load necessary libraries
library(ggplot2)
library(reshape2)
library(dplyr)
data <- data.frame(
  Population = c("ZEPD", "ZEPN", "TKOD", "TKON", "HRAD", "HRAN", "SUBD", "SUBN"),
  White = c(0.05, 0, 0.636, 0.1, 0.714, 1, 1, 1),
  Pink = c(0.3, 0.722, 0.273, 0.6, 0.286, 0, 0, 0),
  DarkPink = c(0.65, 0.277, 0.091, 0.3, 0, 0, 0, 0)
)
# Transform data to long format
data_long <- melt(data, id.vars = "Population", variable.name = "Color", value.name = "Proportion")
# Create a new column to distinguish drought-treated (D) from natural (N)
data_long$Treatment <- ifelse(grepl("D$", data_long$Population), "Drought", "Natural")
# Order populations to ensure Drought (D) and Natural (N) pairs are next to each other
data_long$Population <- factor(data_long$Population, levels = c("ZEPD", "ZEPN", "TKOD", "TKON", "HRAD", "HRAN", "SUBD", "SUBN"))
# Define colors for stacked bars
color_palette <- c("White" = "white", "Pink" = "pink", "DarkPink" = "magenta")
# Generate stacked bar plot
stacked_plot <- ggplot(data_long, aes(x = Population, y = Proportion, fill = Color)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette) +
  labs(
    title = "Induction of Pink Coloration After Drought Treatment",
    x = "Population",
    y = "Proportion",
    fill = "Color Category"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Save the plot as a PDF
ggsave("Drought_Color_Induction_StackedBar.pdf", plot = stacked_plot, width = 10, height = 6, device = "pdf")






##### ASSEMBLIES LONG READS ####
#install.packages("remotes")
#remotes::install_github("elhumble/primerdesignR")
library(primerdesignR)
library(data.table)
setwd("/home/aa/pigmentation/mappingExp/indelsPrimers/")

# in readmePigmentation.sh
#cd blast/bin
#./makeblastdb -input_type fasta -in /home/aa/pigmentation/indelsPrimers/arenosaAssemblies/VEL/VEL3171_CCS_Q10.a_ctg.p_ctg.fa -dbtype nucl -title VEL3171_CCS_Q10.a_ctg.p_ctg.fa.blast-db -out /home/aa/pigmentation/indelsPrimers/arenosaAssemblies/VEL/VEL3171_CCS_Q10.a_ctg.p_ctg.fa.blast-db

pops<-c("ZID","VEL","SUB","BDO","GUL","ING","KAM","BAL","VLA", "BUD", "BOR") # ,"BUD"
region<-"FAR5"

for (pop in pops) {  # pop<-"VEL"
  blastseqs2genome(indir=paste0(region,"/query/"), genomepath=paste0("arenosaAssemblies/",pop,".fa.blast-db"), outdir=paste0(region,"/"), blastpath="blast/blastn")
  file.rename(paste0(region,"/",region,"2genome"), paste0(region,"/",pop,region,"2genome"))
  aa<-fread(paste0(region,"/",pop,region,"2genome"))
  aa<-aa[ order(aa[,3],decreasing = T), ]
  aa<-aa[1:4,]
  aa<-na.omit(aa)
  for (i in 1:nrow(aa)) {
    print(paste0(pop,i))
  if (aa[i,]$V10 > aa[i,]$V11) {extract_target_location(chr=aa[i,]$V2, start=aa[i,]$V11, end=aa[i,]$V10, outdir=paste0(region,"/"), genomepath=paste0("arenosaAssemblies/",pop,".fa"),bedtoolspath = "bedtools")
    print(paste0(pop," reversed"))} else 
    {extract_target_location(chr=aa[i,]$V2, start=aa[i,]$V10, end=aa[i,]$V11, outdir=paste0(region,"/"), genomepath=paste0("arenosaAssemblies/",pop,".fa"),bedtoolspath = "bedtools")}
  bb<-read.table(paste0(region,"/target_region.fasta"),h=T)
  colnames(bb)<-paste0(">",pop,region,i)
  write.table(bb,paste0(region,"/",pop,region,i,".fasta"),row.names = F,col.names = T,quote = F)  }
}
file.remove(paste0(region,"/target_region.fasta"))
file.remove(paste0(region,"/target_region.bed"))

### Use in Geneious, design primers, download separate files into a new folder 'primers' then test them here

pop="VEL"
blastprimers2genome(indir="/home/aa/pigmentation/mappingExp/indelsPrimers/primers/", genomepath=paste0("arenosaAssemblies/",pop,".fa.blast-db"), outdir="/home/aa/pigmentation/mappingExp/indelsPrimers/primers/", blastpath="blast/blastn")

################## ALTERNATIVE FOR THE EXTRACTION OF THE FULL GENE
##### ASSEMBLIES LONG READS – s omezením na ±2 kb ####
library(primerdesignR)
library(data.table)

setwd("/home/aa/pigmentation/mappingExp/indelsPrimers/")

pops <- c("ARA","ZID", "VEL", "SUB", "BDO", "GUL", "ING", "KAM", "BAL", "VLA", "BUD", "BOR")
region <- "ttg2"
dist_limit <- 2000  # maximální vzdálenost od top hitu v bp

for (pop in pops) {
  blastseqs2genome(
    indir = paste0(region, "/query/"),
    genomepath = paste0("arenosaAssemblies/", pop, ".fa.blast-db"),
    outdir = paste0(region, "/"),
    blastpath = "blast/blastn"
  )
  
  file.rename(paste0(region, "/", region, "2genome"),
              paste0(region, "/", pop, region, "2genome"))
  
  aa <- fread(paste0(region, "/", pop, region, "2genome"))
  aa <- na.omit(aa)
  if (nrow(aa) == 0) next
  
  # Nejlepší hit
  top_hit <- aa[which.max(aa$V3), ]
  aa_chr <- top_hit$V2
  
  # Pozice nejlepšího hitu
  top_start <- min(top_hit$V10, top_hit$V11)
  top_end <- max(top_hit$V10, top_hit$V11)
  
  # Vyber pouze hity ze stejného chromozomu, jejichž začátek nebo konec je do ±2 kb od top hitu
  aa_close <- aa[
    V2 == aa_chr &
      (
        abs(V10 - top_start) <= dist_limit | abs(V11 - top_start) <= dist_limit |
          abs(V10 - top_end) <= dist_limit   | abs(V11 - top_end) <= dist_limit
      )
  ]
  
  if (nrow(aa_close) == 0) next  # žádné blízké hity – přeskočit
  
  # Rozsah oblasti k extrakci
  region_start <- min(pmin(aa_close$V10, aa_close$V11))
  region_end   <- max(pmax(aa_close$V10, aa_close$V11))
  
  extract_target_location(
    chr = aa_chr,
    start = region_start,
    end = region_end,
    outdir = paste0(region, "/"),
    genomepath = paste0("arenosaAssemblies/", pop, ".fa"),
    bedtoolspath = "bedtools"
  )
  
  # Načti FASTA a ulož
  bb <- read.table(paste0(region, "/target_region.fasta"), h = TRUE)
  colnames(bb) <- paste0(">", pop, region, "_clipped2kb")
  write.table(bb, paste0(region, "/", pop, region, "_clipped2kb.fasta"),
              row.names = FALSE, col.names = TRUE, quote = FALSE)
}

file.remove(paste0(region, "/target_region.fasta"))
file.remove(paste0(region, "/target_region.bed"))



################### association mapping
library(data.table)
library(stringr)
setwd("/home/aa/pigmentation/mappingExp/mapping/")

#download .csv from GDrive, replace weird signs by normal alleles
cul<-fread("/home/aa/pigmentation/mappingExp/mapping/cultivation - overview27Feb.csv")
cul<-cul[,c(1:3,5,10:12)]
colnames(cul)<-c("indiv","line","seedlCol","petalCol","l811","l019","l056")
length(subset(cul,substr(cul$line,1,1) %in% "t")$line)
length(subset(cul,substr(cul$line,1,1) %in% "z")$line)

#cul1<-subset(cul, !cul$l811 %in% NA)
cul1<-subset(cul, !cul$l811 %in% "na")
cul1<-subset(cul1, !cul1$l019 %in% "na")
cul1<-subset(cul1, !cul1$l056 %in% "na")
cul1<-subset(cul1, !cul1$l056 %in% "")

length(subset(cul1,substr(cul1$line,1,1) %in% "t")$line)
length(subset(cul1,substr(cul1$line,1,1) %in% "z")$line)

cul1<-subset(cul1, !cul1$petalCol %in% NA)
length(subset(cul1,substr(cul1$line,1,1) %in% "t")$line)
length(subset(cul1,substr(cul1$line,1,1) %in% "z")$line)


cul1$l811p <- str_count(cul1$l811, "p")
cul1$l019p <- str_count(cul1$l019, "p")
cul1$l056p <- str_count(cul1$l056, "p")

cul1$l811pAF<-""
cul1$l019pAF<-""
cul1$l056pAF<-""




for (i in 1:nrow(cul1)) { # i=1
  if (substr(cul1[i,2],1,1)== "t") {
    cul1$l811pAF[i]<- as.numeric(cul1$l811p[i])/4
    cul1$l019pAF[i]<- as.numeric(cul1$l019p[i])/4
    cul1$l056pAF[i]<- as.numeric(cul1$l056p[i])/4
  } else {
    cul1$l811pAF[i]<- as.numeric(cul1$l811p[i])/2
    cul1$l019pAF[i]<- as.numeric(cul1$l019p[i])/2
    cul1$l056pAF[i]<- as.numeric(cul1$l056p[i])/2
  }
}
cul1$lsumpAF<-as.numeric(cul1$l811pAF)+as.numeric(cul1$l019pAF)+as.numeric(cul1$l056pAF)
cul1$l811.056pAF<-as.numeric(cul1$l811pAF)+as.numeric(cul1$l056pAF)

plot(cul1$petalCol ~ cul1$l811pAF)
plot(cul1$petalCol ~ cul1$l019pAF)
plot(cul1$petalCol ~ cul1$l056pAF)
plot(cul1$petalCol ~ cul1$lsumpAF)
plot(cul1$petalCol ~ cul1$l811.056pAF)

cult<-subset(cul1,substr(cul1$line,1,1)== "t")
culd<-subset(cul1,substr(cul1$line,1,1)== "z")

boxplot(cult$petalCol ~ cult$l811pAF,ylab = "petal color",xlab="allele frequency coding SNP")
boxplot(cult$petalCol ~ cult$l019pAF,ylab = "petal color",xlab="allele frequency downstream SNP 1")
boxplot(cult$petalCol ~ cult$l056pAF,ylab = "petal color",xlab="allele frequency downstream SNP 2")
boxplot(cult$petalCol ~ cult$lsumpAF)
boxplot(cult$petalCol ~ cult$l811.056pAF,ylab = "petal color",xlab="allele frequency coding + downstream SNP")
#plot(lm(cult$petalCol ~ cult$l811.056pAF))

boxplot(culd$petalCol ~ culd$l811pAF,ylab = "petal color",xlab="allele frequency coding SNP")
boxplot(culd$petalCol ~ culd$l019pAF,ylab = "petal color",xlab="allele frequency downstream SNP")
boxplot(culd$petalCol ~ culd$l056pAF,ylab = "petal color",xlab="allele frequency downstream SNP")
boxplot(culd$petalCol ~ culd$lsumpAF,ylab = "petal color",xlab="allele frequency coding SNP")
boxplot(culd$petalCol ~ culd$l811.056pAF,ylab = "petal color",xlab="allele frequency  coding + downstream  SNP")

############### Seedling color #################
library(data.table)
cul<-fread("/home/aa/pigmentation/mappingExp/mapping/cultivation - overview9Feb.csv")
cul<-cul[,c(1:3,5,10:12)]
colnames(cul)<-c("indiv","line","seedlCol","petalCol","l811","l019","l056")
length(subset(cul,substr(cul$line,1,1) %in% "t")$line)
length(subset(cul,substr(cul$line,1,1) %in% "z")$line)

#cul1<-subset(cul, !cul$l811 %in% NA)
cul1<-subset(cul, !cul$l811 %in% "na")
cul1<-subset(cul1, !cul1$l019 %in% "na")
cul1<-subset(cul1, !cul1$l056 %in% "na")
cul1<-subset(cul1, !cul1$l056 %in% "")

length(subset(cul1,substr(cul1$line,1,1) %in% "t")$line)
length(subset(cul1,substr(cul1$line,1,1) %in% "z")$line)

cul1<-subset(cul1, !cul1$seedlCol %in% "na")
length(subset(cul1,substr(cul1$line,1,1) %in% "t")$line)
length(subset(cul1,substr(cul1$line,1,1) %in% "z")$line)

cul1$l811p <- str_count(cul1$l811, "p")
cul1$l019p <- str_count(cul1$l019, "p")
cul1$l056p <- str_count(cul1$l056, "p")
cul1$l811pAF<-""
cul1$l019pAF<-""
cul1$l056pAF<-""
# loop
for (i in 1:nrow(cul1)) { # i=1
  if (substr(cul1[i,2],1,1)== "t") {
    cul1$l811pAF[i]<- as.numeric(cul1$l811p[i])/4
    cul1$l019pAF[i]<- as.numeric(cul1$l019p[i])/4
    cul1$l056pAF[i]<- as.numeric(cul1$l056p[i])/4
  } else {
    cul1$l811pAF[i]<- as.numeric(cul1$l811p[i])/2
    cul1$l019pAF[i]<- as.numeric(cul1$l019p[i])/2
    cul1$l056pAF[i]<- as.numeric(cul1$l056p[i])/2
  }
}
cul1$lsumpAF<-as.numeric(cul1$l811pAF)+as.numeric(cul1$l019pAF)+as.numeric(cul1$l056pAF)
cul1$l811.056pAF<-as.numeric(cul1$l811pAF)+as.numeric(cul1$l056pAF)

cult<-subset(cul1,substr(cul1$line,1,1)== "t")
culd<-subset(cul1,substr(cul1$line,1,1)== "z")

boxplot(as.numeric(cul1$seedlCol) ~ cul1$l811pAF)
boxplot(as.numeric(cul1$seedlCol) ~ cul1$l019pAF)
boxplot(as.numeric(cul1$seedlCol) ~ cul1$l056pAF)
boxplot(as.numeric(cul1$seedlCol) ~ cul1$lsumpAF)
boxplot(as.numeric(cul1$seedlCol) ~ cul1$l811.056pAF,ylab = "seedling color",xlab="allele frequency  coding + downstream  SNP")

pdf("/home/aa/pigmentation/SI/figures/seedlingAssociationMapping.pdf")
boxplot(as.numeric(cult$seedlCol) ~ cult$l811pAF)
boxplot(as.numeric(cult$seedlCol) ~ cult$l019pAF)
boxplot(as.numeric(cult$seedlCol) ~ cult$l056pAF)
boxplot(as.numeric(cult$seedlCol) ~ cult$lsumpAF)
boxplot(as.numeric(cult$seedlCol) ~ cult$l811.056pAF,ylab = "seedling color",xlab="allele frequency  coding + downstream  SNP")
dev.off()

boxplot(as.numeric(culd$seedlCol) ~ culd$l811pAF)
boxplot(as.numeric(culd$seedlCol) ~ culd$l019pAF)
boxplot(as.numeric(culd$seedlCol) ~ culd$l056pAF)
boxplot(as.numeric(culd$seedlCol) ~ culd$lsumpAF)
boxplot(as.numeric(culd$seedlCol) ~ culd$l811.056pAF,ylab = "seedling color",xlab="allele frequency  coding + downstream  SNP")




### Proces maps
setwd("/home/aa/pigmentation/filedWork/2024mapping/coordinates/")

library(gpx)
one <- read_gpx("export.gpx")
one1<-one$waypoints
two <- read_gpx("export (1).gpx")
two1<-two$waypoints
thr <- read_gpx("export (2).gpx")
thr1<-thr$waypoints
all<-rbind(one1, two1, thr1)
write.table(all,"coord.txt", sep = "\t", col.names = F, quote = F)











##############################
############ RNA SEQ #########
##############################

# Install DESeq2 if not already installed
#if (!requireNamespace("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("DESeq2")
#install.packages("DBI")
# Load the necessary libraries
library(DESeq2)
library(ggplot2)
library("data.table")

setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/")

# Define function to read STAR count files (reverse strand data)
read_star_counts <- function(filepath) {
  data <- read.table(filepath, header = FALSE, skip = 4)
  counts <- data[, c(1, 4)]  # Extracting gene ID and reverse strand counts (column 4)
  colnames(counts) <- c("GeneID", sub("_ReadsPerGene.out.tab", "", basename(filepath)))
  return(counts)
}
# Define paths to all STAR output files
file_paths <- list.files("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/data/starOutput/", pattern = "_ReadsPerGene.out.tab$", full.names = TRUE)
# Read all count files
count_list <- lapply(file_paths, read_star_counts)
# Merge all count data into a single data frame
counts_data <- Reduce(function(x, y) merge(x, y, by = "GeneID"), count_list)
# Set rownames as Gene IDs and remove GeneID column
rownames(counts_data) <- counts_data$GeneID
counts_data <- counts_data[, -1]

# Load the metadata
metadata <- read.table("data/SampleMetadata.txt", header = TRUE, sep = "\t")
# Ensure that metadata sample IDs match count matrix column names
metadata <- metadata[match(colnames(counts_data), metadata$sampleID), ]
# Check if all samples in the metadata have corresponding counts
if (any(is.na(metadata$sampleID))) {
  stop("Some sample IDs in metadata do not match the read counts.")}

# Prepare for DESeq2 analysis
dds <- DESeqDataSetFromMatrix(countData = counts_data, colData = metadata, design = ~ color) #  design = ~ ploidy + ecotype + color

# Keep genes with at least some counts across all samples
hist(rowSums(counts(dds) == 0),breaks = 30)
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds)) >= 1200, ]
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds) <= 5) <= 6, ]
hist(log10(rowSums(counts(dds))),breaks = 100)


# Normalize counts
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized=TRUE)
hist(log10(rowSums(normalized_counts)),breaks = 100)

pink_samples <- which(metadata$color == "pink")
white_samples <- which(metadata$color == "white")
# Calculate variance for pink and white groups separately
pink_variances <- apply(normalized_counts[, pink_samples], 1, var)
white_variances <- apply(normalized_counts[, white_samples], 1, var)
hist(log10(pink_variances),breaks = 100)
hist(log10(white_variances),breaks = 100)
# Define threshold for filtering high-variance genes
variance_threshold <- quantile(c(pink_variances, white_variances), 0.95)
# Filter out genes with variance higher than the threshold in either group
high_var_genes <- (pink_variances > variance_threshold) | (white_variances > variance_threshold)
dds <- dds[!high_var_genes, ]
hist(log10(rowSums(counts(dds))),breaks = 100)

# Run the DESeq2 pipeline
dds <- DESeq(dds)
resultsNames(dds)

#resLFC <- lfcShrink(dds, coef="color_white_vs_pink", type="apeglm")
explan<-as.data.frame(mcols(mcols(dds), use.names=TRUE))
###### The end of the main DeSeq2 analysis 

# Plot PCA to check overall sample clustering
vsd <- vst(dds, blind = FALSE)
plotPCA(vsd, intgroup = "color")
# Nicer PCA
vsd <- vst(dds, blind = FALSE)
pcaData <- plotPCA(vsd, intgroup = "color", returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
# Add sample labels from the metadata
pcaData$sampleID <- metadata$sampleID  # Assuming metadata has a sampleID column
# Create a PCA plot with ggplot2, including sample labels
pdf("results/PCAbyColor4OctFilt.pdf", width = 8.5, height = 5)
ggplot(pcaData, aes(PC1, PC2, color = color, label = sampleID)) +
  geom_point(size = 5) +
  geom_text(vjust = 1.0, hjust = -0.3) +  # Add text labels
  scale_color_manual(values=c("pink"="magenta", "white"="grey70")) +  
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_minimal() +
  ggtitle("Gene expression in petals")
dev.off()

# Results with shrinkage
res_color <- lfcShrink(dds, coef="color_white_vs_pink", type="apeglm")

# Alternative: extract the results for your variable of interest (e.g., color)
#res_color <- results(dds, contrast = c("color", "pink", "white"))
# Order results by adjusted p-value
res_ordered <- res_color[order(res_color$padj), ]
# Show summary of significant results
summary(res_color)
sum(res_color$padj < 0.001, na.rm=TRUE)




###### Extract significant
# Extract significant DEGs (adjusted p-value < 0.05)
sig_DEGs <- subset(res_color, padj < 0.001)
sig_DEGs_df <- as.data.frame(sig_DEGs)
sig_DEGs_df$absLog2FC <- abs(sig_DEGs_df$log2FoldChange)
sig_DEGs_df<-sig_DEGs_df[order(sig_DEGs_df$absLog2FC,decreasing = T), ] ### REORDER HERE for plotting 
sig_DEGs_df$ID <- substr(row.names(sig_DEGs_df),1,9)
write.csv(sig_DEGs_df, file = "results/significantColor4OctFilt_0.001.csv", row.names = F)

# ### Or:
# # Order genes by adjusted p-value
# top_genes <- res_color[order(abs(res_color$log2FoldChange), decreasing = T), ][1:500, ]
# # Convert to a data frame for easier export
# top_genes_df <- as.data.frame(top_genes)
# top_genes_df$absLog2FC <- abs(top_genes_df$log2FoldChange)
# top_genes_df$ID <- substr(row.names(top_genes_df),1,9)
# write.csv(top_genes_df, file = "results/topLogFCColor_500.csv", row.names = F)

#### Annotation
# Load data
dict <- fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT <- substr(dict$AT, 1, 9)
ann <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR10orth_des_20150927.txt", h = T, quote = "")
ann2018 <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR11orth_des_20181231.txt", h = T, quote = "")

# Initialize an empty list to collect the rows
result_list <- list()
# Loop through sig_DEGs_df IDs
for (id in sig_DEGs_df$ID) { # id = "AL2G24170"
  # Find AT code from dict
  if (nrow(subset(dict, dict$AL == id)) > 0) {
    atcode <- subset(dict, dict$AL == id)$AT
    # Extract info from ann and ann2018
    an15 <- subset(ann, ann$`Version-2 A.t. Ortholog` == atcode)[, c(2,4,6,7)]
    an18 <- subset(ann2018, substr(ann2018$AT, 1, 9) == atcode)[1, 4:6]
    # If an15 or an18 are empty, replace with NA
    if (nrow(an15) == 0) {
      an15 <- data.table(`Version-2 A.t. Ortholog` = NA, `Gene Model Description` = NA, `Primary Gene Symbol` = NA, `All Gene Symbols` = NA)  # Adjust column names as per your ann file
    }
    if (nrow(an18) == 0) {
      an18 <- data.table(annShort = NA, annCurator = NA, annComputer = NA)  # Adjust column names as per your ann2018 file
    }
    # Merge info with sig_DEGs_df row
    result_list[[id]] <- cbind(sig_DEGs_df[sig_DEGs_df$ID == id, ], an15, an18)
  }
}
# Convert the list to a data frame
sig_DEGs_df_ann <- rbindlist(result_list, use.names = TRUE, fill = TRUE)

write.csv(sig_DEGs_df_ann, file = "results/significantColor4OctFilt_0.001_annotated.csv", row.names = F)


###### Visualize
# interesting interactive thing: not always needed
plotMA(res_color, ylim=c(-5.5,5.5))
#idx <- identify(res_color$baseMean, res_color$log2FoldChange)
#rownames(res_color)[idx]
#plotMA(resLFC, ylim=c(-8,15))




# candidate genes
plotCounts(dds, gene=which.min(res_color$padj), intgroup="color")
plotCounts(dds, gene="AL2G24200.v2.1", intgroup="color")


sig_DEGs_df_ann<-fread("results/significantColor4OctFilt_0.001_annotated.csv")

pdf("results/dotplot_significantColor4OctFilt_0.001_annotated.pdf", width = 4, height = 8)
for (id in sig_DEGs_df_ann$ID) {
  # Extract normalized counts for the specific gene
  gene_counts <- plotCounts(dds, gene=paste0(id,".v2.1"), intgroup="color", returnData=TRUE)
  # Check if gene_counts is valid
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    # Add the sample IDs from your metadata
    gene_counts$sampleID <- rownames(gene_counts)
    # Create a ggplot with sample IDs as labels
    p <- ggplot(gene_counts, aes(x=color, y=count, label=sampleID)) +
      geom_point(size=5, aes(color=color)) +  # Plot points
      geom_text(vjust=0.5, hjust=-0.5) +  # Add sample ID labels
      scale_color_manual(values=c("pink"="magenta", "white"="grey80")) +  
      labs(title=paste0(id," ",subset(sig_DEGs_df_ann, sig_DEGs_df_ann$ID == id)[,8][[1]]), # 9 if shrinkage not used
           x=sub(";.*", "", subset(sig_DEGs_df_ann, sig_DEGs_df_ann$ID == id)[,14][[1]]), # 15 if shrinkage not used
           y="Normalized Counts") +
      theme_minimal() #+ 
#      scale_y_log10()  # Optionally use log scale for better visualization
    # Print the plot
    print(p)
  }
}
dev.off()






##############################################
############# ALTERNATIVE WITH WILCOXON'S TEST
# Load the necessary libraries
library(DESeq2)
library(ggplot2)
library("data.table")

setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/")
# Define function to read STAR count files (reverse strand data)
read_star_counts <- function(filepath) {
  data <- read.table(filepath, header = FALSE, skip = 4)
  counts <- data[, c(1, 4)]  # Extracting gene ID and reverse strand counts (column 4)
  colnames(counts) <- c("GeneID", sub("_ReadsPerGene.out.tab", "", basename(filepath)))
  return(counts)
}
# Define paths to all STAR output files
file_paths <- list.files("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/data/starOutput/", pattern = "_ReadsPerGene.out.tab$", full.names = TRUE)
# Read all count files
count_list <- lapply(file_paths, read_star_counts)
# Merge all count data into a single data frame
counts_data <- Reduce(function(x, y) merge(x, y, by = "GeneID"), count_list)
# Set rownames as Gene IDs and remove GeneID column
rownames(counts_data) <- counts_data$GeneID
counts_data <- counts_data[, -1]

# Load the metadata
metadata <- read.table("data/SampleMetadata.txt", header = TRUE, sep = "\t")
# Ensure that metadata sample IDs match count matrix column names
metadata <- metadata[match(colnames(counts_data), metadata$sampleID), ]
# Check if all samples in the metadata have corresponding counts
if (any(is.na(metadata$sampleID))) {
  stop("Some sample IDs in metadata do not match the read counts.")
}

# Prepare for DESeq2 analysis
dds <- DESeqDataSetFromMatrix(countData = counts_data, colData = metadata, design = ~ color) #  design = ~ ploidy + ecotype + color

# Filter
# Keep genes with at least some counts across all samples
hist(rowSums(counts(dds) == 0),breaks = 30)
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds)) >= 240, ]
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds) <= 1) <= 15, ]
hist(log10(rowSums(counts(dds))),breaks = 100)

# Plot PCA to check overall sample clustering
vsd <- vst(dds, blind = FALSE)
plotPCA(vsd, intgroup = "color")
# Nicer PCA
vsd <- vst(dds, blind = FALSE)
pcaData <- plotPCA(vsd, intgroup = "color", returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
# Add sample labels from the metadata
pcaData$sampleID <- metadata$sampleID  # Assuming metadata has a sampleID column
# Create a PCA plot with ggplot2, including sample labels
pdf("results/PCAbyColor4OctFilt240_15.pdf", width = 8, height = 5)
ggplot(pcaData, aes(PC1, PC2, color = color, label = sampleID)) +
  geom_point(size = 5) +
  geom_text(vjust = 1.0, hjust = -0.3) +  # Add text labels
  scale_color_manual(values=c("pink"="magenta", "white"="grey70")) +  
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_minimal() +
  ggtitle("Gene expression in petals")
dev.off()

# Normalize counts
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized=TRUE)
## Optional: 
write.csv(normalized_counts,"data/normalizedCounts_filtered_240RC_15nonzero_7Oct.csv")
hist(log10(rowSums(normalized_counts)),breaks = 100)
pink_samples <- which(metadata$color == "pink")
white_samples <- which(metadata$color == "white")

### Rank-Based Testing Using Wilcoxon Rank-Sum Test ###
# Perform Wilcoxon rank-sum test for each gene
wilcox_test_results <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$p.value
})

wilcox_test_stat <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$statistic
})
# Adjust p-values using Benjamini-Hochberg correction for multiple testing
adjusted_pvals <- p.adjust(wilcox_test_results, method = "BH")
# Combine results into a data frame
wilcox_res <- data.frame(GeneID = substr(rownames(normalized_counts),1,9), pvalue = wilcox_test_results, padj = adjusted_pvals, statW = wilcox_test_stat)
# Filter for significant genes (adjusted p-value < 0.001)
sig_genes <- subset(wilcox_res, padj < 0.05)
sig_genes <- sig_genes[order(sig_genes$padj), ]
# Write significant genes to a file
write.csv(wilcox_res, file = "results/rankBasedColor_all_20601_4OctFilt.csv", row.names = FALSE)
write.csv(sig_genes, file = "results/rankBasedColor_p0.05_4OctFilt.csv", row.names = FALSE)

#### Annotation
# Load data
dict <- fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT <- substr(dict$AT, 1, 9)
ann <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR10orth_des_20150927.txt", h = T, quote = "")
ann2018 <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR11orth_des_20181231.txt", h = T, quote = "")

# Initialize an empty list to collect the rows
result_list <- list()
# Loop through sig_DEGs_df IDs
for (id in sig_genes$GeneID) { # id = "AL2G24170"
  # Find AT code from dict
  if (nrow(subset(dict, dict$AL == id)) > 0) {
    atcode <- subset(dict, dict$AL == id)$AT
    # Extract info from ann and ann2018
    an15 <- subset(ann, ann$`Version-2 A.t. Ortholog` == atcode)[, c(2,4,6,7)]
    an18 <- subset(ann2018, substr(ann2018$AT, 1, 9) == atcode)[1, 4:6]
    # If an15 or an18 are empty, replace with NA
    if (nrow(an15) == 0) {
      an15 <- data.table(`Version-2 A.t. Ortholog` = NA, `Gene Model Description` = NA, `Primary Gene Symbol` = NA, `All Gene Symbols` = NA)  # Adjust column names as per your ann file
    }
    if (nrow(an18) == 0) {
      an18 <- data.table(annShort = NA, annCurator = NA, annComputer = NA)  # Adjust column names as per your ann2018 file
    }
    # Merge info with sig_DEGs_df row
    result_list[[id]] <- cbind(sig_genes[sig_genes$GeneID == id, ], an15, an18)
  }
}
# Convert the list to a data frame
sig_DEGs_df_ann <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
rangeW<-mean(range(sig_DEGs_df_ann$statW))
sig_DEGs_df_ann$adjusted_W <- abs(sig_DEGs_df_ann$statW - rangeW)
sig_DEGs_df_ann <- sig_DEGs_df_ann[order(sig_DEGs_df_ann$adjusted_W,decreasing = T), ]

write.csv(sig_DEGs_df_ann, file = "results/rankBasedColor_p0.05_4OctFilt_annotated.csv", row.names = F)


###### Visualize
# candidate genes
sig_DEGs_df_ann<-fread("results/rankBasedColor_p0.05_4OctFilt_annotated.csv")

pdf("results/rankBasedColor_p0.05_4OctFilt_annotated.pdf", width = 4, height = 8)
for (id in sig_DEGs_df_ann$GeneID) {
  # Extract normalized counts for the specific gene
  gene_counts <- plotCounts(dds, gene=paste0(id,".v2.1"), intgroup="color", returnData=TRUE)
  # Check if gene_counts is valid
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    # Add the sample IDs from your metadata
    gene_counts$sampleID <- rownames(gene_counts)
    # Create a ggplot with sample IDs as labels
    p <- ggplot(gene_counts, aes(x=color, y=count, label=sampleID)) +
      geom_point(size=5, aes(color=color)) +  # Plot points
      geom_text(vjust=0.5, hjust=-0.5) +  # Add sample ID labels
      scale_color_manual(values=c("pink"="magenta", "white"="grey80")) +  
      labs(title=paste0(id," - ",subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,5][[1]]), # 9 if shrinkage not used
           x=sub(";.*", "", subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,11][[1]]), # 15 if shrinkage not used
           y="Normalized Counts") +
      theme_minimal() #+ 
    #      scale_y_log10()  # Optionally use log scale for better visualization
    # Print the plot
    print(p)
  }
}
dev.off()

############# Visualize for publication
######### For publication. first, get the dds object above, then run this 
############# Visualize for publication (Updated Style) #########
library(data.table)
library(ggplot2)
library(DESeq2)
# 1. Definice barev a tvarů pro jitter body (bodíky)
# 2x_white: šedé prázdné -> tvar 21 (obrys šedý, vnitřek bílý)
# 2x_pink: šedé plné   -> tvar 16, barva šedá
# 4x_white & 4x_pink: černé plné -> tvar 16, barva černá
kategorie_shapes <- c("2x_white" = 21, "2x_pink" = 16, "4x_white" = 16, "4x_pink" = 16)
kategorie_colors <- c("2x_white" = "grey50", "2x_pink" = "grey50", "4x_white" = "black", "4x_pink" = "black")

# 2. Definice výplní pro samotné BOXPLOTY
# Bílé kategorie budou bílé, růžové/pink dostanou transparentní fialovou
box_colors <- c("2x_white" = "white", "2x_pink" = "purple", "4x_white" = "white", "4x_pink" = "purple")

# Načtení genů pro publikaci
sig_DEGs_df_ann <- fread("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/stringTOM60genes.csv")[1:23,]

pdf("results/schematic_boxplots_flavonoidPathway_FINAL_jun26.pdf", width = 3, height = 6)

for (id in sig_DEGs_df_ann$GeneID) {
  gene_id <- paste0(id, ".v2.1")
  
  # Načtení dat z DESeq2 dds objektu
  gene_counts <- plotCounts(dds, gene = gene_id, intgroup = c("ploidy", "color"), returnData = TRUE)
  
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    # Vytvoření kombinované kategorie a nastavení pořadí
    gene_counts$kategorie <- paste(gene_counts$ploidy, gene_counts$color, sep = "_")
    cilove_poradi <- c("2x_white", "2x_pink", "4x_white", "4x_pink")
    gene_counts$kategorie <- factor(gene_counts$kategorie, levels = cilove_poradi)
    
    # Přepočet counts
    gene_counts$kcounts <- gene_counts$count / 1000
    
    # Vykreslení grafu
    p <- ggplot(gene_counts, aes(x = kategorie, y = kcounts)) +
      # Boxplot: fill mapujeme na kategorii, abychom mohli boxy obarvit různě
      geom_boxplot(aes(fill = kategorie), alpha = 0.4, color = "black", outlier.shape = NA) +
      
      # Jitter body: mapujeme tvar i barvu. 
      # Přidal jsem fill = "white", který se uplatní pouze u tvaru 21 (2x_white), aby byl uvnitř prázdný/bílý.
      geom_jitter(aes(shape = kategorie, color = kategorie), 
                  fill = "white", width = 0.2, size = 2.5, stroke = 1, alpha = 0.8) +
      
      # Manuální stupnice pro boxy a body
      scale_fill_manual(values = box_colors) +
      scale_shape_manual(values = kategorie_shapes) +
      scale_color_manual(values = kategorie_colors) +
      
      # Popisky a motiv
      labs(title = id, x = "Ploidy & Color Category", y = "Normalized Counts (x1000)") +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        axis.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 13),
        legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5, size = 14)
      )
    
    print(p)
  }
}
dev.off()














########################################## LEAF
library(tidyverse)
library(data.table)

# --- 1. Load Data ---
header <- read.csv("/home/aa/pigmentation/tkpAnalysis/PAP2leafheader.csv", row.names = 1)
counts <- read.csv("/home/aa/pigmentation/tkpAnalysis/PAP2leafcounts.csv", row.names = 1)

# --- 2. Data Transformation ---
# Transpose counts to have samples as rows
counts_long <- as.data.frame(t(counts))
counts_long$sample <- rownames(counts_long)

# Merge with header to get population names
# We use the 'V1' from counts (which became rownames) to match with 'sample' in header
merged_data <- counts_long %>%
  inner_join(header, by = "sample")

# Create Treatment column based on the suffix of the sample name (hl or lh)
merged_data <- merged_data %>%
  mutate(treatment = case_when(
    str_detect(sample, "hl$") ~ "hl",
    str_detect(sample, "lh$") ~ "lh",
    TRUE ~ "other"
  ))

# --- 3. Filtering and Ordering ---
# Target populations: SUB, VEL, HRA, TKO (stripping the "A." or "F." prefix if present)
# According to your header, popnames look like "F.SUB.", "A.VEL.", etc.
target_pops <- c("F.SUB.", "A.VEL.", "F.HRA.", "A.TKO.")

plot_data <- merged_data %>%
  filter(popname %in% target_pops) %>%
  filter(treatment %in% c("hl", "lh")) %>%
  mutate(popname = factor(popname, levels = target_pops))

colnames(plot_data)[1]<-"V1"
# --- 4. Visualization ---
# Using the purple boxplot style from previous tasks
# We will use the first gene/count column (V1) for the Y axis
p <- ggplot(plot_data, aes(x = popname, y = log10(as.numeric(V1)+1))) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +
  geom_jitter(aes(color = treatment), width = 0.2, size = 2, alpha = 0.8, stroke = 1) + 
  scale_color_manual(values = c("hl" = "blue3", "lh" = "gold")) + 
  labs(title = "PAP2 Leaf Read Counts",
       x = "Population",
       y = "Log10(Read counts)",
       color = "Treatment") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
q <- ggplot(plot_data, aes(x = popname, y = (as.numeric(V1)))) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +
  geom_jitter(aes(color = treatment), width = 0.2, size = 2, alpha = 0.8, stroke = 1) + 
  scale_color_manual(values = c("hl" = "blue3", "lh" = "gold")) + 
  labs(title = "PAP2 Leaf Read Counts",
       x = "Population",
       y = "Read counts",
       color = "Treatment") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )
# --- 5. Save Output ---
pdf("/home/aa/pigmentation/tkpAnalysis/PAP2_Leafcounts_boxplot.pdf", width = 8, height = 6)
print(p)
print(q)
dev.off()
# Print to console to verify
print(p)
print(q)

# --- 6.1. Prepare Covariates (Ploidy and Altitude) ---
# We define ploidy and altitude categories based on the population names
analysis_data <- plot_data %>%
  mutate(
    # Ensure V1 is numeric for statistical calculations
    V1_numeric = as.numeric(V1),
    log_V1 = log10(V1_numeric + 1),
    # Ploidy: SUB, VEL = Diploid (2x) | HRA, TKO = Tetraploid (4x)
    ploidy = case_when(
      popname %in% c("F.SUB.", "A.VEL.") ~ "2x",
      popname %in% c("F.HRA.", "A.TKO.") ~ "4x"
    ),
    ploidy = factor(ploidy, levels = c("2x", "4x")),
    # Altitude: VEL, TKO = Alpine/Mountain | SUB, HRA = Lowland
    altitude = case_when(
      popname %in% c("A.VEL.", "A.TKO.") ~ "alpine",
      popname %in% c("F.SUB.", "F.HRA.") ~ "foothill"
    ),
    altitude = factor(altitude, levels = c("foothill", "alpine")),
    # Ensure treatment is a factor
    treatment = factor(treatment, levels = c("hl", "lh"))
  )

# --- 6.2. Test 1: Differences between Treatments (accounting for Ploidy) ---
# Formula: log_V1 ~ treatment + ploidy
# This tests the effect of treatment while holding ploidy constant.
model_treatment <- lm(log_V1 ~ treatment + ploidy, data = analysis_data)
cat("\n==================================================================\n")
cat("TEST 1: Effect of Treatment (HL vs LH) accounting for Ploidy\n")
cat("==================================================================\n")
print(summary(model_treatment))
print(anova(model_treatment))
# --- 6.3. Test 2: Differences between Altitudes (accounting for Ploidy) ---
# Formula: log_V1 ~ altitude + ploidy
# This tests whether mountain populations differ from lowland ones, correcting for ploidy.
model_altitude <- lm(log_V1 ~ altitude + ploidy, data = analysis_data)
cat("\n==================================================================\n")
cat("TEST 2: Effect of Altitude (Alpine vs Lowland) accounting for Ploidy\n")
cat("==================================================================\n")
print(summary(model_altitude))
print(anova(model_altitude))
# --- 6.4. Optional Test 3: Full Factorial Model (Interaction) ---
# This checks if the treatment effect depends on the altitude or ploidy.
model_full <- lm(log_V1 ~ treatment * altitude + ploidy, data = analysis_data)
cat("\n==================================================================\n")
cat("OPTIONAL TEST 3: Full Model with Treatment x Altitude Interaction\n")
cat("==================================================================\n")
print(summary(model_full))
print(anova(model_full))







############# ECOTYPIC DIVERGENCE IN ZEP-SUB## FOR JACHYM
##############################################
############# ALTERNATIVE WITH WILCOXON'S TEST
# Load the necessary libraries
library(DESeq2)
library(ggplot2)
library("data.table")

setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/")
# Define function to read STAR count files (reverse strand data)
read_star_counts <- function(filepath) {
  data <- read.table(filepath, header = FALSE, skip = 4)
  counts <- data[, c(1, 4)]  # Extracting gene ID and reverse strand counts (column 4)
  colnames(counts) <- c("GeneID", sub("_ReadsPerGene.out.tab", "", basename(filepath)))
  return(counts)
}
# Define paths to all STAR output files
file_paths <- list.files("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/data/starOutput/", pattern = "_ReadsPerGene.out.tab$", full.names = TRUE)
# Read all count files
count_list <- lapply(file_paths, read_star_counts)
# Merge all count data into a single data frame
counts_data <- Reduce(function(x, y) merge(x, y, by = "GeneID"), count_list)
# Set rownames as Gene IDs and remove GeneID column
rownames(counts_data) <- counts_data$GeneID
counts_data <- counts_data[, -1]

# Load the metadata
metadata <- read.table("data/SampleMetadata.txt", header = TRUE, sep = "\t")
# Ensure that metadata sample IDs match count matrix column names
metadata <- metadata[match(colnames(counts_data), metadata$sampleID), ]
# Check if all samples in the metadata have corresponding counts
if (any(is.na(metadata$sampleID))) {
  stop("Some sample IDs in metadata do not match the read counts.")
}

### Subset only diploids
metadata <- metadata[metadata$ploidy == "2x", ]
# Subset the gene expression data to keep only columns corresponding to 2x individuals
counts_data <- counts_data[, colnames(counts_data) %in% metadata_subs$sampleID]

# Prepare for DESeq2 analysis
dds <- DESeqDataSetFromMatrix(countData = counts_data, colData = metadata, design = ~ color) #  design = ~ ploidy + ecotype + color

# Filter
# Keep genes with at least some counts across all samples
hist(rowSums(counts(dds) <= 1),breaks = 30)
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds)) >= 140, ]
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds) <= 1) <= 8, ]
hist(log10(rowSums(counts(dds))),breaks = 100)

# Plot PCA to check overall sample clustering
vsd <- vst(dds, blind = FALSE)
plotPCA(vsd, intgroup = "color")
# Nicer PCA
vsd <- vst(dds, blind = FALSE)
pcaData <- plotPCA(vsd, intgroup = "color", returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
# Add sample labels from the metadata
pcaData$sampleID <- metadata$sampleID  # Assuming metadata has a sampleID column
# Create a PCA plot with ggplot2, including sample labels
pdf("results/PCAbyColor_2x.pdf", width = 8, height = 5)
ggplot(pcaData, aes(PC1, PC2, color = color, label = sampleID)) +
  geom_point(size = 5) +
  geom_text(vjust = 1.0, hjust = -0.3) +  # Add text labels
  scale_color_manual(values=c("pink"="magenta", "white"="grey70")) +  
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_minimal() +
  ggtitle("Gene expression in petals")
dev.off()

# Normalize counts
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized=TRUE)
## Optional: 
write.csv(normalized_counts,"data/normalizedCounts_2x.csv")
hist(log10(rowSums(normalized_counts)),breaks = 100)
pink_samples <- which(metadata$color == "pink")
white_samples <- which(metadata$color == "white")

### Rank-Based Testing Using Wilcoxon Rank-Sum Test ###
# Perform Wilcoxon rank-sum test for each gene
wilcox_test_results <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$p.value
})

wilcox_test_stat <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$statistic
})
# Adjust p-values using Benjamini-Hochberg correction for multiple testing
adjusted_pvals <- p.adjust(wilcox_test_results, method = "BH")
# Combine results into a data frame
wilcox_res <- data.frame(GeneID = substr(rownames(normalized_counts),1,9), pvalue = wilcox_test_results, padj = adjusted_pvals, statW = wilcox_test_stat)
# Filter for significant genes (adjusted p-value < 0.001)
sig_genes <- subset(wilcox_res, padj < 0.05)
sig_genes <- sig_genes[order(sig_genes$padj), ]
# Write significant genes to a file
write.csv(sig_genes, file = "results/rankBasedColor_p0.01_2x.csv", row.names = FALSE)

#### Annotation
# Load data
dict <- fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT <- substr(dict$AT, 1, 9)
ann <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR10orth_des_20150927.txt", h = T, quote = "")
ann2018 <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR11orth_des_20181231.txt", h = T, quote = "")

# Initialize an empty list to collect the rows
result_list <- list()
# Loop through sig_DEGs_df IDs
for (id in sig_genes$GeneID) { # id = "AL2G24170"
  # Find AT code from dict
  if (nrow(subset(dict, dict$AL == id)) > 0) {
    atcode <- subset(dict, dict$AL == id)$AT
    # Extract info from ann and ann2018
    an15 <- subset(ann, ann$`Version-2 A.t. Ortholog` == atcode)[, c(2,4,6,7)]
    an18 <- subset(ann2018, substr(ann2018$AT, 1, 9) == atcode)[1, 4:6]
    # If an15 or an18 are empty, replace with NA
    if (nrow(an15) == 0) {
      an15 <- data.table(`Version-2 A.t. Ortholog` = NA, `Gene Model Description` = NA, `Primary Gene Symbol` = NA, `All Gene Symbols` = NA)  # Adjust column names as per your ann file
    }
    if (nrow(an18) == 0) {
      an18 <- data.table(annShort = NA, annCurator = NA, annComputer = NA)  # Adjust column names as per your ann2018 file
    }
    # Merge info with sig_DEGs_df row
    result_list[[id]] <- cbind(sig_genes[sig_genes$GeneID == id, ], an15, an18)
  }
}
# Convert the list to a data frame
sig_DEGs_df_ann <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
rangeW<-mean(range(sig_DEGs_df_ann$statW))
sig_DEGs_df_ann$adjusted_W <- abs(sig_DEGs_df_ann$statW - rangeW)
sig_DEGs_df_ann <- sig_DEGs_df_ann[order(sig_DEGs_df_ann$adjusted_W,decreasing = T), ]

write.csv(sig_DEGs_df_ann, file = "results/rankBasedColor_p0.05_2x_annotated.csv", row.names = F)


###### Visualize
# candidate genes
sig_DEGs_df_ann<-fread("results/rankBasedColor_p0.01_2x_annotated.csv")

pdf("results/rankBasedColor_p0.01_2x_annotated.pdf", width = 4, height = 8)
for (id in sig_DEGs_df_ann$GeneID) {
  # Extract normalized counts for the specific gene
  gene_counts <- plotCounts(dds, gene=paste0(id,".v2.1"), intgroup="color", returnData=TRUE)
  # Check if gene_counts is valid
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    # Add the sample IDs from your metadata
    gene_counts$sampleID <- rownames(gene_counts)
    # Create a ggplot with sample IDs as labels
    p <- ggplot(gene_counts, aes(x=color, y=count, label=sampleID)) +
      geom_point(size=5, aes(color=color)) +  # Plot points
      geom_text(vjust=0.5, hjust=-0.5) +  # Add sample ID labels
      scale_color_manual(values=c("pink"="magenta", "white"="grey80")) +  
      labs(title=paste0(id," - ",subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,5][[1]]), # 9 if shrinkage not used
           x=sub(";.*", "", subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,11][[1]]), # 15 if shrinkage not used
           y="Normalized Counts") +
      theme_minimal() #+ 
    #      scale_y_log10()  # Optionally use log scale for better visualization
    # Print the plot
    print(p)
  }
}
dev.off()





############# COLOR DIVERGENCE IN TKO ##
##############################################
############# ALTERNATIVE WITH WILCOXON'S TEST
# Load the necessary libraries
library(DESeq2)
library(ggplot2)
library("data.table")

setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/")
# Define function to read STAR count files (reverse strand data)
read_star_counts <- function(filepath) {
  data <- read.table(filepath, header = FALSE, skip = 4)
  counts <- data[, c(1, 4)]  # Extracting gene ID and reverse strand counts (column 4)
  colnames(counts) <- c("GeneID", sub("_ReadsPerGene.out.tab", "", basename(filepath)))
  return(counts)
}
# Define paths to all STAR output files
file_paths <- list.files("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/data/starOutput/", pattern = "_ReadsPerGene.out.tab$", full.names = TRUE)
# Read all count files
count_list <- lapply(file_paths, read_star_counts)
# Merge all count data into a single data frame
counts_data <- Reduce(function(x, y) merge(x, y, by = "GeneID"), count_list)
# Set rownames as Gene IDs and remove GeneID column
rownames(counts_data) <- counts_data$GeneID
counts_data <- counts_data[, -1]

# Load the metadata
metadata <- read.table("data/SampleMetadata.txt", header = TRUE, sep = "\t")
# Ensure that metadata sample IDs match count matrix column names
metadata <- metadata[match(colnames(counts_data), metadata$sampleID), ]
# Check if all samples in the metadata have corresponding counts
if (any(is.na(metadata$sampleID))) {
  stop("Some sample IDs in metadata do not match the read counts.")
}

### Subset only tetraploids
metadata <- metadata[metadata$ploidy == "4x", ]
# Subset the gene expression data to keep only columns corresponding to 2x individuals
counts_data <- counts_data[, colnames(counts_data) %in% metadata$sampleID]

# Prepare for DESeq2 analysis
dds <- DESeqDataSetFromMatrix(countData = counts_data, colData = metadata, design = ~ color) #  design = ~ ploidy + ecotype + color

# Filter
# Keep genes with at least some counts across all samples
hist(rowSums(counts(dds) <= 1),breaks = 30)
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds)) >= 100, ]
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds) <= 1) <= 6, ]
hist(log10(rowSums(counts(dds))),breaks = 100)

# Plot PCA to check overall sample clustering
vsd <- vst(dds, blind = FALSE)
plotPCA(vsd, intgroup = "color")
# Nicer PCA
vsd <- vst(dds, blind = FALSE)
pcaData <- plotPCA(vsd, intgroup = "color", returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
# Add sample labels from the metadata
pcaData$sampleID <- metadata$sampleID  # Assuming metadata has a sampleID column
# Create a PCA plot with ggplot2, including sample labels
pdf("results/PCAbyColor_4x.pdf", width = 8, height = 5)
ggplot(pcaData, aes(PC1, PC2, color = color, label = sampleID)) +
  geom_point(size = 5) +
  geom_text(vjust = 1.0, hjust = -0.3) +  # Add text labels
  scale_color_manual(values=c("pink"="magenta", "white"="grey70")) +  
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_minimal() +
  ggtitle("Gene expression in petals")
dev.off()

# Normalize counts
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized=TRUE)
## Optional: 
write.csv(normalized_counts,"data/normalizedCounts_4x.csv")
hist(log10(rowSums(normalized_counts)),breaks = 100)
pink_samples <- which(metadata$color == "pink")
white_samples <- which(metadata$color == "white")

### Rank-Based Testing Using Wilcoxon Rank-Sum Test ###
# Perform Wilcoxon rank-sum test for each gene
wilcox_test_results <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$p.value
})

wilcox_test_stat <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$statistic
})
# Adjust p-values using Benjamini-Hochberg correction for multiple testing
adjusted_pvals <- p.adjust(wilcox_test_results, method = "BH")
# Combine results into a data frame
wilcox_res <- data.frame(GeneID = substr(rownames(normalized_counts),1,9), pvalue = wilcox_test_results, padj = adjusted_pvals, statW = wilcox_test_stat)
# Filter for significant genes (adjusted p-value < 0.001)
sig_genes <- subset(wilcox_res, pvalue < 0.05)
sig_genes <- sig_genes[order(sig_genes$pvalue), ]
# Write significant genes to a file
write.csv(sig_genes, file = "results/rankBasedColor_pnonadj0.05_4x.csv", row.names = FALSE)

#### Annotation
# Load data
dict <- fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT <- substr(dict$AT, 1, 9)
ann <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR10orth_des_20150927.txt", h = T, quote = "")
ann2018 <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR11orth_des_20181231.txt", h = T, quote = "")

# Initialize an empty list to collect the rows
result_list <- list()
# Loop through sig_DEGs_df IDs
for (id in sig_genes$GeneID) { # id = "AL2G24170"
  # Find AT code from dict
  if (nrow(subset(dict, dict$AL == id)) > 0) {
    atcode <- subset(dict, dict$AL == id)$AT
    # Extract info from ann and ann2018
    an15 <- subset(ann, ann$`Version-2 A.t. Ortholog` == atcode)[, c(2,4,6,7)]
    an18 <- subset(ann2018, substr(ann2018$AT, 1, 9) == atcode)[1, 4:6]
    # If an15 or an18 are empty, replace with NA
    if (nrow(an15) == 0) {
      an15 <- data.table(`Version-2 A.t. Ortholog` = NA, `Gene Model Description` = NA, `Primary Gene Symbol` = NA, `All Gene Symbols` = NA)  # Adjust column names as per your ann file
    }
    if (nrow(an18) == 0) {
      an18 <- data.table(annShort = NA, annCurator = NA, annComputer = NA)  # Adjust column names as per your ann2018 file
    }
    # Merge info with sig_DEGs_df row
    result_list[[id]] <- cbind(sig_genes[sig_genes$GeneID == id, ], an15, an18)
  }
}
# Convert the list to a data frame
sig_DEGs_df_ann <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
rangeW<-mean(range(sig_DEGs_df_ann$statW))
sig_DEGs_df_ann$adjusted_W <- abs(sig_DEGs_df_ann$statW - rangeW)
sig_DEGs_df_ann <- sig_DEGs_df_ann[order(sig_DEGs_df_ann$adjusted_W,decreasing = T), ]

write.csv(sig_DEGs_df_ann, file = "results/rankBasedColor_pnonadj0.05_4x_annotated.csv", row.names = F)


###### Visualize
# candidate genes
sig_DEGs_df_ann<-fread("results/rankBasedColor_pnonadj0.05_4x_annotated.csv")

pdf("results/rankBasedColor_pnonadj0.05_4x_annotated.pdf", width = 4, height = 8)
for (id in sig_DEGs_df_ann$GeneID) {
  # Extract normalized counts for the specific gene
  gene_counts <- plotCounts(dds, gene=paste0(id,".v2.1"), intgroup="color", returnData=TRUE)
  # Check if gene_counts is valid
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    # Add the sample IDs from your metadata
    gene_counts$sampleID <- rownames(gene_counts)
    # Create a ggplot with sample IDs as labels
    p <- ggplot(gene_counts, aes(x=color, y=count, label=sampleID)) +
      geom_point(size=5, aes(color=color)) +  # Plot points
      geom_text(vjust=0.5, hjust=-0.5) +  # Add sample ID labels
      scale_color_manual(values=c("pink"="magenta", "white"="grey80")) +  
      labs(title=paste0(id," - ",subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,5][[1]]), # 9 if shrinkage not used
           x=sub(";.*", "", subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,11][[1]]), # 15 if shrinkage not used
           y="Normalized Counts") +
      theme_minimal() #+ 
    #      scale_y_log10()  # Optionally use log scale for better visualization
    # Print the plot
    print(p)
  }
}
dev.off()


######### For publication. first, get the dds object above, then run this #############
library(data.table)
library(ggplot2)
library(DESeq2)

sig_DEGs_df_ann <- fread("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/stringTOM60genes.csv")[1:23,]
pdf("results/schematic_boxplots_flavonoidPathway_FINAL_v4.pdf", width = 1.4, height = 1.6)

for (id in sig_DEGs_df_ann$GeneID) {
  gene_id <- paste0(id, ".v2.1")
  gene_counts <- plotCounts(dds, gene = gene_id, intgroup = "color", returnData = TRUE)
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    gene_counts$kcounts <- gene_counts$count / 1000
    ymin <- floor(min(gene_counts$kcounts))
    ymax <- ceiling(max(gene_counts$kcounts))
    
    p <- ggplot(gene_counts, aes(x = color, y = kcounts, fill = color)) +
      geom_boxplot(width = 0.8, outlier.shape = NA, 
                   color = "black", linewidth = 0.6) +
      geom_jitter(width = 0.1, size = 2, shape = 21, stroke = 0.2, fill = "black") +
      scale_fill_manual(values = c("white" = "white", "pink" = "#e3a8ff")) +
      scale_y_continuous(
        limits = c(ymin, ymax),
        breaks = c(ymin, ymax),
        labels = c(ymin, ymax),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      theme_void() +
      theme(
        axis.text.y = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(rep(0.1, 4), "cm"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12)
      ) +
      ggtitle(id)
    print(p)
  }
}
dev.off()


##############################################
############# PLOIDY DIFFERENTIATED
# Load the necessary libraries
library(DESeq2)
library(ggplot2)
library("data.table")

setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/")
# Define function to read STAR count files (reverse strand data)
read_star_counts <- function(filepath) {
  data <- read.table(filepath, header = FALSE, skip = 4)
  counts <- data[, c(1, 4)]  # Extracting gene ID and reverse strand counts (column 4)
  colnames(counts) <- c("GeneID", sub("_ReadsPerGene.out.tab", "", basename(filepath)))
  return(counts)
}
# Define paths to all STAR output files
file_paths <- list.files("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/data/starOutput/", pattern = "_ReadsPerGene.out.tab$", full.names = TRUE)
# Read all count files
count_list <- lapply(file_paths, read_star_counts)
# Merge all count data into a single data frame
counts_data <- Reduce(function(x, y) merge(x, y, by = "GeneID"), count_list)
# Set rownames as Gene IDs and remove GeneID column
rownames(counts_data) <- counts_data$GeneID
counts_data <- counts_data[, -1]

# Load the metadata
metadata <- read.table("data/SampleMetadata.txt", header = TRUE, sep = "\t")
# Ensure that metadata sample IDs match count matrix column names
metadata <- metadata[match(colnames(counts_data), metadata$sampleID), ]
# Check if all samples in the metadata have corresponding counts
if (any(is.na(metadata$sampleID))) {
  stop("Some sample IDs in metadata do not match the read counts.")
}

# Prepare for DESeq2 analysis
dds <- DESeqDataSetFromMatrix(countData = counts_data, colData = metadata, design = ~ ploidy) #  design = ~ ploidy + ecotype + color

# Filter
# Keep genes with at least some counts across all samples
hist(rowSums(counts(dds) == 0),breaks = 30)
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds)) >= 240, ]
hist(log10(rowSums(counts(dds))),breaks = 100)
dds <- dds[rowSums(counts(dds) <= 1) <= 15, ]
hist(log10(rowSums(counts(dds))),breaks = 100)

# Plot PCA to check overall sample clustering
vsd <- vst(dds, blind = FALSE)
plotPCA(vsd, intgroup = "ploidy")
# Nicer PCA
vsd <- vst(dds, blind = FALSE)
pcaData <- plotPCA(vsd, intgroup = "ploidy", returnData = TRUE)
percentVar <- round(100 * attr(pcaData, "percentVar"))
# Add sample labels from the metadata
pcaData$sampleID <- metadata$sampleID  # Assuming metadata has a sampleID column
# Create a PCA plot with ggplot2, including sample labels
pdf("results/PCAbyPloidy4OctFilt240_15.pdf", width = 8, height = 5)
ggplot(pcaData, aes(PC1, PC2, color = ploidy, label = sampleID)) +
  geom_point(size = 5) +
  geom_text(vjust = 1.0, hjust = -0.3) +  # Add text labels
  scale_color_manual(values=c("2x"="blue", "4x"="red")) +  
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  theme_minimal() +
  ggtitle("Gene expression in petals")
dev.off()

# Normalize counts
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized=TRUE)
hist(log10(rowSums(normalized_counts)),breaks = 100)
pink_samples <- which(metadata$ploidy == "4x")
white_samples <- which(metadata$ploidy == "2x")

### Rank-Based Testing Using Wilcoxon Rank-Sum Test ###
# Perform Wilcoxon rank-sum test for each gene
wilcox_test_results <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$p.value
})

wilcox_test_stat <- apply(normalized_counts, 1, function(gene_counts) {
  wilcox.test(gene_counts[pink_samples], gene_counts[white_samples])$statistic
})
# Adjust p-values using Benjamini-Hochberg correction for multiple testing
adjusted_pvals <- p.adjust(wilcox_test_results, method = "BH")
# Combine results into a data frame
wilcox_res <- data.frame(GeneID = substr(rownames(normalized_counts),1,9), pvalue = wilcox_test_results, padj = adjusted_pvals, statW = wilcox_test_stat)
# Filter for significant genes (adjusted p-value < 0.001)
sig_genes <- subset(wilcox_res, padj < 0.001)
sig_genes <- sig_genes[order(sig_genes$padj), ]
# Write significant genes to a file
write.csv(sig_genes, file = "results/rankBasedPloidy_p0.001_4OctFilt.csv", row.names = FALSE)

#### Annotation
# Load data
dict <- fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT <- substr(dict$AT, 1, 9)
ann <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR10orth_des_20150927.txt", h = T, quote = "")
ann2018 <- fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR11orth_des_20181231.txt", h = T, quote = "")

# Initialize an empty list to collect the rows
result_list <- list()
# Loop through sig_DEGs_df IDs
for (id in sig_genes$GeneID) { # id = "AL2G24170"
  # Find AT code from dict
  if (nrow(subset(dict, dict$AL == id)) > 0) {
    atcode <- subset(dict, dict$AL == id)$AT
    # Extract info from ann and ann2018
    an15 <- subset(ann, ann$`Version-2 A.t. Ortholog` == atcode)[, c(2,4,6,7)]
    an18 <- subset(ann2018, substr(ann2018$AT, 1, 9) == atcode)[1, 4:6]
    # If an15 or an18 are empty, replace with NA
    if (nrow(an15) == 0) {
      an15 <- data.table(`Version-2 A.t. Ortholog` = NA, `Gene Model Description` = NA, `Primary Gene Symbol` = NA, `All Gene Symbols` = NA)  # Adjust column names as per your ann file
    }
    if (nrow(an18) == 0) {
      an18 <- data.table(annShort = NA, annCurator = NA, annComputer = NA)  # Adjust column names as per your ann2018 file
    }
    # Merge info with sig_DEGs_df row
    result_list[[id]] <- cbind(sig_genes[sig_genes$GeneID == id, ], an15, an18)
  }
}
# Convert the list to a data frame
sig_DEGs_df_ann <- rbindlist(result_list, use.names = TRUE, fill = TRUE)
rangeW<-mean(range(sig_DEGs_df_ann$statW))
sig_DEGs_df_ann$adjusted_W <- abs(sig_DEGs_df_ann$statW - rangeW)
sig_DEGs_df_ann <- sig_DEGs_df_ann[order(sig_DEGs_df_ann$adjusted_W,decreasing = T), ]

write.csv(sig_DEGs_df_ann, file = "results/rankBasedPloidy_p0.001_4OctFilt_annotated.csv", row.names = F)


###### Visualize
# candidate genes
sig_DEGs_df_ann<-fread("results/rankBasedPloidy_p0.001_4OctFilt_annotated.csv")

pdf("results/rankBasedPloidy_p0.001_4OctFilt_annotated.pdf", width = 4, height = 8)
for (id in sig_DEGs_df_ann$GeneID) {
  # Extract normalized counts for the specific gene
  gene_counts <- plotCounts(dds, gene=paste0(id,".v2.1"), intgroup="ploidy", returnData=TRUE)
  # Check if gene_counts is valid
  if (!is.null(gene_counts) && nrow(gene_counts) > 0) {
    # Add the sample IDs from your metadata
    gene_counts$sampleID <- rownames(gene_counts)
    # Create a ggplot with sample IDs as labels
    p <- ggplot(gene_counts, aes(x=ploidy, y=count, label=sampleID)) +
      geom_point(size=5, aes(color=ploidy)) +  # Plot points
      geom_text(vjust=0.5, hjust=-0.5) +  # Add sample ID labels
      scale_color_manual(values=c("4x"="red", "2x"="blue")) +  
      labs(title=paste0(id," - ",subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,5][[1]]), # 9 if shrinkage not used
           x=sub(";.*", "", subset(sig_DEGs_df_ann, sig_DEGs_df_ann$GeneID == id)[,11][[1]]), # 15 if shrinkage not used
           y="Normalized Counts") +
      theme_minimal() #+ 
    #      scale_y_log10()  # Optionally use log scale for better visualization
    # Print the plot
    print(p)
  }
}
dev.off()



### Overlap with string
library(data.table)
setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/")
data<-fread("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/rankBasedColor4OctFilt_0.01_annotated.csv")
results<-fread("file:///home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/Merged_Gene_Table_35flavonoidsOrdered.csv")
merged_data <- merge(x = results, y = data, by = "AT")
fwrite(merged_data,"24flavonoids19MarFinal.csv")

######### PAP2 CML23 deregulation
library(data.table)
setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/")
metadata<-read.table("../data/SampleMetadata.txt",h=T)
dataExpr<-read.csv("../data/normalizedCounts_filtered_240RC_15nonzero_7Oct.csv")
rownames(dataExpr) <- dataExpr$X
dataExpr <- t(dataExpr[, -1])

metadata$sampleID <- paste0("X", as.numeric(metadata$sampleID))
# Reorder metadata based on the row names of dataExpr
metadata_reordered <- metadata[match(rownames(dataExpr), metadata$sampleID), ]
# Check if the reordering worked correctly
all.equal(rownames(dataExpr), metadata_reordered$sampleID)
# relate to colors
rownames(dataExpr)
metadata_reordered$color

pink_samples <- which(metadata_reordered$color == "pink")
white_samples <- which(metadata_reordered$color == "white")

dataPink<-dataExpr[pink_samples,]

cor.test(
  as.numeric(dataPink[, grepl("AL2G24170", colnames(dataPink))]),
  as.numeric(dataPink[, grepl("AL2G24180", colnames(dataPink))])
)


### White
dataPink<-dataExpr[white_samples,]

cor.test(
  as.numeric(dataPink[, grepl("AL2G24170", colnames(dataPink))]),
  as.numeric(dataPink[, grepl("AL2G24180", colnames(dataPink))])
)

# Extract numeric data based on row names containing "AL2G24170" and "AL2G24180"
x_values <- as.numeric(dataPink[, grepl("AL2G24170", colnames(dataPink))])
y_values <- as.numeric(dataPink[, grepl("AL2G24180", colnames(dataPink))])
labels <- rownames(dataPink)  

# Create a data frame for plotting
plot_data <- data.frame(x = x_values, y = y_values, labels = labels)

# Create the scatter plot with labeled points
ggplot(plot_data, aes(x = x, y = y, label = labels)) +
  geom_point() +  # Add points
  geom_text(vjust = -0.5, hjust = 1, size = 3) +  # Add labels
  labs(x = "AL2G24170", y = "AL2G24180", title = "Scatter Plot with Row Names") +
  theme_minimal()



######################################3
########## MERGE DATASETS + SEM #######
####################################
############## OPTIONAL Pripravit si gene list pro analyzu vsech flavonoidu
library(stringr)
library(data.table)
df <- fread("~/pigmentation/tkpAnalysis/petalRNASeq/112Phenylpropanoidstring_protein_annotations.tsv", header = T, stringsAsFactors = FALSE, sep = "\t")
df$AT_ID <- stringr::str_extract(df$other_names_and_aliases, "AT[1-5]G\\d{5}")
write.table(df,"/home/aa/pigmentation/tkpAnalysis/petalRNASeq/string60Genes.tsv",quote = F,row.names = F,sep = "\t")

dict<-fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/ALATdict.txt")
colnames(dict)<-c("Var1","AT_ID")
dict$AT_ID<-substr(dict$AT,1,9)
data<-merge(x = df,y = dict,by = "AT_ID")
genes<-unique(data$Var1)





library(tidyverse)
# Transkriptom (Read counts) - geny jsou v řádcích, vzorky ve sloupcích
counts <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/data/normalizedCounts_filtered_240RC_15nonzero_7Oct.csv", row.names = 1)
# Metadata a genotypy (24 jedinců)
metadata <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/cultivation - tkpMetadata.csv")
# Fenotyp: Flavonoly
flavonols <- read.csv("/home/aa/pigmentation/mappingExp/anthocyanins/flavonolAbsMay26.csv")
# Fenotyp: Antokyany a celkové spektrum (225 jedinců)
anthocyanins <- read.csv("/home/aa/pigmentation/SI/225petal_91leaf_spectrum_AFs_May26Final.csv")
anthocyanins<- read.csv("~/pigmentation/SI/225_IndividualsLeafPetalAnthocFlavonols.csv")

# Metadata: Převedeme "005" -> "5", "018" -> "18" atd., aby to odpovídalo ostatním tabulkám
metadata <- metadata %>%
  mutate(individual_id = as.character(as.numeric(sampleID)))

# Flavonoly: Přejmenujeme sampleN na individual_id pro snadný join
flavonols <- flavonols %>%
  mutate(individual_id = as.character(as.numeric(sampleN))) %>%
  select(individual_id, absorbance_360_per_mg, absorbance_360_per_g)

# Antokyany: Přejmenujeme sampleN na individual_id
anthocyanins <- anthocyanins %>%
  mutate(individual_id = as.character(as.numeric(sampleN)))

# Propojíme celou tabulku antokyanů s flavonolovou absorbancí podle ID jedince
si_table_all_225 <- anthocyanins %>%
  left_join(flavonols, by = "individual_id")
# Uložení do složky SI tabulek
#write.csv(si_table_all_225, file = "/home/aa/pigmentation/SI/225_IndividualsLeafPetalAnthocFlavonols.csv", row.names = FALSE)



# A) Transpozice expresní matice (chceme jedince v řádcích a geny ve sloupcích)
counts_transposed <- as.data.frame(t(counts))
counts_transposed$individual_id <- as.character(as.numeric(substr(rownames(counts_transposed),2,4)))

# B) Definice 32 specifických genů fenylpropanoidové dráhy pro selekci
target_genes <- c(
  "AL2G24170", "AL6G43450", "AL4G33640", "AL4G33370", "AL4G25370", 
  "AL2G19700", "AL6G24580", "AL6G15110", "AL5G31640", "AL6G18330", 
  "AL8G18230", "AL6G19370", "AL7G30640", "AL8G29370", "AL8G29160", 
  "AL7G42320", "AL4G27010", "AL3G35670", "AL1G13000", "AL1G13540", 
  "AL5G19690", "AL6G28270", "AL7G25360", "AL3G21670", "AL1G64610", 
  "AL7G16230", "AL5G34170", "AL4G12650", "AL7G14830", "AL4G36780", 
  "AL1G20780", "AL6G48240"
)

target_genes<-c('AL2G24170','AL6G43450','AL4G33640','AL4G33370','AL2G19700','AL6G24580','AL5G31640','AL6G18330','AL8G18230','AL7G30640','AL8G29370','AL8G29160','AL7G42320','AL4G27010','AL3G35670','AL1G13000','AL1G13540','AL5G19690','AL6G28270','AL3G21670','AL5G34170','AL4G12650') ## core22


# C) Propojení Metadat + Exprese vybraných genů + Fenotypů
rna_summary_table <- metadata %>%
  # Přidáme transponovanou expresi
  inner_join(counts_transposed, by = "individual_id") %>%
  # Přidáme antokyany z vyčištěné tabulky
  left_join(
    anthocyanins %>% select(individual_id, anthocyanin_concentration_mg_per_g_petal, freq_811),
    by = "individual_id"
  ) %>%
  # Přidáme flavonoly
  left_join(flavonols, by = "individual_id")

# D) KLÍČOVÝ KROK: Selekce metadat, fenotypů a POUZE vybraných 32 genů
# Nejprve definujeme sloupce s metadaty a fenotypy, které chceme stoprocentně nechat
metadata_columns <- c(
  "sampleID", "individual_id", "ploidy", "ecotype", "color", 
  "G_811", "G_019", "G_056", "volatiles", "seedLine", "PcolorScale",
  "anthocyanin_concentration_mg_per_g_petal","freq_811", 
  "absorbance_360_per_mg", "absorbance_360_per_g"
)

# Sloučíme seznam metadat a tvých vybraných genů
final_columns <- c(metadata_columns, paste0(target_genes,".v2.1"))

# Ofiltrujeme tabulku pouze na tyto požadované sloupce
rna_summary_table <- rna_summary_table %>%
  select(any_of(final_columns))

# Uložení hlavní souhrnné tabulky
write.csv(rna_summary_table, 
          file = "/home/aa/pigmentation/tkpAnalysis/petalRNASeq/RNASeq_22_individuals_master_table22AllPhenylpropanoid.csv", 
          row.names = FALSE)

write.csv(data, 
          file = "/home/aa/pigmentation/tkpAnalysis/petalRNASeq/100AllPhenylpropanoid.csv", 
          row.names = FALSE)
############ PCA, RDA, SEM
library(tidyverse)
library(vegan) # Klíčový balíček pro RDA (Redundancy Analysis)
# Načtení master tabulky s expresí a fenotypem
master_table <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/RNASeq_24_individuals_master_table22AllPhenylpropanoid.csv")

# Načtení tabulky s názvy genů pro přejmenování
gene_annotations <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/stringTOM60genes.csv")


# Seznam 32 genů fenylpropanoidové dráhy (přesně podle sloupců v master_table)
# target_genes <- c("AL2G24170", "AL6G43450", "AL4G33640", "AL4G33370", "AL4G25370", "AL2G19700", "AL6G24580", "AL6G15110", "AL5G31640", "AL6G18330",  "AL8G18230", "AL6G19370", "AL7G30640", "AL8G29370", "AL8G29160", "AL7G42320", "AL4G27010", "AL3G35670", "AL1G13000", "AL1G13540", "AL5G19690", "AL6G28270", "AL7G25360", "AL3G21670", "AL1G64610", "AL7G16230", "AL5G34170", "AL4G12650", "AL7G14830", "AL4G36780", "AL1G20780", "AL6G48240") ###32

target_genes<-c('AL2G24170','AL6G43450','AL4G33640','AL4G33370','AL2G19700','AL6G24580','AL5G31640','AL6G18330','AL8G18230','AL7G30640','AL8G29370','AL8G29160','AL7G42320','AL4G27010','AL3G35670','AL1G13000','AL1G13540','AL5G19690','AL6G28270','AL3G21670','AL5G34170','AL4G12650') ## core22

target_genes<-c('AL2G24170','AL6G43450','AL4G33640','AL4G33370','AL2G19700','AL6G24580','AL5G31640','AL6G18330','AL8G18230','AL7G30640','AL8G29370','AL8G29160','AL7G42320','AL1G13000','AL1G13540','AL5G19690','AL6G28270','AL3G21670','AL5G34170','AL4G12650') ## core20

target_genes<-c("AL2G24170","AL6G43450","AL4G33640","AL4G33370","AL2G19700","AL6G24580","AL5G31640","AL6G18330","AL7G30640","AL8G29370","AL8G29160","AL7G42320","AL1G13000","AL1G13540","AL5G19690","AL6G28270") ## core16

target_genes<-genes #85

# 1. Odstraníme přípony typu ".v2.1" z názvů sloupců v master_table,
# aby přesně odpovídaly tvému seznamu target_genes a tabulce anotací.
colnames(master_table) <- str_remove(colnames(master_table), "\\.v2\\.1$")
# 2. Nyní můžeme bezpečně vybrat matici 32 genů, protože názvy už dokonale sedí
expr_matrix <- master_table %>% select(any_of(target_genes))
# 3. Provedeme log-transformaci pro stabilizaci rozptylu (nahrazuje VST)
expr_log <- log10(expr_matrix + 1)
# 4. Příprava slovníku pro přejmenování: vyčistíme názvy z stringTOM60genes.csv
# Vezmeme pouze text před prvním lomítkem (např. "PAP2/MYB90" -> "PAP2")
gene_mapping <- gene_annotations %>%
  filter(GeneID %in% target_genes) %>%
  mutate(short_name = str_split_fixed(Gene, "/", 2)[, 1]) %>%
  select(GeneID, short_name)
# Vytvoříme pojmenovaný vektor pro funkci přejmenování
rename_vector <- gene_mapping$short_name
names(rename_vector) <- gene_mapping$GeneID
# 5. Přejmenujeme sloupce v log-transformované matici na krátké názvy.
# Pokud by nějaký gen v anotační tabulce chyběl, kód elegantně ponechá původní AL ID.
colnames(expr_log) <- ifelse(colnames(expr_log) %in% names(rename_vector),
                             rename_vector[colnames(expr_log)],
                             colnames(expr_log))
# Použijeme prda() bez environmentálních proměnných, což je matematicky čisté PCA
pca_result <- rda(expr_log, scale = TRUE)
cat("\n==================================================================\n")
cat("PCA: PROCENTO VYSVĚTLENÉ VARIABILITY (Variance Explained)\n")
cat("==================================================================\n")
print(summary(pca_result)$cont$importance[, 1:4]) # Vypíše PC1 až PC4

# Příprava dat pro ggplot vizualizaci PCA
pca_scores <- as.data.frame(scores(pca_result, choices = c(1, 2), display = "sites"))
pca_data <- cbind(master_table %>% select(individual_id, color, ploidy, freq_811), pca_scores)
# Graf 1: PCA - Distribuce jedinců podle celkové exprese dráhy
p_pca <- ggplot(pca_data, aes(x = PC1, y = PC2)) +
  geom_point(aes(color = color, shape = ploidy), size = 4, alpha = 0.8) +
  scale_color_manual(values = c("pink" = "magenta3", "white" = "grey60")) +
  labs(title = "PCA: Phenylpropanoid genes' expression",
       x = paste0("PC1 (", round(summary(pca_result)$cont$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$cont$importance[2,2]*100, 1), "%)"),
       color = "Phenotype", shape = "Ploidy") +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"))

# RDA otestuje, jak moc z té variability v genech dokáže vysvětlit PAP2 genotyp 
# (reprezentovaný jako frekvence alely p: freq_811), kontrolovaný na ploidii a ekotyp.

# Připravíme si matici prediktorů (nezávislých proměnných)
predictors <- master_table %>% 
  select(freq_811, ecotype) %>%
  mutate(ecotype = as.factor(ecotype))
predictors[18,1]<-0
# Spuštění RDA: Exprese ~ PAP2_genotyp + Ploidy + Ecotype
rda_model <- rda(expr_log ~ freq_811 + ecotype, data = predictors, scale = TRUE)

cat("\n==================================================================\n")
cat("RDA: STATISTICKÉ OVĚŘENÍ VLIVU PREDIKTORŮ (Permutation Test)\n")
cat("==================================================================\n")
# Celková významnost modelu
cat("Celkový model:\n")
print(anova.cca(rda_model, permutations = 999))
# Významnost jednotlivých proměnných (zda freq_811 vyjde signifikantně)
cat("\nJednotlivé proměnné:\n")
print(anova.cca(rda_model, by = "margin", permutations = 999))

# Extrakce koordinátů pro RDA graf (ggplot)
rda_sites <- as.data.frame(scores(rda_model, choices = c(1, 2), display = "sites"))
rda_species <- as.data.frame(scores(rda_model, choices = c(1, 2), display = "species"))

rda_plot_data <- cbind(master_table %>% select(individual_id, color, ploidy), rda_sites)

# Graf 2: RDA Biplot (Jedinci + Geny s vyčištěnými popisky)
p_rda <- ggplot() +
  # Body pro jedince
  geom_point(data = rda_plot_data, aes(x = RDA1, y = RDA2, color = color, shape = ploidy), size = 4, alpha = 0.7) +
  # Šipky/body pro geny (species loadings)
  geom_segment(data = rda_species, aes(x = 0, y = 0, xend = RDA1, yend = RDA2), 
               arrow = arrow(length = unit(0.2, "cm")), color = "darkgray", alpha = 0.6) +
  # ULTRA JEDNODUCHÉ POPISKY GENŮ (získané z krátkých názvů před lomítkem)
  geom_text(data = rda_species, aes(x = RDA1 * 1.1, y = RDA2 * 1.1, label = rownames(rda_species)), 
            size = 3.5, fontface = "bold", color = "black", check_overlap = TRUE) +
  scale_color_manual(values = c("pink" = "magenta3", "white" = "grey60")) +
  labs(title = "RDA: Gene Expression Constrained by PAP2 Genotype",
       x = paste0("RDA1 (", round(summary(rda_model)$cont$importance[2,1]*100, 1), "%)"),
       y = paste0("RDA2 (", round(summary(rda_model)$cont$importance[2,2]*100, 1), "%)"),
       color = "Phenotype", shape = "Ploidy") +
  theme_bw() +
  theme(panel.grid = element_blank(), plot.title = element_text(face = "bold"))

pdf("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/PCA_RDA_Pathway_Analysis_16genes.pdf", width = 16, height = 4)
print(p_pca)
print(p_rda)
dev.off()

# 1. Vytáhneme souřadnice jedinců (sites) pro první dvě osy z PCA a přejmenujeme je
pca_scores_to_export <- as.data.frame(scores(pca_result, choices = c(1, 2), display = "sites"))
colnames(pca_scores_to_export) <- c("PCA_PC1", "PCA_PC2")
# 2. Vytáhneme souřadnice jedinců (sites) pro první dvě osy z RDA a přejmenujeme je
rda_scores_to_export <- as.data.frame(scores(rda_model, choices = c(1, 2), display = "sites"))
colnames(rda_scores_to_export) <- c("RDA_RDA1", "RDA_RDA2")
# 3. Spojíme tyto nové sloupce dohromady s původní master_table (podle řádků)
master_table_with_ordinations <- cbind(
  master_table, 
  pca_scores_to_export, 
  rda_scores_to_export
)
# 4. Uložení výsledné rozšířené tabulky do tvé složky
write.csv(master_table_with_ordinations, 
          file = "/home/aa/pigmentation/tkpAnalysis/petalRNASeq/RNASeq_24_individuals_master_table_with_PCA_RDA_16genes.csv", 
          row.names = FALSE)


####################SEM
library(lavaan)
library(tidyverse)
# 1. Načtení dat
sem_data <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/RNASeq_24_individuals_master_table_with_PCA_RDA_16genes.csv")
# 2. PŘÍPRAVA DAT (Ošetření rozptylů pomocí log-transformace)
sem_data_clean <- sem_data %>%
  mutate(
    # Převedeme ecotype na dummy (0/1)
    ecotype_dummy = ifelse(ecotype == "A", 1, 0),
    # Logaritmujeme fenotypy (přičteme 1 pro jistotu proti nulám),
    # čímž srazíme hodnoty v tisících na hodnoty mezi 0 až 4.
    log_anthocyanin = log10(anthocyanin_concentration_mg_per_g_petal + 1),
    log_flavonols   = log10(absorbance_360_per_mg + 1)
  )

hist(sem_data$anthocyanin_concentration_mg_per_g_petal,breaks = 25)
hist(log10(sem_data$anthocyanin_concentration_mg_per_g_petal + 1),breaks = 25)
# 3. Definice SEM modelu s novými zlogaritmovanými proměnnými
sem_model_syntax <- '
  # Krok 1: Transkripční úroveň (regulace sítě)
  PCA_PC1 ~ a*freq_811 + ecotype_dummy
  # Krok 2: Metabolická úroveň (používáme stabilizované log proměnné)
  log_anthocyanin ~ c*PCA_PC1
  log_flavonols   ~ d*PCA_PC1
  # Reziduální korelace mezi oběma metabolity
  log_anthocyanin ~~ log_flavonols
  # Nepřímé efekty (Indirect effects)
  indirect_anthocyanin := a * c
  indirect_flavonols   := a * d
'


# 4. Spuštění modelu
# MLR estimator s robustními standardními chybami teď proběhne hladce
fit <- sem(sem_model_syntax, data = sem_data_clean, estimator = "MLR")

# 5. Výpis výsledků
cat("\n==================================================================\n")
cat("SEM VÝSLEDKY: Celková shoda modelu (Model Fit Indices)\n")
cat("==================================================================\n")
print(fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.robust", "srmr")))

cat("\n==================================================================\n")
cat("SEM VÝSLEDKY: Cesty a standardizované koeficienty (Path Coefficients)\n")
cat("==================================================================\n")
standardizedSolution(fit) %>% 
  filter(op %in% c("~", ":=", "~~")) %>% 
  select(lhs, op, rhs, est.std, pvalue) %>% 
  print(row.names = FALSE)

#################################
############## WCGNA ############
#################################
#install.packages("WGCNA")
setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/WCGNA/")
library(WGCNA)
library(dplyr)
library(tidyr)
library(readr)
library(pheatmap)

options(stringsAsFactors = FALSE);
allowWGCNAThreads() 

metadata<-read.table("../data/SampleMetadata.txt",h=T)
dataExpr<-read.csv("../data/normalizedCounts_filtered_240RC_15nonzero_7Oct.csv")
rownames(dataExpr) <- dataExpr$X
dataExpr <- dataExpr[, -1]

#### Optional, to subsample genes based on DE results
wilcox<-read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/rankBasedColor_all_20601_4OctFilt.csv",h=T)
wilcoxSubs<-subset(wilcox,wilcox$padj<0.1)
dataExpr<-subset(dataExpr, rownames(dataExpr) %in% paste0(wilcoxSubs$GeneID,".v2.1"))
#### The end of optional

dataExpr <- as.data.frame(t(dataExpr))
# My are already filtered, so just in case
# gsg <- goodSamplesGenes(dataExpr, verbose = 3)
# dataExpr <- dataExpr[gsg$goodSamples, gsg$goodGenes]

# Perform sample clustering to detect outliers and visualize them:
sampleTree <- hclust(dist(dataExpr), method = "average")
plot(sampleTree, main = "Sample Clustering", xlab="", sub="")


#### Optional
  # Choose a soft-thresholding power (e.g., 6) based on scale-free topology fit. 
  #This runs long and is only needed once. for my full data it's 8.
  powers <- c(1:30)
 # alternative from maize tutorial: powers = c(c(1:20), seq(from = 22, to=30, by=2))
  sft <- pickSoftThreshold(dataExpr, powerVector = powers, verbose = 5)
  power <- sft$powerEstimate
  # Vizualise and pick the threshold
  par(mfrow = c(1,2));
  cex1 = 0.9;
  plot(sft$fitIndices[, 1],
       -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
       xlab = "Soft Threshold (power)",
       ylab = "Scale Free Topology Model Fit, signed R^2",
       main = paste("Scale independence"))
  text(sft$fitIndices[, 1],
       -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
       labels = powers, cex = cex1, col = "red")
  abline(h = 0.90, col = "red")
  plot(sft$fitIndices[, 1],
       sft$fitIndices[, 5],
       xlab = "Soft Threshold (power)",
       ylab = "Mean Connectivity",
       type = "n",
       main = paste("Mean connectivity"))
  text(sft$fitIndices[, 1],
       sft$fitIndices[, 5],
       labels = powers,
       cex = cex1, col = "red")
  
######### The result was 8, no need to repeat ###############

# Construct the network and detect modules:
cor <- WGCNA::cor
net <- blockwiseModules(dataExpr, power = 8, TOMType = "unsigned",
                        minModuleSize = 20, maxBlockSize = 3000,
                        mergeCutHeight = 0.25, numericLabels = TRUE, 
                        pamRespectsDendro = FALSE, saveTOMs = TRUE, 
                        verbose = 3, loadTOM = F) # Or power = power, Try with min modulesize 20, reassign = 0.05, # IF TOMs GENERATED, LOAD TOM = T
cor<- stats::cor
sizeGrWindow(12, 9)
mergedColors = labels2colors(net$colors)
pdf(file = "module_tree_blockwise.pdf", width = 8, height = 6);
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
dev.off()

# First, ensure sample IDs in metadata are formatted similarly to the row names in dataExpr
metadata$sampleID <- paste0("X", as.numeric(metadata$sampleID))
# Reorder metadata based on the row names of dataExpr
metadata_reordered <- metadata[match(rownames(dataExpr), metadata$sampleID), ]
# Check if the reordering worked correctly
all.equal(rownames(dataExpr), metadata_reordered$sampleID)
# relate to colors
rownames(dataExpr)
metadata_reordered$color
# Ensure the categorical traits are converted to binary (dummy) variables
metadata_reordered_binary <- model.matrix(~ ploidy + ecotype + color, data = metadata_reordered)[, -1]
rownames(metadata_reordered_binary)<-metadata_reordered$sampleID

# Calculate correlations and p-values
MEs <- moduleEigengenes(dataExpr, net$colors)$eigengenes
correlationMatrix <- cor(MEs, metadata_reordered_binary, use = "pairwise.complete.obs")
correlationMatrixAbs<-as.data.frame(abs(correlationMatrix))
pValues <- corPvalueStudent(correlationMatrix, nrow(MEs))

# Using pheatmap to plot the correlation matrix with significance annotations
pdf("traitModuleCorAbs.pdf",height = 12,width = 6)
pheatmap(correlationMatrixAbs, cluster_rows = TRUE, cluster_cols = TRUE,display_numbers = signif(pValues, 2), number_format = "%.2f",main = "Module-Trait Relationships")
dev.off()
# Explore the results
net$colors[net$blockGenes[[1]]]
MEs
correlationMatrixAbs
pValues
colorModuleVector<-substr(rownames(subset(correlationMatrixAbs,correlationMatrixAbs$colorwhite > 0.7)),3,4)
colorModuleGenes <- net$colors[net$colors == colorModuleVector]
# Export correlations
module_df <- data.frame(
  gene_id = names(net$colors),
  modules = net$colors,
  colors = labels2colors(net$colors)
)
write.csv(module_df,"geneModules.csv")
write.csv(MEs,"modulesIndividuals.csv")
write.csv(cbind(correlationMatrix,pValues),"moduleTraitCorr.csv")

### Identify hub genes and prepare Cytoscape input
#modules_of_interest=colorModuleVector #OR: c("0","1","2")
genes_of_interest = module_df %>%
  subset(modules %in% colorModuleVector)
expr_of_interest = dataExpr[,genes_of_interest$gene_id]
expr_of_interest[1:5,1:5]
TOM = TOMsimilarityFromExpr(expr_of_interest,power = 8)
row.names(TOM) = colnames(expr_of_interest)
colnames(TOM) = colnames(expr_of_interest)
TOM[1:15,1:15]

#zoom into a candidate gene
geneCand<-TOM["AL2G24170.v2.1",]
geneCand<-geneCand[geneCand<1]
hist(geneCand,breaks = 100)
summary(geneCand)

geneCand<-TOM["AL7G25360.v2.1",]
geneCand<-geneCand[geneCand<1]
hist(geneCand,breaks = 100)
summary(geneCand)
geneCand1<-subset(geneCand,geneCand > 0.2)




### Filter network irrespective of gene of interest and find the most connected hub genes
# Set the threshold
threshold <- 0.11
# Filter the TOM matrix based on the threshold
filtered_edges <- as.data.frame(which(TOM > threshold, arr.ind = TRUE))
filtered_edges <- filtered_edges %>%
  mutate(
    correlation = TOM[cbind(row, col)],
    gene1 = row.names(TOM)[row],
    gene2 = colnames(TOM)[col]
  ) %>%
  select(-row, -col) %>%
  filter(gene1 != gene2) %>%
  mutate(
    module1 = module_df[gene1, ]$modules,
    module2 = module_df[gene2, ]$modules
  )
# Remove duplicate edges by ensuring each pair appears only once
filtered_edges <- filtered_edges %>%
  unique()

connectedness<-as.data.frame(table(filtered_edges$gene1))
hist(connectedness$Freq,breaks = 100)
hist(filtered_edges$correlation,breaks = 100)
summary(filtered_edges$correlation)
# Write the filtered edge list to a file
write_delim(filtered_edges, file = "filtered_edgelist_R2_0.11.tsv", delim = "\t")


##############The same but log transformed ######### WORKS BETTER
#################################
########## WGCNA ###############
#################################
setwd("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/WCGNA/")
library(WGCNA)
library(dplyr)
library(data.table)
library(pheatmap)
options(stringsAsFactors = FALSE)
allowWGCNAThreads()

# Load metadata and expression
metadata <- read.table("../data/SampleMetadata.txt", h = TRUE)
dataExpr <- read.csv("../data/normalizedCounts_filtered_240RC_15nonzero_7Oct.csv")
rownames(dataExpr) <- dataExpr$X
dataExpr <- dataExpr[, -1]

# Optional: filter to DE genes (padj < 0.1)
wilcox <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/rankBasedColor_all_20601_4OctFilt.csv", h = TRUE)
wilcoxSubs <- subset(wilcox, wilcox$padj < 0.05)
dataExpr <- subset(dataExpr, rownames(dataExpr) %in% paste0(c(wilcoxSubs$GeneID,"AL8G18230",'AL6G19370','AL4G25370'), ".v2.1"))

## alternative for candidate gene set:
# dataExpr <- subset(dataExpr, rownames(dataExpr) %in% paste0(c('AL2G24170','AL1G13000','AL5G19690','AL7G30640','AL1G13540','AL8G29160','AL7G42320','AL7G25360','AL6G43450','AL4G33370','AL6G28270','AL4G33640','AL1G20780','AL6G24580','AL8G29370','AL6G48240','AL7G47960','AL4G27010','AL6G24080','AL6G18330','AL2G19520','AL3G35670','AL6G41210','AL1G37490','AL8G41090','AL4G29100','AL2G19700','AL7G31440','AL6G53230','AL3G23970','AL4G47060','AL5G31640','AL8G18230'), ".v2.1"))


# Transpose and log-transform
dataExpr <- as.data.frame(t(dataExpr))
dataExpr <- log2(dataExpr + 1)

# Optional: sample clustering
sampleTree <- hclust(dist(dataExpr), method = "average")
plot(sampleTree, main = "Sample Clustering", xlab="", sub="")

cor <- WGCNA::cor
net <- blockwiseModules(dataExpr, power = 8, TOMType = "unsigned",
                        minModuleSize = 20, maxBlockSize = 3000,
                        mergeCutHeight = 0.25, numericLabels = TRUE, 
                        pamRespectsDendro = FALSE, saveTOMs = TRUE, 
                        verbose = 3, loadTOM = F)

mergedColors <- labels2colors(net$colors)
pdf("module_tree_blockwise.pdf", width = 8, height = 6)
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors", dendroLabels = FALSE, hang = 0.03)
dev.off()

# Traits and module correlations
metadata$sampleID <- paste0("X", as.numeric(metadata$sampleID))
metadata_reordered <- metadata[match(rownames(dataExpr), metadata$sampleID), ]
metadata_reordered_binary <- model.matrix(~ ploidy + ecotype + color, data = metadata_reordered)[, -1]
rownames(metadata_reordered_binary) <- metadata_reordered$sampleID

MEs <- moduleEigengenes(dataExpr, net$colors)$eigengenes
correlationMatrix <- cor(MEs, metadata_reordered_binary, use = "pairwise.complete.obs")
correlationMatrixAbs <- abs(correlationMatrix)
pValues <- corPvalueStudent(correlationMatrix, nrow(MEs))

pdf("traitModuleCorAbs.pdf", height = 12, width = 6)
pheatmap(correlationMatrixAbs, cluster_rows = TRUE, cluster_cols = TRUE,
         display_numbers = signif(pValues, 2), number_format = "%.2f",
         main = "Module-Trait Relationships")
dev.off()

# Extract module and expression info
correlationMatrixAbs<-as.data.frame(correlationMatrixAbs)
colorModuleVector <- substr(rownames(subset(correlationMatrixAbs, correlationMatrixAbs$colorwhite > 0.3)), 3, 4)
module_df <- data.frame(
  gene_id = names(net$colors),
  modules = net$colors,
  colors = labels2colors(net$colors)
)
write.csv(module_df, "geneModules.csv")
write.csv(MEs, "modulesIndividuals.csv")
write.csv(cbind(correlationMatrix, pValues), "moduleTraitCorr.csv")

# Subset TOM for genes of interest
genes_of_interest <- module_df %>% filter(modules %in% colorModuleVector)
expr_of_interest <- dataExpr[, genes_of_interest$gene_id]
TOM <- TOMsimilarityFromExpr(expr_of_interest, power = 8)
rownames(TOM) <- colnames(expr_of_interest)
colnames(TOM) <- colnames(expr_of_interest)

geneCand <- TOM["AL2G24170.v2.1",]
TOM["AL2G24170.v2.1","AL4G25370.v2.1"]
TOM["AL2G24170.v2.1","AL6G19370.v2.1"]


geneCand <- geneCand[geneCand < 1]
hist(geneCand,breaks = 100) # this value is not correlation but rather neighborhood similarity. It can be much lower..
quantile(geneCand,0.9) # a lot of random posibilities for filtration criteria
threshold <- 0.075 # same here..
coexpressed_genes <- names(geneCand[geneCand > threshold])
selected_genes <- c("AL2G24170.v2.1", coexpressed_genes)
TOM_sub <- TOM[selected_genes, selected_genes]

# Network visualization
library(igraph)
TOM_sub[TOM_sub < threshold] <- 0
graph_obj <- graph_from_adjacency_matrix(TOM_sub, mode = "undirected", weighted = TRUE, diag = FALSE)
module_colors <- module_df$colors[match(V(graph_obj)$name, module_df$gene_id)]
V(graph_obj)$color <- module_colors

pdf("coexpressedWithPAP2.pdf", width = 14, height = 14)
plot(graph_obj, vertex.label = substr(V(graph_obj)$name, 1, 9),
     vertex.size = 5, edge.width = E(graph_obj)$weight * 10,
     layout = layout_with_fr)
dev.off()

# Annotate pre-selected PAP2-co-expressed genes
# Load ortholog dictionary
dict <- fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT <- substr(dict$AT, 1, 9)
# Get unique genes from TOM_sub
genes_to_annotate <- substr(colnames(TOM_sub), 1, 9)
genes_to_annotate <- unique(genes_to_annotate)

annotation_list <- list()
pap2_id <- "AL2G24170.v2.1"
for (id in genes_to_annotate) {
  full_id <- colnames(TOM_sub)[substr(colnames(TOM_sub), 1, 9) == id][1]
  tom_value <- if (!is.na(full_id) && full_id != pap2_id) TOM_sub[pap2_id, full_id] else NA
  match_row <- dict[AL == id]
  if (nrow(match_row) > 0) {
    atcode <- match_row$AT[1]
    annotation_list[[id]] <- data.table(gene_id = id, TAIR_ID = atcode, TOM_with_PAP2 = tom_value)
  } else {
    annotation_list[[id]] <- data.table(gene_id = id, TAIR_ID = NA, TOM_with_PAP2 = tom_value)
  }
}

# Combine and save
TOM_annotation <- rbindlist(annotation_list, use.names = TRUE, fill = TRUE)
write.csv(TOM_annotation, "annotated_TOM_sub_geneslogtr.csv", row.names = FALSE)

#### Add PAP2-TOM to the table of DEGs
tab<-fread("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/results/rankBasedColor4OctFilt_0.01_annotated.csv")
pap2_id <- "AL2G24170.v2.1"
tab$TOM_with_PAP2 <- NA

for (i in 1:nrow(tab)) { #  i=1
  gene_id <- paste0(tab$GeneID[i],".v2.1")
  if (gene_id %in% colnames(TOM) && pap2_id %in% rownames(TOM)) {
    tab$TOM_with_PAP2[i] <- TOM[pap2_id, gene_id]
  }
}
fwrite(tab, "rankBasedColor4OctFilt_0.01_annotated_PAP2TOM.csv")
tab1<-subset(tab, tab$TOM_with_PAP2!=1)
plot(tab1$adjusted_W~tab1$TOM_with_PAP2)
############### THE END
library(stringr)
df <- read.table("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/string_protein_annotations.tsv", header = T, stringsAsFactors = FALSE, sep = "\t")
df$AT_ID <- stringr::str_extract(df$other_names_and_aliases, "AT[1-5]G\\d{5}")
write.table(df,"/home/aa/pigmentation/tkpAnalysis/petalRNASeq/string60Genes.tsv",quote = F,row.names = F,sep = "\t")


df1 <- read.csv("/home/aa/pigmentation/tkpAnalysis/petalRNASeq/string60Genes.csv", stringsAsFactors = FALSE,sep= "\t",header = T)     # obsahuje sloupec AT_ID
df2 <- read.csv("file:///home/aa/pigmentation/tkpAnalysis/petalRNASeq/WCGNA/rankBasedColor4OctFilt_0.01_annotated_PAP2TOM.csv", stringsAsFactors = FALSE)       # obsahuje sloupec AT
merged_df <- merge(df2, df1, by.x = "AT", by.y = "AT_ID", all.x = TRUE)
write.csv(merged_df, "/home/aa/pigmentation/tkpAnalysis/petalRNASeq/stringTOM60genes.csv", row.names = FALSE)


########################

#Pokus 1
edge_list = data.frame(TOM) %>%
  mutate(
    gene1 = row.names(.)
  ) %>%
  pivot_longer(-gene1) %>%
  dplyr::rename(gene2 = name, correlation = value) %>%
  unique() %>%
  subset(!(gene1==gene2)) %>%
  mutate(
    module1 = module_df[gene1,]$modules,
    module2 = module_df[gene2,]$modules
  )
head(edge_list)
edge_list<-subset(edge_list,edge_list$correlation > 0.1)
cbind(as.data.frame(table(edge_list$gene1)),as.data.frame(table(edge_list$gene2)))
hist(as.data.frame(table(edge_list$gene1))$Freq,breaks = 50)
write_delim(edge_list,
            file = "edgelist_cor0.1.tsv",
            delim = "\t")


# Create edge list data frame





# Save edge list to CSV for Cytoscape
write.csv(edge_list, file = "module_3_edge_list.csv", row.names = FALSE)















# Find genes in module "3"
module_3_genes <- names(dataExpr)[module_colors == 3]
# Identify which block contains genes in module "3"
block_containing_module <- sapply(1:length(net$blockGenes), function(blockNum) {
  any(module_3_genes %in% names(dataExpr)[net$blockGenes[[blockNum]]])
})
# Output block numbers containing module "3"
block_numbers <- which(block_containing_module)
block_numbers


### Solution 1
# Load the appropriate TOM block for the module
# Ensure you are working with the right block if using multiple TOM files
blockNumber <- 1  # Replace with appropriate block number for module "3"
for (blockNumber in 1:5) {
load(paste0("blockwiseTOM-block.", blockNumber, ".RData"))} # Load the TOM file for the correct block

# Identify genes in module "3"
module_genes <- names(dataExpr)[net$colors == 3]
selectGenes <- net$colors == 3

# Subset TOM for module genes
TOM <- as.matrix(TOM)
module_TOM <- TOM[selectGenes, selectGenes]
module_genes <- module_genes[selectGenes]

# Filter for high similarity edges (e.g., TOM > 0.1)
threshold <- 0.1
edges <- which(module_TOM > threshold, arr.ind = TRUE)

# Create edge list data frame
edge_list <- data.frame(
  from = module_genes[edges[, 1]],
  to = module_genes[edges[, 2]],
  weight = module_TOM[edges]
)

# Save edge list to CSV for Cytoscape
write.csv(edge_list, file = "module_3_edge_list.csv", row.names = FALSE)











# Extract the genes in the module of interest (module 3)
module_genes <- names(net$colors)[net$colors == 3]
# Load the blockwise TOM files (for example, block 1)
load("blockwiseTOM-block.1.RData")
load("blockwiseTOM-block.3.RData")
# For each block, repeat the process

# Recalculate TOM for the selected module genes
TOM <- TOMsimilarityFromExpr(dataExpr[, module_genes], power = 8)  # Use the same power as in blockwiseModules

# Set a threshold for network connectivity (adjust as needed)
TOM_threshold <- 0.1  # Only keep connections above this threshold

# Create an edge list based on the TOM
edge_list <- data.frame()
for (i in 1:(nrow(TOM)-1)) {
  for (j in (i+1):ncol(TOM)) {
    if (TOM[i, j] > TOM_threshold) {
      edge_list <- rbind(edge_list, data.frame(from = rownames(TOM)[i], 
                                               to = colnames(TOM)[j], 
                                               weight = TOM[i, j]))}}}

# Create a data frame with node (gene) information
node_attributes <- data.frame(
  gene = module_genes,
  module = rep(3, length(module_genes))  # All belong to module 3
)

# Optionally, add additional attributes, e.g., gene expression values
# For instance, adding eigengene values (can modify for specific traits):
module_eigengenes <- MEs[, "ME3"]
node_attributes$eigengene_value <- module_eigengenes


# Export the edge list to a CSV file for Cytoscape
write.csv(edge_list, file = "cytoscape_edge_list.csv", row.names = FALSE)

# Export the node attributes to a CSV file for Cytoscape
write.csv(node_attributes, file = "cytoscape_node_attributes.csv", row.names = FALSE)

#














pdf("results/dotplot_significantColor_0.001_annotated.pdf", width = 4,height = 8)
for (id in sig_DEGs_df_ann$ID) { # id = 'AL6G33650'
# Nicer plot with ggplot2
# Extract normalized counts for the specific gene
gene_counts <- plotCounts(dds, gene=paste0(id,".v2.1"), intgroup="color", returnData=TRUE)
# Add the sample IDs from your metadata
gene_counts$sampleID <- rownames(gene_counts)
# Create a ggplot with sample IDs as labels
ggplot(gene_counts, aes(x=color, y=count, label=sampleID)) +
  geom_point(size=5, aes(color=color)) +  # Plot points
  geom_text(vjust=0.5, hjust=-0.5) +  # Add sample ID labels
  scale_color_manual(values=c("pink"="magenta", "white"="grey80")) +  
  labs(title=paste0(id," ",subset(sig_DEGs_df_ann,sig_DEGs_df_ann$ID == id)[,9][[1]]),
       x=sub(";.*", "", subset(sig_DEGs_df_ann,sig_DEGs_df_ann$ID == id)[,15][[1]]),
       y="Normalized Counts") +
  theme_minimal() + 
  scale_y_log10()  # Optionally use log scale for better visualization
}
dev.off()









### TODO wisualize within a loop :)

















library("data.table")
dict<-fread("/home/aa/Desktop/references/lyrataV2/functions/ALATdict.txt")
dict$AT<-substr(dict$AT,1,9)
ann<-fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR10orth_des_20150927.txt",h=T,quote="")
ann2018<-fread("/home/aa/Desktop/references/lyrataV2/LyV2_TAIR11orth_des_20181231.txt",h=T,quote="")

for (id in sig_DEGs_df$ID) { # id = "AL2G24170"
  if (nrow(subset(dict,dict$AL == id))>0) {
    atcode<-subset(dict,dict$AL == id)$AT
    an15<-subset(ann,ann$`Version-2 A.t. Ortholog` == atcode)[,c(2,4,6,7)]
    an18<-subset(ann2018,substr(ann2018$AT,1,9) == atcode)[1,4:6]
    
  }
}



sumPar<-matrix(nrow = nrow(top_genes_df),ncol = 8,dimnames = list(c(),c("AL","AT","lineages","T15_name","T15_descr","T18_descrShort","T18_descrCurat","T18_descrComp")))
for (i in 1:nrow(top_genes_df)) { # i=1
  g<-unlist(top_genes_df[i,1])
  ann1<-subset(ann, ann$`Version-2` %in% g)
  ann2018_1<-subset(ann2018, ann2018$AL %in% g)
  pair<-droplevels(subset(s11,s11$Elements %like% g))
  pairc<-paste(pair$Intersections, sep="", collapse=', ')
  sumPar[i,1]<-unlist(top_genes_df[i,1])
  sumPar[i,2]<-unlist(top_genes_df[i,2])
  sumPar[i,3]<-pairc
  sumPar[i,4]<-unlist(ann1[1,7])
  sumPar[i,5]<-unlist(ann1[1,4])
  sumPar[i,6]<-unlist(ann2018_1[1,4])
  sumPar[i,7]<-unlist(ann2018_1[1,5])
  sumPar[i,8]<-unlist(ann2018_1[1,6])
}


# Export top 50 genes to a CSV file
write.csv(top_genes_df, file = "top500_genes_info.csv", row.names = TRUE)



# Heatmap of the top 50 DEGs
top50_genes <- head(order(res_color$padj), 50)
library(pheatmap)
pheatmap(assay(vsd)[top50_genes,], cluster_rows=TRUE, cluster_cols=TRUE)










# Combine outlier and top 50 genes information
combined_genes <- unique(c(rownames(outlier_info), rownames(top50_genes_df)))
combined_genes_info <- res[combined_genes, ]
combined_genes_info_df <- as.data.frame(combined_genes_info)

# Export combined genes to a CSV file
write.csv(combined_genes_info_df, file = "combined_genes_info.csv", row.names = TRUE)









# Create metadata for the 24 samples
sample_info <- data.frame(
  sampleID = colnames(counts_data),
  color = rep(c("pink", "white"), each = 9),
  ploidy = c(rep("diploid", 14), rep("tetraploid", 4)),
  ecotype = c(rep("alpine", 17), rep("foothill", 1))
)
# Ensure rownames of metadata match with colnames of counts_data
rownames(sample_info) <- sample_info$sampleID



########## COMBINE COLOR and ENVIRONMENTS
setwd("/home/aa/pigmentation/envCorr/")
# Load required libraries
library(dplyr)
library(readr)
library(tidyr)
library(corrr)

# Load the datasets
env_data <- read.delim("/home/aa/BetyProject/envAssoc/Data/clim_total_results_2.5ByMonth_8Apr24.csv",header = T,sep = " ")
pop_data <- read.delim("/home/aa/pigmentation/GWAScolor/popInfo19JunFinalColor.txt", header = TRUE, sep = "\t")

# Check the structure of datasets
str(env_data)
str(pop_data)

# Merge datasets by the 'pop' column (adjust column names if necessary)
merged_data <- merge(pop_data, env_data, by = "pop")

# Select only numerical environmental variables and `colProp`
# Assuming `colProp` is numeric, ensure it is numeric in case it's read differently
merged_data$colProp <- as.numeric(merged_data$colProp)

# Check for numeric columns in environmental variables
env_vars <- merged_data %>% select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(env_vars, use = "pairwise.complete.obs")

# Extract correlations with colProp
cor_with_colProp <- cor_matrix[, "colProp"]
cor_with_colProp <- cor_matrix[, "colComp"]

# Sort correlations in descending order
sorted_correlations <- sort(abs(cor_with_colProp), decreasing = TRUE)

# Display the variable with the highest correlation
print(sorted_correlations)

# Visualize the strongest correlation
library(ggplot2)
pdf("correlationsColorEnv.pdf")
top_var <- names(sorted_correlations)[3] # The first one is 'colProp' itself
ggplot(merged_data, aes_string(x = top_var, y = "colComp", label = "pop")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + # Adjust text position and size
  labs(title = paste("Correlation between", top_var, "and colComp"),
       x = top_var,
       y = "colComp") +
  theme_minimal()

top_var <- names(sorted_correlations)[13] # The first one is 'colProp' itself
ggplot(merged_data, aes_string(x = top_var, y = "colComp", label = "pop")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + # Adjust text position and size
  labs(title = paste("Correlation between", top_var, "and colComp"),
       x = top_var,
       y = "colComp") +
  theme_minimal()


top_var <- names(sorted_correlations)[6] # The first one is 'colProp' itself
ggplot(merged_data, aes_string(x = top_var, y = "colComp", label = "pop")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + # Adjust text position and size
  labs(title = paste("Correlation between", top_var, "and colComp"),
       x = top_var,
       y = "colComp") +
  theme_minimal()

top_var <- names(sorted_correlations)[8] # The first one is 'colProp' itself
ggplot(merged_data, aes_string(x = top_var, y = "colComp", label = "pop")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) + # Adjust text position and size
  labs(title = paste("Correlation between", top_var, "and colComp"),
       x = top_var,
       y = "colComp") +
  theme_minimal()

dev.off()

cor.test(y = merged_data$colComp, x = merged_data$BIO7)



############### CALCULATE THE ANTHOCYANIN CONCENTRATION ###########
#################### Field work

setwd("/home/aa/pigmentation/fieldWork/")
# Load necessary libraries
library(readxl)
library(dplyr)
library(data.table)

# Load the data
cultivation <- read.csv("/home/aa/pigmentation/fieldWork/cultivation - fieldInfo.csv")
filedwork_spectrum <- read_excel("/home/aa/pigmentation/fieldWork/filedworkSpectrum.xlsx", sheet = 2)

# Filter rows with odd `Sample` names (A1, A3, ..., B1, B3, ...)
# Use grepl to match samples ending in odd numbers
sample_data <- filedwork_spectrum %>%
  filter(grepl("[13579]$", Sample))

# Compute anthocyanin absorbance (A530 - A660)
sample_data$anthocyanin_absorbance <- sample_data$`530` - sample_data$`660`

# Turn negative absorbance values into 0
sample_data$anthocyanin_absorbance[sample_data$anthocyanin_absorbance < 0] <- 0

# Extract standard absorbance and concentrations (last two rows)
standard_data <- filedwork_spectrum[(nrow(filedwork_spectrum) - 1):nrow(filedwork_spectrum), ]

# Merge the sample data with cultivation data
merged_data <- cbind(cultivation, sample_data)


# Extract standard absorbance for 0.001 mg/mL
standard_absorbance <- standard_data$`530`[2] - standard_data$`660`[2]  # Use the second row for 0.001 mg/mL

# Calculate anthocyanin concentration (mg/mL) using the single standard
merged_data <- merged_data %>%
  mutate(
    anthocyanin_concentration_mg_per_mL = anthocyanin_absorbance / standard_absorbance * 0.001
  ) ## OLD

merged_data <- merged_data %>%
  mutate(
    anthocyanin_concentration_mg_per_mL = anthocyanin_absorbance*0.01 / standard_absorbance
  ) ## CHAMBER

# Calculate anthocyanin concentration in mg/g dry weight
# Dry weight is in column `petals(mg)` (convert to grams)
merged_data <- merged_data %>%
  mutate(
    anthocyanin_concentration_mg_per_g = anthocyanin_concentration_mg_per_mL * 0.1 / (petals.mg. / 1000)
  ) ## OLD

merged_data <- merged_data %>%
  mutate(
    anthocyanin_concentration_mg_per_g = anthocyanin_concentration_mg_per_mL * 0.2 / petals.mg. * 1000
  ) ## CHAMBER

# Replace negative concentrations with 0, as they are biologically invalid
merged_data$anthocyanin_concentration_mg_per_g[merged_data$anthocyanin_concentration_mg_per_g < 0] <- 0

fwrite(merged_data,"mergedDataField.csv")

# Save all plots in a single PDF file
pdf("corrFieldColSpectrophotometry.pdf")

# Loop through specified columns to create scatterplots
for (variable in seq(20, 22)) {
  top_var <- names(merged_data)[variable]  # Get variable name from column index
  p <- ggplot(merged_data, aes_string(x = top_var, y = "color.field ", label = "SampleN")) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust text position and size
    labs(title = paste("Correlation between", top_var, "and color.field "),
         x = top_var,
         y = "color.field ") +
    theme_minimal()
  
  print(p)  # Print the plot to the PDF
}

# Close the PDF file
dev.off()

# Perform correlation tests
print(cor.test(x = merged_data$color.field , y = merged_data$anthocyanin_absorbance))

print(cor.test(x = merged_data$color.field , y = merged_data$anthocyanin_concentration_mg_per_mL))

print(cor.test(x = merged_data$color.field , y = merged_data$anthocyanin_concentration_mg_per_g))

hist(merged_data$anthocyanin_concentration_mg_per_g,breaks = 100)

##### Concentrations for color cathegories:
# Přidání nového sloupce s kategoriemi podle `color.field`
merged_data <- merged_data %>%
  mutate(
    color_category = case_when(
      color.field == 0 ~ "0",
      color.field > 0 & color.field <= 2 ~ "0.5-2",
      color.field > 2 & color.field <= 4 ~ "2.5-4"
    )
  )

# Spočítání průměrné koncentrace antokyanů pro každou kategorii
average_concentration <- merged_data %>%
  group_by(color_category) %>%
  summarize(
    mean_concentration_mg_per_g = mean(anthocyanin_concentration_mg_per_g, na.rm = TRUE),
    n_samples = n()  # Počet vzorků v každé kategorii
  )
# Zobrazení výsledků
print(average_concentration)

# Spočítání průměrné koncentrace antokyanů pro každou kategorii
median_concentration <- merged_data %>%
  group_by(color_category) %>%
  summarize(
    median_concentration_mg_per_g = median(anthocyanin_concentration_mg_per_g, na.rm = TRUE),
    n_samples = n()  # Počet vzorků v každé kategorii
  )
# Zobrazení výsledků
print(median_concentration)

# Define a gradient color palette for the boxes
color_palette <- c("white", "lightpink", "magenta")

# Create a boxplot with anthocyanin-themed colors
boxplot <- ggplot(merged_data, aes(x = color_category, y = anthocyanin_concentration_mg_per_g, fill = color_category)) +
  geom_boxplot(color = "black", outlier.shape = NA) +  # Boxplot with black borders, no outlier shapes
  geom_jitter(width = 0.2, color = "darkgrey", alpha = 0.6) +  # Jittered points for individual samples
  scale_fill_manual(
    values = c("0" = "white", "0.5-2" = "lightpink", "2.5-4" = "magenta")  # Assign magenta-based colors to categories
  ) +
  labs(
    title = "Anthocyanin Concentration by Color Categories",
    x = "Color Categories (Field Classification)",
    y = "Anthocyanin Concentration (mg/g dry weight)"
  ) +
  theme_minimal() +  # Clean theme for the plot
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered and bold title
    axis.text = element_text(size = 12),  # Larger axis text
    axis.title = element_text(size = 13),  # Larger axis titles
    legend.position = "none"  # Hide the legend since the category-color mapping is clear
  )

# Display the plot
print(boxplot)

# Save the plot as a PNG file
ggsave("anthocyanin_concentrationVsColorField_boxplot.png", plot = boxplot, width = 8, height = 6)

#### Association mapping 
# Association mapping using linear regression for locus 811
lm_811 <- lm(anthocyanin_concentration_mg_per_g ~ AL2G24170.Thr223Asn.11089811, data = merged_data)
summary(lm_811)

# Association mapping using linear regression for locus 056
lm_056 <- lm(anthocyanin_concentration_mg_per_g ~ AL2G24190..11099056, data = merged_data)
summary(lm_056)

lm_additive <- lm(anthocyanin_concentration_mg_per_g ~ AL2G24170.Thr223Asn.11089811 * AL2G24190..11099056, data = merged_data)
summary(lm_additive)


# Visualize results for locus 811
# Load library
library(ggplot2)
# Create the boxplot
p <- ggplot(merged_data, aes(x = as.factor(AL2G24170.Thr223Asn.11089811), 
                             y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.8) +  # Black jittered dots
  labs(x = "Population", y = "Anthocyanins (mg/g)") +  # Axis labels
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14))
ggsave("boxplot_anthocyaninsField811.pdf", plot = p, width = 4, height = 5)


# Visualize results for locus 056
p <- ggplot(merged_data, aes(x = as.factor(AL2G24190..11099056), 
                             y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.8) +  # Black jittered dots
  labs(x = "Population", y = "Anthocyanins (mg/g)") +  # Axis labels
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14))
ggsave("boxplot_anthocyaninsField056.pdf", plot = p, width = 4, height = 5)


# Visualize additive
p <- ggplot(merged_data, aes(x = as.factor(AL2G24190..11099056+AL2G24170.Thr223Asn.11089811), 
                             y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(width = 0.2, size = 1.5, color = "black", alpha = 0.8) +  # Black jittered dots
  labs(x = "allele", y = "Anthocyanins (mg/g)") +  # Axis labels
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14))
ggsave("boxplot_anthocyaninsFieldAdditive.pdf", plot = p, width = 4, height = 5)





# Model 1: Plný model (oba SNPy)
full_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                   AL2G24170.Thr223Asn.11089811 + 
                   AL2G24190..11099056, 
                 data = merged_data)

# Model 2: Pouze SNP 811
model_811 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  AL2G24170.Thr223Asn.11089811, 
                data = merged_data)

# Model 3: Pouze SNP 056
model_056 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  AL2G24190..11099056, 
                data = merged_data)

# Model 4: Interakční model
interaction_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                          AL2G24170.Thr223Asn.11089811 * 
                          AL2G24190..11099056, 
                        data = merged_data)

# Porovnání modelů pomocí ANOVA
anova(full_model, model_811)
anova(full_model, model_056)
anova(full_model, interaction_model)

# Porovnání modelů pomocí AIC
AIC(full_model, model_811, model_056, interaction_model)

# Shrnutí modelů
summary(full_model)
summary(interaction_model)


###### FIELD DATA ON MAP
# Load necessary libraries
library(tidyverse)    
library(elevatr)      
library(ggplot2)      
library(ggspatial)    
library(sf)           
library(raster)       
library(viridis) 
library(colorspace)

setwd("/home/aa/pigmentation/fieldWork/")
data <- read.csv("mergedDataField.csv", header = TRUE)

# Get elevation data
coords <- data.frame(lon = data$lon, lat = data$lat)
coords_sf <- st_as_sf(coords, coords = c("lon", "lat"), crs = 4326)
elevation <- get_elev_point(locations = coords_sf, src = "aws", z = 14)  # Increased resolution (z = 15)
# Merge with original data
data$altitude <- elevation$elevation

# Correlation test and plot
print(cor.test(x = data$altitude , y = data$anthocyanin_concentration_mg_per_g))
plot(x = data$altitude , y = data$anthocyanin_concentration_mg_per_g)
# Filter data
dataPlot <- subset(data, data$lon < 19.8) # 

# --- 1. GET ELEVATION DATA ---
coords_sf <- st_as_sf(dataPlot, coords = c("lon", "lat"), crs = 4326)
elevation <- get_elev_point(locations = coords_sf, src = "aws")
dataPlot$altitude <- elevation$elevation
# --- 2. COLOR SCALE FOR ANTHOCYANINS ---
dataPlot$color_scale <- (dataPlot$anthocyanin_concentration_mg_per_g - 
                           min(dataPlot$anthocyanin_concentration_mg_per_g)) / 
  (max(dataPlot$anthocyanin_concentration_mg_per_g) - 
     min(dataPlot$anthocyanin_concentration_mg_per_g))
color_palette <- colorRampPalette(c("white", "#c74cfeff"))
# --- 3. HIGH-RES ELEVATION RASTER ---
bbox <- st_bbox(coords_sf)
elev_raster <- get_elev_raster(locations = coords_sf, z = 10, src = "aws") # High-res raster
elev_raster_cropped <- crop(elev_raster, extent(bbox$xmin-0.05, bbox$xmax+0.05, bbox$ymin-0.05, bbox$ymax+0.05))
elev_df <- as.data.frame(rasterToPoints(elev_raster_cropped))
colnames(elev_df)[3] <- "elevation"

# --- 4. PLOT MAP AND SAVE AS PDF ---
pdf("Anthocyanin_Concentration_Maprecolor.pdf", width = 8, height = 9) # Open PDF file
# Modify elevation color scale
ggplot() +
  # Elevation raster with legend
  geom_tile(data = elev_df, aes(x = x, y = y, fill = elevation), alpha = 0.5) +
  scale_fill_gradientn(colors = terrain_hcl(100), name = "Altitude (m)", guide = "colorbar") +  
  # Contour lines for terrain
  geom_contour(data = elev_df, aes(x = x, y = y, z = elevation), color = "gray", binwidth = 50) +
  # Anthocyanin concentration points with black outline
  geom_point(data = dataPlot, aes(x = lon, y = lat, color = color_scale), 
             size = 5, stroke = 1, shape = 21, fill = scales::gradient_n_pal(color_palette(100))(dataPlot$color_scale), color = "black") +  
  # Color scale for anthocyanin concentration
  scale_color_gradientn(colors = color_palette(100), name = "Anthocyanin Conc.") +  
  # Set correct map extent
  coord_sf(xlim = c(bbox$xmin, bbox$xmax), ylim = c(bbox$ymin, bbox$ymax)) +
  # Minimal theme
  theme_minimal() +
  labs(title = "Anthocyanin Concentration Mapping",
       x = "Longitude", y = "Latitude")
dev.off() # Close PDF file

########### Topography Europe ##############
# Load libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(elevatr)
library(raster)
library(viridis)
library(colorspace)

# --- 1. DEFINE REGION (Czechia, Slovakia, Romania) ---
bbox <- c(xmin = 12.0, xmax = 27.0, ymin = 42, ymax = 50)  # Bounding box for the region

# Convert bbox to spatial object
region <- st_as_sf(data.frame(x = c(bbox["xmin"], bbox["xmax"]),
                              y = c(bbox["ymin"], bbox["ymax"])), 
                   coords = c("x", "y"), crs = 4326)

# --- 2. GET HIGH-RES ELEVATION DATA ---
elev_raster <- get_elev_raster(locations = region, z = 5, src = "aws")  # High-res terrain data
elev_raster_cropped <- crop(elev_raster, extent(bbox["xmin"], bbox["xmax"], bbox["ymin"], bbox["ymax"]))

# Convert raster to data frame for ggplot
elev_df <- as.data.frame(rasterToPoints(elev_raster_cropped))
colnames(elev_df)[3] <- "elevation"

# --- 3. PLOT MAP WITH CONTOURS ---
ggplot() +
  geom_tile(data = elev_df, aes(x = x, y = y, fill = elevation), alpha = 0.9) +  
  scale_fill_gradientn(colors = terrain_hcl(100), name = "Altitude (m)") +  
  geom_contour(data = elev_df, aes(x = x, y = y, z = elevation), color = "gray", binwidth = 500) +  
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]), ylim = c(bbox["ymin"], bbox["ymax"])) +
  theme_minimal() +
  labs(title = "Topography of Central Europe",
       x = "Longitude", y = "Latitude")

# --- 4. SAVE MAP AS PDF ---
ggsave("Europe_Topography.pdf", width = 10, height = 7)








################### ANALYZE UV SPECTRUM ##################
# Načtení knihoven
library(dplyr)
library(tidyr)


cultivation_data <- read.csv("/home/aa/pigmentation/mappingExp/anthocyanins/cultivation - overviewMetadata.csv")
spectrum_data <- read_excel("/home/aa/pigmentation/mappingExp/anthocyanins/run4fullSpectrum.xlsx", sheet = 5,na = "?????")
spectrum_data$sampleN<-as.character(spectrum_data$sampleN)

# Získání jmen sloupců, které představují vlnové délky (např. "200", "210", ...)
wavelength_cols <- names(spectrum_data)[sapply(names(spectrum_data), function(x) grepl("^[0-9]{3}$", x))]

# Sloučení s kultivačními daty
merged <- left_join(spectrum_data, cultivation_data, by = "sampleN") %>%
  mutate(color_group = ifelse(color.chamber == 0, "white", "pink"))

# Převod do dlouhého formátu
long_data <- merged %>%
  pivot_longer(cols = all_of(wavelength_cols),
               names_to = "wavelength",
               values_to = "absorbance") %>%
  mutate(
    wavelength = as.numeric(wavelength),
    absorbance = as.numeric(absorbance)
  ) %>%
  filter(!is.na(absorbance), !is.na(color_group))

# Pro každou vlnovou délku proveď Wilcoxonův test
results <- long_data %>%
  group_by(wavelength) %>%
  summarise(
    p_value = tryCatch(
      wilcox.test(absorbance ~ color_group)$p.value,
      error = function(e) NA
    ),
    mean_white = mean(absorbance[color_group == "white"], na.rm = TRUE),
    mean_pink = mean(absorbance[color_group == "pink"], na.rm = TRUE),
    n_white = sum(color_group == "white"),
    n_pink = sum(color_group == "pink")
  ) %>%
  arrange(wavelength)
write.csv(results,"spectrumWhitePinkDifference.csv")

# VIZUALIE
library(ggplot2)
library(dplyr)

# Nastav jednotné limity osy Y
y_min <- min(long_data$absorbance, na.rm = TRUE)
y_max <- max(long_data$absorbance, na.rm = TRUE)

# Přidej p-hodnoty z testu k long_data (pro anotace)
annotated_data <- long_data %>%
  inner_join(results %>% select(wavelength, p_value), by = "wavelength")

# Otevři PDF pro výstup
pdf("/home/aa/pigmentation/mappingExp/anthocyanins/absorbance_boxplots_annotated.pdf", width = 6, height = 8)

for (wl in sort(unique(annotated_data$wavelength))) {
  subset_data <- filter(annotated_data, wavelength == wl)
  pval <- signif(unique(subset_data$p_value), 3)
  
  p <- ggplot(subset_data, aes(x = color_group, y = absorbance, fill = color_group)) +
    geom_boxplot(outlier.shape = NA, alpha = 0.5) +
    geom_jitter(width = 0.15, size = 1, alpha = 0.7) +
    scale_fill_manual(values = c("white" = "white", "pink" = "pink")) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(title = paste("Absorbance at", wl, "nm"),
         subtitle = paste("p =", pval),
         x = "Color group", y = "Absorbance") +
    theme_minimal() +
    theme(legend.position = "none")
  
  print(p)
}
dev.off()

# P-hodnoty přes spektrum – základní scatterplot + threshold
ggplot(results, aes(x = wavelength, y = p_value)) +
  geom_point() +
  geom_hline(yintercept = 0.05, linetype = "dashed", colour = "red") +
  scale_y_log10() +
  labs(title = "P-values for absorbance differences by wavelength",
       x = "Wavelength (nm)",
       y = "P-value (log scale)") +
  theme_minimal()

################################################
############## FLAVONOID ABSORBANCE STANDARDIZED
# Libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(stringr)

# Working directory
setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")

# Load data
cultivation <- read.csv("cultivation - overviewMetadata.csv")
tkp <- read.csv("cultivation - tkpMetadata.csv")
sample_data <- read_excel("run4fullSpectrum.xlsx", sheet = 4)
spectrum_data <- read_excel("run4fullSpectrum.xlsx", sheet = 5, na = "?????")
spectrum_data$sampleN <- as.character(spectrum_data$sampleN)

# Calculate anthocyanin absorbance and concentration
standard_data <- sample_data[1:7, ]
sample_data$anthocyanin_absorbance <- pmax(sample_data$`530` - sample_data$`660`, 0)
standard_abs <- standard_data$`530`[5] - standard_data$`660`[5]
standard_conc <- as.numeric(standard_data$sampleN[5])

merged_conc <- left_join(sample_data, cultivation, by = "sampleN") %>%
  mutate(
    anthocyanin_concentration_mg_per_mL = anthocyanin_absorbance * standard_conc / standard_abs,
    anthocyanin_concentration_mg_per_g = (anthocyanin_concentration_mg_per_mL * 0.25 / as.numeric(petals.mg.)) * 1000
  )

library(tidyverse)

# 1. Propojení dat a výpočet absorbance pro dostupné vzorky
merged_abs <- left_join(spectrum_data, cultivation, by = "sampleN") %>%
  filter(!is.na(petals.mg.)) %>%
  mutate(
    # Výpočet absorbance na gram (zatímco NA hodnoty zůstávají jako NA)
    absorbance_360_per_mg = as.numeric(`360`) / as.numeric(petals.mg.),
    absorbance_360_per_g = absorbance_360_per_mg * 1000,
    seed_category = factor(substr(seedLine, 1, 1), levels = c("s", "z", "h", "t"))
  )

# --- Upravená část: Skupinově specifické nahrazování NA ---

# 1. Definice offsetu (např. 1 % z celkového maxima pro vizuální odskok)
overall_max <- max(merged_abs$absorbance_360_per_mg, na.rm = TRUE)
offset <- 0.01 * overall_max

# 2. Výpočet maxim pro konkrétní seedLine skupiny
# Počítáme maxima pouze z dostupných (non-NA) hodnot
max_z <- max(merged_abs$absorbance_360_per_mg[str_starts(merged_abs$seedLine, "z")], na.rm = TRUE)
max_t <- max(merged_abs$absorbance_360_per_mg[str_starts(merged_abs$seedLine, "t")], na.rm = TRUE)

# 3. Nahrazení NA hodnot podle příslušnosti ke skupině
merged_abs <- merged_abs %>%
  mutate(
    absorbance_360_per_mg = case_when(
      # Pokud je NA a patří do skupiny z
      is.na(absorbance_360_per_mg) & str_starts(seedLine, "z") ~ max_z + offset,
      
      # Pokud je NA a patří do skupiny t
      is.na(absorbance_360_per_mg) & str_starts(seedLine, "t") ~ max_t + offset,
      
      # Pro ostatní (nebo pokud už hodnotu mají) ponechat původní
      TRUE ~ absorbance_360_per_mg
    ),
    # Automatický přepočet na gramy, aby sloupce zůstaly v souladu
    absorbance_360_per_g = absorbance_360_per_mg * 1000
  )

# Merge anthocyanin and flavonol data
merged_all <- left_join(merged_abs, merged_conc %>% select(sampleN, anthocyanin_concentration_mg_per_g), by = "sampleN")

# Correlation test
print(cor.test(
  x = merged_all$anthocyanin_concentration_mg_per_g,
  y = merged_all$absorbance_360_per_mg
))

write.csv(merged_all[,c(1,59:64)],"/home/aa/pigmentation/mappingExp/anthocyanins/flavonolAbsMay26.csv")

####### VISUALIZE FOR ALL
library(tidyverse)
my_shapes <- c("t" = 16, "z" = 16, "s" = 1, "h" = 1)
my_colors <- c("t" = "black", "z" = "grey50", "s" = "grey50", "h" = "black")

# --- 2. Výpočet korelace pro popisek ---
cor_result <- cor.test(merged_all$anthocyanin_concentration_mg_per_g, 
                       merged_all$absorbance_360_per_mg, 
                       use = "complete.obs")

# --- 3. Tvorba grafu ---
p_corr <- ggplot(merged_all, aes(x = anthocyanin_concentration_mg_per_g, 
                                 y = absorbance_360_per_mg,
                                 shape = seed_category,
                                 color = seed_category)) +
  # Body s definovaným stylem
  geom_point(size = 2.5, stroke = 0.8, alpha = 0.8) +
  scale_shape_manual(values = my_shapes) +
  scale_color_manual(values = my_colors) +
  # Regresní čára (lineární trend)
  geom_smooth(aes(group = 1), method = "lm", color = "firebrick", 
              se = TRUE, linetype = "dashed", size = 0.7, fill = "gray90") +
  # Popisky v angličtině
  labs(
    title = "Correlation: Anthocyanins vs Absorbance at 360 nm",
    subtitle = paste0("Pearson's r = ", round(cor_result$estimate, 3), 
                      ", p-value = ", format.pval(cor_result$p.value)),
    x = "Anthocyanin Concentration (mg/g)",
    y = "Absorbance at 360 nm (per g)",
    shape = "Seed Line",
    color = "Seed Line"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# --- 4. Uložení do PDF ---
ggsave("correlation_anthocyanins_vs_abs360.pdf", plot = p_corr, width = 6, height = 5)

# Zobrazení grafu
print(p_corr)


### VISUALIZE FOR TKP INDIV
merged_alltkp<-subset(merged_all, merged_all$sampleN %in% tkp$sampleID)
my_shapes <- c("t" = 16, "z" = 16, "s" = 1, "h" = 1)
my_colors <- c("t" = "black", "z" = "grey50", "s" = "grey50", "h" = "black")

# --- 2. Výpočet korelace pro popisek ---
cor_result <- cor.test(merged_alltkp$anthocyanin_concentration_mg_per_g, 
                       merged_alltkp$absorbance_360_per_mg, 
                       use = "complete.obs")

# --- 3. Tvorba grafu ---
p_corr <- ggplot(merged_alltkp, aes(x = anthocyanin_concentration_mg_per_g, 
                                 y = absorbance_360_per_mg,
                                 shape = seed_category,
                                 color = seed_category)) +
  # Body s definovaným stylem
  geom_point(size = 2.5, stroke = 0.8, alpha = 0.8) +
  scale_shape_manual(values = my_shapes) +
  scale_color_manual(values = my_colors) +
  # Regresní čára (lineární trend)
  geom_smooth(aes(group = 1), method = "lm", color = "firebrick", 
              se = TRUE, linetype = "dashed", size = 0.7, fill = "gray90") +
  # Popisky v angličtině
  labs(
    title = "Correlation: Anthocyanins vs Absorbance at 360 nm per mg",
    subtitle = paste0("Pearson's r = ", round(cor_result$estimate, 3), 
                      ", p-value = ", format.pval(cor_result$p.value)),
    x = "Anthocyanin Concentration (mg/g)",
    y = "Absorbance at 360 nm (per g)",
    shape = "Seed Line",
    color = "Seed Line"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

# --- 4. Uložení do PDF ---
ggsave("correlation_anthocyanins_vs_abs360TKP_indiv.pdf", plot = p_corr, width = 6, height = 5)

# Zobrazení grafu
print(p_corr)


# Create the boxplot with data points
boxplot_plot <- ggplot(merged_alltkp, aes(x = seed_category, y = absorbance_360_per_mg)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7, color = "blue") + 
  labs(x = "Population", y = "Flavonol absorbance (per mg)") +
  theme_minimal()
# Save as a PDF
ggsave("FlavonolAbsorbanceChamber_Boxplot_Populations.pdf", plot = boxplot_plot, width = 8, height = 6, device = "pdf")

######### Divide pink and white
library(tidyverse)
library(ggpubr) # Helpful for statistical annotations

# --- 1. CATEGORY CREATION ---
# We create a new grouping variable 'extended_category'
# s and z remain same, t is split into tw (< 3) and tp (> 3)
merged_alltkp <- merged_alltkp %>%
  mutate(
    first_char = substr(seedLine, 1, 1),
    extended_category = case_when(
      first_char == "s" ~ "s",
      first_char == "z" ~ "z",
      first_char == "t" & anthocyanin_concentration_mg_per_g < 3 ~ "tw",
      first_char == "t" & anthocyanin_concentration_mg_per_g >= 3 ~ "tp",
      TRUE ~ "other"
    ),
    # Set levels for consistent plotting order
    extended_category = factor(extended_category, levels = c("s", "z", "tw", "tp"))
  ) %>%
  filter(extended_category != "other")

# --- 2. STYLE DEFINITIONS ---
# Matching your previous style for the new categories
# tw (t-white/low) and tp (t-purple/high) use the black filled circle
my_shapes_ext <- c("s" = 1, "z" = 16, "tw" = 16, "tp" = 16)
my_colors_ext <- c("s" = "grey50", "z" = "grey50", "tw" = "black", "tp" = "black")

# --- 3. HELPER FUNCTION FOR BOXPLOTS ---
# Creates plots consistent with your "purple box" style
create_custom_boxplot <- function(data, y_var, y_label, title) {
  ggplot(data, aes(x = extended_category, y = .data[[y_var]])) +
    # Transparent purple boxes as requested previously
    geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) + 
    # Jittered points with specific shapes and colors
    geom_jitter(aes(shape = extended_category, color = extended_category), 
                width = 0.2, size = 2, stroke = 1, alpha = 0.8) +
    scale_shape_manual(values = my_shapes_ext) +
    scale_color_manual(values = my_colors_ext) +
    labs(title = title, x = "Seed Line Category", y = y_label) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
}

# --- 4. VISUALIZATION ---
# Plot A: Flavonol Absorbance
p1 <- create_custom_boxplot(merged_alltkp, "absorbance_360_per_mg", 
                            "Flavonol Absorbance (per mg)", 
                            "Flavonol Absorbance by Category")
# Plot B: Anthocyanin Concentration
p2 <- create_custom_boxplot(merged_alltkp, "anthocyanin_concentration_mg_per_g", 
                            "Anthocyanins (mg/g)", 
                            "Anthocyanin Concentration by Category")
# Save plots
ggsave("Boxplot_Flavonols_Extended.pdf", plot = p1, width = 5, height = 6)
ggsave("Boxplot_Anthocyanins_Extended.pdf", plot = p2, width = 5, height = 6)

# --- 5. STATISTICAL TESTING (Wilcoxon Rank Sum Test) ---
# A) Tests for Flavonol Absorbance (360 nm)
# Compare s vs z groups
wilcox.test(absorbance_360_per_mg ~ extended_category, 
            data = subset(merged_alltkp, extended_category %in% c("s", "z")))
# Compare tw vs tp groups
wilcox.test(absorbance_360_per_mg ~ extended_category, 
            data = subset(merged_alltkp, extended_category %in% c("tw", "tp")))


# B) Tests for Anthocyanin Concentration
# Compare s vs z groups
wilcox.test(anthocyanin_concentration_mg_per_g ~ extended_category, 
            data = subset(merged_alltkp, extended_category %in% c("s", "z")))

# Compare tw vs tp groups
wilcox.test(anthocyanin_concentration_mg_per_g ~ extended_category, 
            data = subset(merged_alltkp, extended_category %in% c("tw", "tp")))


# Association mapping for loci
merged <- merged_all %>%
  mutate(
    ploidy_811 = ifelse(nchar(G_811) == 2, 2, 4),
    N_811 = str_count(G_811, "p"),
    freq_811 = N_811 / ploidy_811,
    ploidy_056 = ifelse(nchar(G_056) == 2, 2, 4),
    N_056 = str_count(G_056, "p"),
    freq_056 = N_056 / ploidy_056
  )

# Linear regression models
mergedt<-subset(merged, merged$seed_category %in% "t")
model_811 <- lm(absorbance_360_per_mg ~ freq_811, data = mergedt)
model_056 <- lm(absorbance_360_per_mg ~ freq_056, data = mergedt)
summary(model_811)
summary(model_056)

p <- ggplot(merged, aes(x = as.factor(freq_811), 
                        y = absorbance_360_per_mg)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) + 
  geom_jitter(aes(color = as.factor(ploidy_811), 
                  # ZDE JE OPRAVA: as.numeric() zajistí správné porovnání čísel
                  shape = as.factor(as.numeric(sampleN) > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) + 
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) + 
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) + 
  labs(title = "Association Mapping: Flavonols vs Locus 811",
       x = "Allele at Locus 811",
       y = "Flavonols (per mg)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")
# Uložení
ggsave("boxplot_flavonols_Locus811.pdf", plot = p, width = 4, height = 5)

p <- ggplot(merged, aes(x = as.factor(freq_056), 
                        y = absorbance_360_per_mg)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) + 
  geom_jitter(aes(color = as.factor(ploidy_056), 
                  # ZDE JE OPRAVA: as.numeric() zajistí správné porovnání čísel
                  shape = as.factor(as.numeric(sampleN) > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) + 
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) + 
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) + 
  labs(title = "Association Mapping: Flavonols vs Locus 056",
       x = "Allele at Locus 056",
       y = "Flavonols (per mg)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")
# Uložení
ggsave("boxplot_flavonols_Locus056.pdf", plot = p, width = 4, height = 5)












# Boxplots for allele frequency associations
p1 <- ggplot(merged, aes(x = as.factor(freq_811), y = absorbance_360_per_g)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.8, color = "black") +
  labs(
    title = "Association: Absorbance 360 nm vs. Locus 811",
    x = "Allele Frequency at Locus 811 (p)",
    y = "Absorbance at 360 nm (AU/g)"
  ) +
  theme_minimal()
ggsave("Abs360_vs_Locus811_pAllele.pdf", plot = p1, width = 5, height = 5)

p2 <- ggplot(merged, aes(x = as.factor(freq_056), y = absorbance_360_per_g)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.8, color = "black") +
  labs(
    title = "Association: Absorbance 360 nm vs. Locus 056",
    x = "Allele Frequency at Locus 056 (p)",
    y = "Absorbance at 360 nm (AU/g)"
  ) +
  theme_minimal()
ggsave("Abs360_vs_Locus056_pAllele.pdf", plot = p2, width = 5, height = 5)

# Merge full data for final scatterplot with point size by allele freq sum
total_merged <- left_join(merged, merged_conc %>% select(sampleN, anthocyanin_concentration_mg_per_g), by = "sampleN")

# TKO subset
subset_t <- total_merged %>% filter(substr(seedLine, 1, 1) == "t")

scatter_plot_t_freq <- ggplot(subset_t, aes(x = anthocyanin_concentration_mg_per_g, y = absorbance_360_per_g, size = freq_811 + freq_056)) +
  geom_point(color = "black", alpha = 0.8) +
  scale_size_continuous(range = c(1, 4)) +
  labs(
    x = "Anthocyanins (mg/g DW)",
    y = "Absorbance at 360 nm (AU/g)"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8)
  )
ggsave("Abs360_vs_Anthocyanins_TKO_freqsize.pdf", plot = scatter_plot_t_freq, width = 4, height = 4)


### ####
#### with absolute absorbance at 360 nm
scatter_plot_t_freq <- ggplot(subset_t, aes(x = anthocyanin_concentration_mg_per_g, y = `360`, size = freq_811 + freq_056)) +
  geom_point(color = "black", alpha = 0.8) +
  scale_size_continuous(range = c(1, 4)) +
  labs(
    x = "Anthocyanins (mg/g DW)",
    y = "Absorbance at 360 nm (AU/g)"
  ) +
  theme_minimal(base_size = 8) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8)
  )
ggsave("Abs360_vs_Anthocyanins_TKO_freqsize_rawAbsorbance.pdf", plot = scatter_plot_t_freq, width = 4, height = 4)

# Linear regression models
model_811 <- lm(`360` ~ freq_811, data = subset_t)
model_056 <- lm(`360` ~ freq_056, data = subset_t)
model_both <- lm(`360` ~ freq_056 + freq_811, data = subset_t)

summary(model_811)
summary(model_056)
summary(model_both)

# Boxplots for allele frequency associations
p1 <- ggplot(subset_t, aes(x = as.factor(freq_811), y = `360`)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.8, color = "black") +
  labs(
    title = "Association: Absorbance 360 nm vs. Locus 811",
    x = "Allele Frequency at Locus 811 (p)",
    y = "Absorbance at 360 nm"
  ) +
  theme_minimal()
ggsave("Abs360RAW_vs_Locus811_pAllele.pdf", plot = p1, width = 5, height = 5)

p2 <- ggplot(subset_t, aes(x = as.factor(freq_056), y = `360`)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.8, color = "black") +
  labs(
    title = "Association: Absorbance 360 nm vs. Locus 056",
    x = "Allele Frequency at Locus 056 (p)",
    y = "Absorbance at 360 nm"
  ) +
  theme_minimal()
ggsave("Abs360RAW_vs_Locus056_pAllele.pdf", plot = p2, width = 5, height = 5)

p3 <- ggplot(subset_t, aes(x = as.factor(freq_056+freq_811), y = `360`)) +
  geom_boxplot(fill = "lightgreen", alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.8, color = "black") +
  labs(
    title = "",
    x = "Allele Frequency at Locus 811 + 056 (p)",
    y = "Absorbance at 360 nm"
  ) +
  theme_minimal()
ggsave("Abs360RAW_vs_bothLoci_pAllele.pdf", plot = p3, width = 5, height = 5)




########### GET Absorbance CURVE
library(data.table)

# Načti data
aa <- fread("file:///home/aa/pigmentation/mappingExp/anthocyanins/petalSpectrumFull.csv", na.strings = "?????", header = FALSE)
# Nahrazení NA hodnot číslem 4
aa[is.na(aa)] <- 4
# Získání vlnových délek z prvního řádku (např. 200–700 nm)
wavelengths <- as.numeric(aa[1, 11:ncol(aa)])
# Otevření PDF výstupu
pdf("absorbance_spectra.pdf", width = 6, height = 4)
# Smyčka přes všechny vzorky (řádky s daty od 2. řádku dál)
for (i in 2:nrow(aa)) { #i=2
  absorbance <- as.numeric(aa[i, 11:ncol(aa)])
  
  plot(wavelengths, absorbance,
       type = "p", pch = 16, col = "#2C3E50",
       xlab = "Wavelength (nm)",
       ylab = "Absorbance",
       main = paste(aa[i, 1], aa[i, 2]),
       xlim = range(wavelengths),
       ylim = c(0, max(absorbance, na.rm = TRUE)),
       cex.main = 1.2,
       cex.lab = 1,
       cex.axis = 0.9,
       las = 1,
       bty = "n")
  
  # Jemná mřížka pro orientaci
  grid(col = "lightgray", lty = "dotted")
}
# Zavři PDF soubor
dev.off()


write.csv(aa,"/home/aa/pigmentation/mappingExp/anthocyanins/petalSpectrumFull_UVB_UVA_visible.csv",col.names = F)

# ==============================================================================
# SOUHRNNÝ GRAF: PRŮMĚRNÉ SPEKTRUM A ROZPTYL (SUROVÁ ABSORBANCE)
# ==============================================================================

# 1. Extrakce matice absorbancí (řádky = vzorky, sloupce = vlnové délky)
cols <- 11:ncol(aa)
abs_matrix <- as.matrix(aa[2:nrow(aa), ..cols])
class(abs_matrix) <- "numeric"

# 2. Výpočet průměru a směrodatné odchylky (SD) pro každou vlnovou délku
mean_abs <- colMeans(abs_matrix, na.rm = TRUE)
sd_abs   <- apply(abs_matrix, 2, sd, na.rm = TRUE)

upper_bound <- mean_abs + sd_abs
lower_bound <- pmax(0, mean_abs - sd_abs) # Zamezení záporným hodnotám u spodní hranice

# 3. Vykreslení souhrnného grafu
pdf("/home/aa/pigmentation/mappingExp/anthocyanins/absorbance_mean_spectrum.pdf", width = 7, height = 5)

plot(wavelengths, mean_abs, type = "n",
     xlab = "Wavelength (nm)",
     ylab = "Absorbance",
     main = paste0("Mean Absorbance Spectrum (N = ", nrow(abs_matrix), ")"),
     xlim = range(wavelengths),
     ylim = c(0, max(upper_bound, na.rm = TRUE)),
     las = 1, bty = "n", cex.lab = 1, cex.axis = 0.9)

# Mřížka
grid(col = "lightgray", lty = "dotted")

# Stínovaný pás rozptylu (± 1 SD)
polygon(c(wavelengths, rev(wavelengths)),
        c(upper_bound, rev(lower_bound)),
        col = adjustcolor("#2C3E50", alpha.f = 0.25),
        border = NA)

# Průměrná křivka
lines(wavelengths, mean_abs, col = "#2C3E50", lwd = 2.5)

# Legenda
legend("topright", 
       legend = c("Mean absorbance", "Variance (± 1 SD)"),
       col = c("#2C3E50", adjustcolor("#2C3E50", alpha.f = 0.25)),
       lwd = c(2.5, 8), bty = "n")

dev.off()


# ==============================================================================
# DOPORUČENÁ VARIANTA: NORMALIZOVANÁ ABSORBANCE (0 až 1)
# Odstraňuje vliv rozdílných koncentrací roztoků a zvýrazňuje piky
# ==============================================================================

# Normalizace každého vzorku na jeho vlastní maximum
abs_norm <- t(apply(abs_matrix, 1, function(x) {
  m <- max(x, na.rm = TRUE)
  if (m > 0) return(x / m) else return(x)
}))

mean_norm  <- colMeans(abs_norm, na.rm = TRUE)
sd_norm    <- apply(abs_norm, 2, sd, na.rm = TRUE)
upper_norm <- pmin(1, mean_norm + sd_norm)
lower_norm <- pmax(0, mean_norm - sd_norm)

pdf("/home/aa/pigmentation/mappingExp/anthocyanins/absorbance_mean_spectrum_normalized.pdf", width = 7, height = 5)

plot(wavelengths, mean_norm, type = "n",
     xlab = "Wavelength (nm)",
     ylab = "Normalized absorbance (0-1)",
     main = "Concentration independent absorbance spectrum",
     xlim = range(wavelengths),
     ylim = c(0, 1.05),
     las = 1, bty = "n", cex.lab = 1, cex.axis = 0.9)

grid(col = "lightgray", lty = "dotted")

# Stínovaný pás rozptylu
polygon(c(wavelengths, rev(wavelengths)),
        c(upper_norm, rev(lower_norm)),
        col = adjustcolor("#E74C3C", alpha.f = 0.25),
        border = NA)

# Průměrná křivka
lines(wavelengths, mean_norm, col = "#C0392B", lwd = 2.5)

legend("topright", 
       legend = c("Mean (normalized)", "Variance (± 1 SD)"),
       col = c("#C0392B", adjustcolor("#E74C3C", alpha.f = 0.25)),
       lwd = c(2.5, 8), bty = "n")

dev.off()






############### CALCULATE THE ANTHOCYANIN CONCENTRATION ###########
################# CLIMATE CHAMBER
setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")
# Load necessary libraries
library(readxl)
library(dplyr)
library(ggplot2)

# Load the data
cultivation <- read.csv("/home/aa/pigmentation/mappingExp/anthocyanins/cultivation - overviewMetadata.csv")
sample_data <- read_excel("/home/aa/pigmentation/mappingExp/anthocyanins/run4fullSpectrum.xlsx", sheet = 4)
leaf_data <- read_excel("/home/aa/pigmentation/mappingExp/anthocyanins/leavesFinalSpectrum.xlsx", sheet = 3)

# Compute anthocyanin absorbance (A530 - A660)
sample_data$anthocyanin_absorbance <- sample_data$`530` - sample_data$`660`
leaf_data$anthocyanin_absorbance <- leaf_data$`530` - leaf_data$`660`
# standard: 0.062500, 0.3263, 0.25	E1	1.48	0.0423

# Turn negative absorbance values into 0
sample_data$anthocyanin_absorbance[sample_data$anthocyanin_absorbance < 0] <- 0
leaf_data$anthocyanin_absorbance[leaf_data$anthocyanin_absorbance < 0] <- 0

# Extract standard absorbance and concentrations (last two rows)
standard_data <- sample_data[1:7, ]

# Merge the sample data with cultivation data
merged_data <- merge(cultivation, sample_data, by = "sampleN")
merged_data1 <- merge(merged_data, leaf_data, by = "sampleN")

hist(merged_data$anthocyanin_absorbance,breaks = 100)

#standard_curve <- lm(standard_data$sampleN ~ standard_data$anthocyanin_absorbance)
#plot(standard_curve)
plot(standard_data$sampleN ~ standard_data$anthocyanin_absorbance)

# Extract standard absorbance for standard mg/mL
standard_absorbance <- standard_data$`530`[5] - standard_data$`660`[5]  
leaf_absorbance<-0.3263
  
# Calculate anthocyanin concentration (mg/mL) using the single standard
merged_data <- merged_data %>%
  mutate(
    anthocyanin_concentration_mg_per_mL = anthocyanin_absorbance * as.numeric(standard_data$sampleN[5]) / standard_absorbance
  )
# leaf
merged_data1 <- merged_data1 %>%
  mutate(
    anthocyanin_concentration_mg_per_mLpetal = anthocyanin_absorbance.x * as.numeric(standard_data$sampleN[5]) / standard_absorbance
  )
merged_data1 <- merged_data1 %>%
  mutate(
    anthocyanin_concentration_mg_per_mLleaf = anthocyanin_absorbance.y * 0.062500 / leaf_absorbance
  )

# Calculate anthocyanin concentration in mg/g dry weight
# Dry weight is in column `petals(mg)` (convert to grams)
merged_data <- merged_data %>%
  mutate(
    anthocyanin_concentration_mg_per_g = (anthocyanin_concentration_mg_per_mL * 0.25 / (as.numeric(petals.mg.)))*1000) #0.25 was the volume of extraction medium, 1000 to convert to grams

##leaf
merged_data1 <- merged_data1 %>%
  mutate(
    anthocyanin_concentration_mg_per_gpetal = (anthocyanin_concentration_mg_per_mLpetal * 0.25 / (as.numeric(petals.mg.)))*1000)

merged_data1 <- merged_data1 %>%
  mutate(
    anthocyanin_concentration_mg_per_gleaf = (anthocyanin_concentration_mg_per_mLleaf * 1 / (as.numeric(leafs.mg.)))*1000)

# Replace negative concentrations with 0, as they are biologically invalid
merged_data$anthocyanin_concentration_mg_per_g[merged_data$anthocyanin_concentration_mg_per_g < 0] <- 0

treatment_data<-subset(merged_data,merged_data$color.chamber %in% NA)
merged_data<-subset(merged_data,!merged_data$color.chamber %in% NA)

# Save all plots in a single PDF file
pdf("corrChamberColSpectrophotometry.pdf")
# Loop through specified columns to create scatterplots
for (variable in seq(12, 14)) {
  top_var <- names(merged_data)[variable]  # Get variable name from column index
  p <- ggplot(na.omit(merged_data), aes_string(x = top_var, y = "color.chamber ", label = "sampleN")) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust text position and size
    labs(title = paste("Correlation between", top_var, "and color.chamber "),
         x = top_var,
         y = "color.chamber ") +
    theme_minimal()
  print(p)  # Print the plot to the PDF
}
# Close the PDF file
dev.off()

# Correltation Color estimate vs spectrophotometry
print(cor.test(x = merged_data$color.chamber , y = merged_data$anthocyanin_absorbance))

print(cor.test(x = merged_data$color.chamber , y = merged_data$anthocyanin_concentration_mg_per_mL))

print(cor.test(x = merged_data$color.chamber , y = merged_data$anthocyanin_concentration_mg_per_g))

hist(merged_data$anthocyanin_concentration_mg_per_g,breaks = 100)

# Seedling vs petal color
print(cor.test(x = as.numeric(merged_data$seedlingColor) , y = merged_data$anthocyanin_concentration_mg_per_g))
boxplot(merged_data$anthocyanin_concentration_mg_per_g~as.numeric(merged_data$seedlingColor))

## TODO udelat hezci!!! Jako tu asociaci s alelou


# Leaf vs petal
print(cor.test(x = as.numeric(merged_data1$anthocyanin_concentration_mg_per_gleaf) , y = merged_data1$anthocyanin_concentration_mg_per_gpetal))

p <- ggplot(merged_data1, aes_string(x = "anthocyanin_concentration_mg_per_gpetal", y = "anthocyanin_concentration_mg_per_gleaf", label = "sampleN")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust text position and size
  labs(title = paste("Correlation petal - leaf"),
       x = "anthocyanin_concentration_mg_per_gpetal",
       y = "anthocyanin_concentration_mg_per_gleaf") +
  theme_minimal()
print(p)

## alpine only: is there individual-level correspondance?
merged_data2<-subset(merged_data1,as.numeric(merged_data1$sampleN)<300)
merged_data2<-subset(merged_data2,substr(merged_data2$seedLine,1,1) %in% "z")
print(cor.test(x = as.numeric(merged_data2$anthocyanin_concentration_mg_per_gleaf) , y = merged_data2$anthocyanin_concentration_mg_per_gpetal))

p <- ggplot(merged_data2, aes_string(x = "anthocyanin_concentration_mg_per_gpetal", y = "anthocyanin_concentration_mg_per_gleaf", label = "sampleN")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust text position and size
  labs(title = paste("Correlation petal - leaf"),
       x = "anthocyanin_concentration_mg_per_gpetal",
       y = "anthocyanin_concentration_mg_per_gleaf") +
  theme_minimal()
print(p)

# this is not a correct approach. better variance partitioning
merged_data1$pop<-substr(merged_data1$seedLine,1,1)
lm_leafpetal <- lm(merged_data1$anthocyanin_concentration_mg_per_gpetal ~ merged_data1$anthocyanin_concentration_mg_per_gleaf + substr(merged_data1$seedLine,1,1))
summary(lm_leafpetal)
write.table(merged_data1,"mergedDataPetalLeaf.txt",row.names = F)


#### Variance partitioning:
# Load required libraries
library(lme4)  # For mixed models
library(insight)  # For variance partitioning

# Load data
data <- read.table("file:///home/aa/pigmentation/mappingExp/anthocyanins/mergedDataPetalLeaf.txt", header = TRUE)
# Fit a mixed model
# anthocyanin_concentration_mg_per_gleaf as the response variable
# anthocyanin_concentration_mg_per_gpetal as the fixed effect predictor
# pop as a random effect
model_interaction <- lmer(
  anthocyanin_concentration_mg_per_gleaf ~ anthocyanin_concentration_mg_per_gpetal * pop + (1 | pop),
  data = data
)

# Summarize the model to view fixed and random effects
summary(model_interaction)

# Extract variance components
var_comp <- as.data.frame(VarCorr(model_interaction))
var_pop <- var_comp[var_comp$grp == "pop", "vcov"]  # Variance for population
var_residual <- attr(VarCorr(model_interaction), "sc")^2  # Residual variance

# Compute variance explained by the fixed effect (petal color), interaction term, and other components
var_fixed_effect <- var(fitted(update(model_interaction, . ~ anthocyanin_concentration_mg_per_gpetal + (1 | pop))))
var_interaction <- var_fixed_effect - var_fixed_effect_only  # Variance explained by interaction only
var_total <- var_pop + var_residual + var_fixed_effect  # Total variance

# Calculate percentages of variance explained
percent_variance_pop <- (var_pop / var_total) * 100
percent_variance_fixed_only <- (var_fixed_effect_only / var_total) * 100
percent_variance_interaction <- (var_interaction / var_total) * 100
percent_variance_residual <- (var_residual / var_total) * 100

# Output the percentages
cat("Percentage of variance explained by population:", percent_variance_pop, "%\n")
cat("Percentage of variance explained by petal color (fixed effect only):", percent_variance_fixed_only, "%\n")
cat("Percentage of variance explained by interaction between petal color and population:", percent_variance_interaction, "%\n")
cat("Percentage of variance unexplained (residual):", percent_variance_residual, "%\n")

### Pomoci insight
# Perform variance partitioning using insight package
variance_partition <- insight::get_variance(model_interaction)

# Extract variance components
var_fixed <- variance_partition$var.fixed
var_random <- variance_partition$var.random
var_residual <- variance_partition$var.residual
var_total <- var_fixed + var_random + var_residual

# Extract variance explained by interaction and fixed effect separately
# Fit a reduced model without interaction to estimate variance explained by fixed effect only
model_no_interaction <- lmer(
  anthocyanin_concentration_mg_per_gleaf ~ anthocyanin_concentration_mg_per_gpetal + (1 | pop),
  data = data
)
variance_no_interaction <- insight::get_variance(model_no_interaction)
var_fixed_no_interaction <- variance_no_interaction$var.fixed

# Variance explained by interaction
var_interaction <- var_fixed - var_fixed_no_interaction

# Calculate percentages of variance explained
percent_variance_fixed_only <- (var_fixed_no_interaction / var_total) * 100
percent_variance_interaction <- (var_interaction / var_total) * 100
percent_variance_random <- (var_random / var_total) * 100
percent_variance_residual <- (var_residual / var_total) * 100

# Output the percentages
cat("Percentage of variance explained by fixed effects (excluding interaction):", percent_variance_fixed_only, "%\n")
cat("Percentage of variance explained by interaction between petal color and population:", percent_variance_interaction, "%\n")
cat("Percentage of variance explained by random effects (population):", percent_variance_random, "%\n")
cat("Percentage of variance unexplained (residual):", percent_variance_residual, "%\n")


##### Concentrations for color cathegories:
# Přidání nového sloupce s kategoriemi podle `color.chamber`
merged_data <- merged_data %>%
  mutate(
    color_category = case_when(
      color.chamber == 0 ~ "0",
      color.chamber > 0 & color.chamber <= 2 ~ "0.5-2",
      color.chamber > 2 & color.chamber <= 4 ~ "2.5-4"
    )
  )

# Spočítání průměrné koncentrace antokyanů pro každou kategorii
average_concentration <- merged_data %>%
  group_by(color_category) %>%
  summarize(
    mean_concentration_mg_per_g = mean(anthocyanin_concentration_mg_per_g, na.rm = TRUE),
    sd_concentration_mg_per_g = sd(anthocyanin_concentration_mg_per_g, na.rm = TRUE),
    n_samples = n()  # Počet vzorků v každé kategorii
  )
# Zobrazení výsledků
print(average_concentration)

# Median
median_concentration <- merged_data %>%
  group_by(color_category) %>%
  summarize(
    median_concentration_mg_per_g = median(anthocyanin_concentration_mg_per_g, na.rm = TRUE),
    n_samples = n()  # Počet vzorků v každé kategorii
  )
# Zobrazení výsledků
print(median_concentration)

# Define a gradient color palette for the boxes
color_palette <- c("white", "lightpink", "magenta")

# Create a boxplot with anthocyanin-themed colors
boxplot <- ggplot(merged_data, aes(x = color_category, y = anthocyanin_concentration_mg_per_g, fill = color_category)) +
  geom_boxplot(color = "black", outlier.shape = NA) +  # Boxplot with black borders, no outlier shapes
  geom_jitter(width = 0.2, color = "darkgrey", alpha = 0.6) +  # Jittered points for individual samples
  scale_fill_manual(
    values = c("0" = "white", "0.5-2" = "lightpink", "2.5-4" = "magenta")  # Assign magenta-based colors to categories
  ) +
  labs(
    title = "Anthocyanin Concentration by Color Categories",
    x = "Color categories",
    y = "Anthocyanins (mg/g dry weight)"
  ) +
  theme_minimal() +  # Clean theme for the plot
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered and bold title
    axis.text = element_text(size = 12),  # Larger axis text
    axis.title = element_text(size = 13),  # Larger axis titles
    legend.position = "none"  # Hide the legend since the category-color mapping is clear
  )

# Display the plot
print(boxplot)

# Save the plot as a PNG file
ggsave("anthocyanin_concentrationVsColorChamber_boxplot.png", plot = boxplot, width = 8, height = 6)

############ CALCULATE AN and AF for alleles
# Load necessary libraries
library(dplyr)
# Step 1: Count occurrences of allele 'p' for each column and store them in new columns
merged_data <- merged_data %>%
  mutate(
    # Count 'p' occurrences in G_811, G_019, and G_056
    N_811 = stringr::str_count(G_811, "p"),
    N_019 = stringr::str_count(G_019, "p"),
    N_056 = stringr::str_count(G_056, "p")
  )

# Step 2: Calculate allele frequency for each row
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(
    # Determine ploidy level based on the length of alleles in G_811 (assumes consistent ploidy)
    ploidy = ifelse(nchar(G_811) == 2, 2, 4),
    
    # Calculate allele frequency for 'p' in each column
    freq_811 = N_811 / ploidy,
    freq_019 = N_019 / ploidy,
    freq_056 = N_056 / ploidy
  ) %>%
  ungroup()

fwrite(merged_data, "mergedDataAll225IndivAllInfo.csv")

#### COLORS IN STANDARD CONDITIONS
# Load necessary libraries
setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")
library(ggplot2)
data <- read.csv("mergedDataAll225IndivAllInfo.csv", header=TRUE)
# Extract the first letter of seedLine and convert to a factor with a defined order
data$seed_category <- factor(substr(data$seedLine, 1, 1), levels = c("s", "z", "h", "t"))
# Create the boxplot with data points
boxplot_plot <- ggplot(data, aes(x = seed_category, y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7, color = "blue") + 
  labs(x = "Population", y = "Anthocyanin Concentration (mg per g)") +
  theme_minimal()
# Save as a PDF
ggsave("AnthocyaninConcentrationChamber_Boxplot_Populations.pdf", plot = boxplot_plot, width = 8, height = 6, device = "pdf")
# Perform Wilcoxon tests
wilcox_s_z <- wilcox.test(
  anthocyanin_concentration_mg_per_g ~ seed_category, 
  data = subset(data, seed_category %in% c("s", "z"))
)
wilcox_h_t <- wilcox.test(
  anthocyanin_concentration_mg_per_g ~ seed_category, 
  data = subset(data, seed_category %in% c("h", "t"))
)

### Seedling - problem with missing data for HRA and SUB!!!
# Create the boxplot with data points
boxplot_plot <- ggplot(data, aes(x = seed_category, y = seedlingColor)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7, color = "blue") + 
  labs(x = "Population", y = "Seedling color") +
  theme_minimal()
# Save as a PDF
ggsave("AnthocyaninConcentrationChamber_Boxplot_Populations.pdf", plot = boxplot_plot, width = 8, height = 6, device = "pdf")
# Perform Wilcoxon tests
wilcox_s_z <- wilcox.test(
  anthocyanin_concentration_mg_per_g ~ seed_category, 
  data = subset(data, seed_category %in% c("s", "z"))
)
wilcox_h_t <- wilcox.test(
  anthocyanin_concentration_mg_per_g ~ seed_category, 
  data = subset(data, seed_category %in% c("h", "t"))
)

### LEAF
# Načtení potřebné knihovny
library(dplyr)
# 1. Načtení dat
# Předpokládám, že .txt soubor má sloupce oddělené mezerami/tabulátorem (vypadá to na výstup z write.table)
data_all <- read.csv("~/pigmentation/mappingExp/anthocyanins/petal_leaf_spectrum_AFs_May26Final.csv")
data_all$seed_category <- factor(substr(data$seedLine, 1, 1), levels = c("s", "z", "h", "t"))
#data_all$anthocyanin_concentration_mg_per_g_leaf
boxplot_plot <- ggplot(data_all, aes(x = seed_category, y = anthocyanin_concentration_mg_per_g_leaf)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(width = 0.2, size = 1.5, alpha = 0.7, color = "blue") + 
  labs(x = "Population", y = "Anthocyanins (mg/g)") +
  theme_minimal()
# Save as a PDF
ggsave("AnthocyaninConcentrationChamberLeaf_Boxplot_Populations.pdf", plot = boxplot_plot, width = 8, height = 6, device = "pdf")
# Perform Wilcoxon tests
wilcox_s_z <- wilcox.test(
  anthocyanin_concentration_mg_per_g_leaf ~ seed_category, 
  data = subset(data_all, seed_category %in% c("s", "z"))
)
wilcox_h_t <- wilcox.test(
  anthocyanin_concentration_mg_per_g_leaf ~ seed_category, 
  data = subset(data_all, seed_category %in% c("h", "t"))
)

nrow(subset(merged_data1, pop %in% c("s", "z")))
nrow(subset(merged_data1, pop %in% c("h", "t")))

############# CORRELATION LEAF - SEEDLING - PETAL
data<-read.csv("~/pigmentation/mappingExp/anthocyanins/petal_leaf_spectrum_AFs_May26Final.csv")
# Načtení potřebných knihoven
library(tidyverse)
# 1. Načtení dat
data <- read.csv("petal_leaf_spectrum_AFs_May26Final.csv")
# Filtrace: pouze seedLine začínající na 't'
data_t <- data %>% 
  filter(str_starts(seedLine, "t"))

# Leaf Seedling Does not make sense to do?
corr_leaf_color <- cor.test(data_t$anthocyanin_concentration_mg_per_g_leaf, 
                            as.numeric(data_t$seedlingColor), 
                            method = "pearson", use = "complete.obs")

# B)Leaf Petal
corr_leaf_petal <- cor.test(data_t$anthocyanin_concentration_mg_per_g_leaf, 
                            data_t$anthocyanin_concentration_mg_per_g_petal, 
                            method = "pearson", use = "complete.obs")

plot(data_t$anthocyanin_concentration_mg_per_g_leaf~data_t$anthocyanin_concentration_mg_per_g_petal)
plot(data$anthocyanin_concentration_mg_per_g_leaf~data$anthocyanin_concentration_mg_per_g_petal)

# Príprava stĺpca pre kategórie (podľa prvého písmena seedLine)
data_styled <- data %>%
  mutate(category = str_sub(seedLine, 1, 1)) %>%
  filter(category %in% c("t", "h", "s", "z"))

# Definícia manuálnych štylov
# shape 21 je kruh s obrysom (umožňuje rozlíšiť plný/prázdny)
# shape 16 je čisto plný kruh
my_shapes <- c("t" = 16, "z" = 16, "h" = 1, "s" = 1)
my_colors <- c("t" = "black", "z" = "grey60", "h" = "black", "s" = "grey60")

# Funkcia pre vykreslenie
create_styled_plot <- function(df, title_text) {
  correlation <- cor(df$anthocyanin_concentration_mg_per_g_petal, 
                     df$anthocyanin_concentration_mg_per_g_leaf, 
                     use = "complete.obs")
  
  ggplot(df, aes(x = anthocyanin_concentration_mg_per_g_petal, 
                 y = anthocyanin_concentration_mg_per_g_leaf,
                 shape = category,
                 color = category)) +
    geom_point(size = 2.5, stroke = 0.8) +
    scale_shape_manual(values = my_shapes) +
    scale_color_manual(values = my_colors) +
    geom_smooth(aes(group = 1), method = "lm", color = "firebrick", 
                se = FALSE, linetype = "dashed", size = 0.7) +
    labs(title = title_text,
         subtitle = paste("Pearson's correlation r =", round(correlation, 3)),
         x = "Petal Anthocyanins (mg/g)",
         y = "Leaf Anthocyanins (mg/g)",
         shape = "Seed Line Type",
         color = "Seed Line Type") +
    theme_bw() +
    theme(legend.position = "right")
}

# 1. Graf iba pre t*
data_t_only <- data_styled %>% filter(category == "t")
plot_t <- create_styled_plot(data_t_only, "Correlation: seedLine 't*' Subset")

# 2. Graf pre všetky kategórie (t, h, s, z)
plot_all <- create_styled_plot(data_styled, "Correlation: Styled by Category")

# Uloženie do PDF
pdf("LeafPetalAnthocyanin.pdf", width = 8, height = 10)
gridExtra::grid.arrange(plot_t, plot_all, ncol = 1)
dev.off()







#### COMPARISON OF RECOMBINANT 
##### PER AF CATHEGORY
setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")
data <- read.csv("mergedDataAll225IndivAllInfo.csv", header=TRUE)
# Filter only tetraploid individuals
tetraploid_data <- subset(data, ploidy == 4 & substr(seedLine,1,1)=="t")

### TEST the effect of 056
wilcox.test(subset(tetraploid_data,tetraploid_data$freq_056 %in% 0.00 & tetraploid_data$freq_811 %in% 0.00)$anthocyanin_concentration_mg_per_g,subset(tetraploid_data,tetraploid_data$freq_811 %in% 0.00 & tetraploid_data$freq_056 %in% 0.5)$anthocyanin_concentration_mg_per_g,alternative = "less")
nrow(subset(tetraploid_data,tetraploid_data$freq_056 %in% 0.00 & tetraploid_data$freq_811 %in% 0.00))


# Select experimental group: individuals where freq_056 and freq_811 differ by at least 0.5
experimental_group <- subset(tetraploid_data, abs(freq_056 - freq_811) >= 0.5)
# Calculate sum of frequencies for each experimental individual
experimental_group$freq_sum <- experimental_group$freq_056 + experimental_group$freq_811
# Identify potential control individuals (freq_056 == freq_811)
potential_controls <- subset(tetraploid_data, freq_056 == freq_811)
# Calculate sum of frequencies for potential controls
potential_controls$freq_sum <- potential_controls$freq_056 + potential_controls$freq_811
# Filter only relevant freq_sum categories (0.5 and 1)
experimental_group <- subset(experimental_group, freq_sum %in% c(0.5, 1))
control_group <- subset(potential_controls, freq_sum %in% experimental_group$freq_sum)
# Match controls where freq_sum is the same as in the experimental group
control_group <- control_group %>%
  filter(freq_sum %in% experimental_group$freq_sum)
# Add labels for boxplot grouping (ensuring logical order)
experimental_group$group <- factor(
  paste0("Recombined (Sum = ", experimental_group$freq_sum, ")"),
  levels = c("Recombined (Sum = 0.5)", "Recombined (Sum = 1)")
)
control_group$group <- factor(
  paste0("Control (Sum = ", control_group$freq_sum, ")"),
  levels = c("Control (Sum = 0.5)", "Control (Sum = 1)")
)
# Combine datasets
comparison_data <- rbind(control_group, experimental_group)
# Order factor levels to ensure logical boxplot order
comparison_data$group <- factor(
  comparison_data$group,
  levels = c("Control (Sum = 0.5)", "Recombined (Sum = 0.5)", 
             "Control (Sum = 1)", "Recombined (Sum = 1)")
)
# Create a boxplot with data points colored by freq_056
boxplot_plot <- ggplot(comparison_data, aes(x = group, y = anthocyanin_concentration_mg_per_g, fill = group)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +  # Boxplot without separate outliers
  geom_jitter(aes(color = freq_056), position = position_jitter(width = 0.2), size = 2, alpha = 0.7) +  # Data points colored by freq_056
  scale_color_gradient(low = "blue", high = "red") +  # Gradient from blue (low freq_056) to red (high freq_056)
  labs(title = "Anthocyanin Concentration",
       x = "Group",
       y = "Anthocyanin Concentration (mg per g)",
       color = "Frequency of Allele 056") +
  theme_minimal()
# Save the plot
ggsave("Comparison_Anthocyanin_Boxplot_Ordered.pdf", plot = boxplot_plot, width = 10, height = 6,device = "pdf")
# Perform Wilcoxon tests for each freq_sum category
wilcox_05 <- wilcox.test(
  anthocyanin_concentration_mg_per_g ~ group, 
  data = subset(comparison_data, freq_sum == 0.5),alternative="greater"
)
wilcox_1 <- wilcox.test(
  anthocyanin_concentration_mg_per_g ~ group, 
  data = subset(comparison_data, freq_sum == 1)
)


######## increase in concentration depending on N alleles
setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")
library(ggplot2)
# Read the dataset
data <- read.csv("mergedDataAll225IndivAllInfo.csv", header=TRUE)
tetraploid_data <- subset(data, ploidy == 4 & substr(seedLine, 1, 1) == "t")
# Fit a linear model predicting anthocyanin concentration (mg/g) based on N_811
model <- lm(anthocyanin_concentration_mg_per_g ~ N_811, data = tetraploid_data)
# Display the summary of the model
summary(model)
# Extract and print the coefficient for N_811
coef(model)["N_811"]

model <- lm(anthocyanin_concentration_mg_per_g ~ N_056, data = tetraploid_data)
# Display the summary of the model
summary(model)
# Extract and print the coefficient for N_811
coef(model)["N_056"]

######## wilcoxon's test diploid concentrations vs heterozygosity
library(data.table)
wilcox.test(subset(data,data$seedLine %like% "z" & data$freq_811 %in% 0.5)$anthocyanin_concentration_mg_per_g,subset(data,data$seedLine %like% "z" & data$freq_811 %in% 1)$anthocyanin_concentration_mg_per_g,alternative = "less")
nrow(subset(data,data$seedLine %like% "z" & data$freq_811 %in% 1))



######## FOR STANDARD 056 AND 811
# Load necessary libraries
setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")
library(ggplot2)
library(dplyr)
# Read the dataset
data <- read.csv("mergedDataAll225IndivAllInfo.csv", header=TRUE)
tetraploid_data <- subset(data, ploidy == 4 & substr(seedLine, 1, 1) == "t")

### PART 1: Varying freq_811 while keeping freq_056 = 0.5 ###
# Select individuals where freq_056 = 0.5
filtered_data_811 <- subset(tetraploid_data, freq_056 == 0.5)
# Identify unique freq_811 values
unique_freq_811 <- sort(unique(filtered_data_811$freq_811))
# Select individuals where both freq_056 = 0 and freq_811 = 0 (for background)
background_data_811 <- subset(tetraploid_data, freq_056 == 0 & freq_811 == 0)
background_data_811$freq_811 <- -0.1  # Assign a unique value for ordering
# Combine all data for plotting
plot_data_811 <- rbind(filtered_data_811, background_data_811)
# Set categorical labels for X-axis in the desired order
plot_data_811$freq_811_label <- factor(
  paste0("811 = ", plot_data_811$freq_811, ", 056 = ", plot_data_811$freq_056),
  levels = c(
    "811 = -0.1, 056 = 0",  # Background group
    "811 = 0, 056 = 0.5",
    "811 = 0.25, 056 = 0.5",
    "811 = 0.5, 056 = 0.5",
    "811 = 0.75, 056 = 0.5"
  )
)
# Define max y-limit for consistency across all plots
y_limit <- max(plot_data_811$anthocyanin_concentration_mg_per_g, na.rm = TRUE)

# Create the styled boxplot
final_plot_811 <- ggplot(plot_data_811, aes(x = freq_811_label, y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = freq_056), width = 0.2, size = 2, alpha = 0.8) +  # Black jittered dots
  scale_color_gradient(low = "black", high = "black") +  # Gradient for jitter points
  ylim(0, y_limit) +  # Set Y-axis limit
  labs(x = "Allele", y = "Anthocyanins (mg/g)") +  # Axis labels
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend

# Save as PDF
ggsave("Anthocyanin_Freq_811_Plot0.5.pdf", plot = final_plot_811, width = 4, height = 5)

### PART 2: Varying freq_056 while keeping freq_811 = 0.5 ###
# Select individuals where freq_811 = 0.5
filtered_data_056 <- subset(tetraploid_data, freq_811 == 0.5)
# Identify unique freq_056 values
unique_freq_056 <- sort(unique(filtered_data_056$freq_056))
# Select individuals where both freq_056 = 0 and freq_811 = 0 (for background)
background_data_056 <- subset(tetraploid_data, freq_056 == 0 & freq_811 == 0)
background_data_056$freq_056 <- -0.1  # Assign a unique value for ordering
# Combine all data for plotting
plot_data_056 <- rbind(filtered_data_056, background_data_056)
# Set categorical labels for X-axis in the desired order
plot_data_056$freq_056_label <- factor(
  paste0("811 = ", plot_data_056$freq_811, ", 056 = ", plot_data_056$freq_056),
  levels = c(
    "811 = 0, 056 = -0.1",  # Background group
    "811 = 0.5, 056 = 0.25",
    "811 = 0.5, 056 = 0.5",
    "811 = 0.5, 056 = 0.75"
  )
)

# Create the styled boxplot
final_plot_056 <- ggplot(plot_data_056, aes(x = freq_056_label, y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = freq_811), width = 0.2, size = 2, alpha = 0.8) +  # Black jittered dots
  scale_color_gradient(low = "black", high = "black") +  # Gradient for jitter points
  ylim(0, y_limit) +  # Set Y-axis limit
  labs(x = "Allele", y = "Anthocyanins (mg/g)") +  # Axis labels
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend

# Save as PDF
ggsave("Anthocyanin_Freq_056_Plot0.5.pdf", plot = final_plot_056, width = 4, height = 5)


# Spearman correlation test for freq_811 when freq_056 = 0.5
plot_data_811test<-subset(plot_data_811,!plot_data_811$freq_811_label %in% "811 = -0.1, 056 = 0")
test_811 <- cor.test(plot_data_811test$freq_811, plot_data_811test$anthocyanin_concentration_mg_per_g, method = "spearman")
plot(plot_data_811test$freq_811, plot_data_811test$anthocyanin_concentration_mg_per_g)

# Spearman correlation test for freq_056 when freq_811 = 0.5
plot_data_056test<-subset(plot_data_056,!plot_data_056$freq_056_label %in% "811 = 0, 056 = -0.1")
test_056 <- cor.test(plot_data_056test$freq_056, plot_data_056test$anthocyanin_concentration_mg_per_g, method = "spearman")





#### Association mapping with ploidy as covariate

# Association mapping using linear regression for locus 811 with ploidy as covariate
lm_811 <- lm(anthocyanin_concentration_mg_per_g ~ freq_811 + ploidy, data = merged_data)
summary(lm_811)

# Association mapping using linear regression for locus 019 with ploidy as covariate
lm_019 <- lm(anthocyanin_concentration_mg_per_g ~ freq_019, data = tetraploid_data)
summary(lm_019)

# Association mapping using linear regression for locus 056 with ploidy as covariate
lm_056 <- lm(anthocyanin_concentration_mg_per_g ~ freq_056 + ploidy, data = merged_data)
summary(lm_056)

setwd("/home/aa/pigmentation/mappingExp/anthocyanins/")
library(ggplot2)
library(dplyr)
# Read the dataset
data <- read.csv("mergedDataAll225IndivAllInfo.csv", header=TRUE)
tetraploid_data <- subset(data, ploidy == 4 & substr(seedLine, 1, 1) == "t")

p <- ggplot(data, aes(x = as.factor(freq_811), 
                             y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +  # Uniform size for all points
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) +  # Ploidy = 2 (gray), Ploidy = 4 (black)
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +  # Filled (16) for sampleN ≤ 300, empty (21) for > 300
  labs(title = "Association Mapping: Anthocyanin vs Locus 811",
       x = "Allele at Locus 811",
       y = "Anthocyanins (mg/g)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend
# Save as PDF
ggsave("boxplot_anthocyanins_Locus811_ploidy_sampleN_fixed.pdf", plot = p, width = 4, height = 5)

### NEWER visualization of association mapping
# 1. Vytvoření kombinovaného faktoru pro x-osu
# Požadované pořadí: nejdříve tetraploidy (4), potom diploidy (2)
levels_order <- c("4_0", "4_0.25", "4_0.5", "4_0.75", "4_1", 
                  "2_0", "2_0.25", "2_0.5", "2_0.75", "2_1")

data$x_var <- factor(paste(data$ploidy, data$freq_811, sep = "_"), levels = levels_order)
data$x_var <- droplevels(data$x_var) # Odstranění nevyužitých úrovní

p <- ggplot(data, aes(x = x_var, 
                      y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +  # Uniform size for all points
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) +  # Ploidy = 2 (gray), Ploidy = 4 (black)
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +  # Filled (16) for sampleN ≤ 300, empty (21) for > 300
  scale_x_discrete(labels = function(x) sub("^[24]_", "", x)) +  # Zobrazení samotných frekvencí na x-ose
  labs(title = "Association Mapping: Anthocyanin vs Locus 811",
       x = "Allele at Locus 811",
       y = "Anthocyanins (mg/g)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend

# Save as PDF
ggsave("boxplot_anthocyanins_Locus811_new.pdf", plot = p, width = 4, height = 5)



# Visualize results for locus 019
p <- ggplot(data, aes(x = as.factor(freq_019), 
                      y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +  # Uniform size for all points
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) +  # Ploidy = 2 (gray), Ploidy = 4 (black)
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +  # Filled (16) for sampleN ≤ 300, empty (21) for > 300
  labs(title = "Association Mapping: Anthocyanin vs Locus 811",
       x = "Allele at Locus 019",
       y = "Anthocyanins (mg/g)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend
# Save as PDF
ggsave("boxplot_anthocyanins_Locus019_ploidy_sampleN_fixed.pdf", plot = p, width = 4, height = 5)

##Newer
levels_order <- c("4_0", "4_0.25", "4_0.5", "4_0.75", "4_1", 
                  "2_0", "2_0.25", "2_0.5", "2_0.75", "2_1")
data$x_var <- factor(paste(data$ploidy, data$freq_019, sep = "_"), levels = levels_order)
data$x_var <- droplevels(data$x_var) # Odstranění nevyužitých úrovní

p <- ggplot(data, aes(x = x_var, 
                      y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +  # Uniform size for all points
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) +  # Ploidy = 2 (gray), Ploidy = 4 (black)
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +  # Filled (16) for sampleN ≤ 300, empty (21) for > 300
  scale_x_discrete(labels = function(x) sub("^[24]_", "", x)) +  # Zobrazení samotných frekvencí na x-ose
  labs(title = "Association Mapping: Anthocyanin vs Locus 019",
       x = "Allele at Locus 019",
       y = "Anthocyanins (mg/g)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend

# Save as PDF
ggsave("boxplot_anthocyanins_Locus019_new.pdf", plot = p, width = 4, height = 5)





# Visualize results for locus 056
p <- ggplot(data, aes(x = as.factor(freq_056), 
                      y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +  # Uniform size for all points
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) +  # Ploidy = 2 (gray), Ploidy = 4 (black)
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +  # Filled (16) for sampleN ≤ 300, empty (21) for > 300
  labs(title = "Association Mapping: Anthocyanin vs Locus 811",
       x = "Allele at Locus 056",
       y = "Anthocyanins (mg/g)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend
# Save as PDF
ggsave("boxplot_anthocyanins_Locus056_ploidy_sampleN_fixed.pdf", plot = p, width = 4, height = 5)

### NEWER
levels_order <- c("4_0", "4_0.25", "4_0.5", "4_0.75", "4_1", 
                  "2_0", "2_0.25", "2_0.5", "2_0.75", "2_1")

data$x_var <- factor(paste(data$ploidy, data$freq_056, sep = "_"), levels = levels_order)
data$x_var <- droplevels(data$x_var) # Odstranění nevyužitých úrovní

p <- ggplot(data, aes(x = x_var, 
                      y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) +  # Transparent purple boxes
  geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
              width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +  # Uniform size for all points
  scale_color_manual(values = c("2" = "gray50", "4" = "black")) +  # Ploidy = 2 (gray), Ploidy = 4 (black)
  scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +  # Filled (16) for sampleN ≤ 300, empty (21) for > 300
  scale_x_discrete(labels = function(x) sub("^[24]_", "", x)) +  # Zobrazení samotných frekvencí na x-ose
  labs(title = "Association Mapping: Anthocyanin vs Locus 056",
       x = "Allele at Locus 056",
       y = "Anthocyanins (mg/g)") +  
  theme_minimal() + 
  theme(panel.grid = element_blank(),  # Remove grid lines
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14),
        legend.position = "none")  # Remove legend

# Save as PDF
ggsave("boxplot_anthocyanins_Locus056_new.pdf", plot = p, width = 4, height = 5)



########### FOR FLAVONOLS NEWER
library(ggplot2)
library(dplyr)
library(patchwork)
setwd("/home/aa/pigmentation/SI/")

# Načtení dat
data <- read.csv("225_IndividualsLeafPetalAnthocFlavonols.csv", header = TRUE) # případně read.table / sep = "\t" podle formátu

# Definice požadovaného pořadí ploidií a frekvencí
levels_order <- c("4_0", "4_0.25", "4_0.5", "4_0.75", "4_1", 
                  "2_0", "2_0.25", "2_0.5", "2_0.75", "2_1")

loci <- c("811", "019", "056")
plot_list <- list()

# Generování grafů pro jednotlivé lokusy
for (locus in loci) {
  col_name <- paste0("freq_", locus)
  
  # Vytvoření kombinovaného faktoru pro x-osu
  data_sub <- data
  data_sub$x_var <- factor(paste(data_sub$ploidy, data_sub[[col_name]], sep = "_"), levels = levels_order)
  data_sub$x_var <- droplevels(data_sub$x_var)
  
  p <- ggplot(data_sub, aes(x = x_var, y = absorbance_360_per_mg)) +
    geom_boxplot(fill = "gold", alpha = 0.5, color = "black", outlier.shape = NA) +  # Zlatá barva pro UV/flavonoly (360 nm)
    geom_jitter(aes(color = as.factor(ploidy), shape = as.factor(sampleN > 300)), 
                width = 0.2, size = 1.5, stroke = 1, alpha = 0.8) +
    scale_color_manual(values = c("2" = "gray50", "4" = "black")) +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) +
    scale_x_discrete(labels = function(x) sub("^[24]_", "", x)) +
    labs(title = paste("Locus", locus),
         x = paste("Allele at Locus", locus),
         y = "Absorbance 360 nm / mg") +  
    theme_minimal() + 
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.position = "none")
  
  plot_list[[locus]] <- p
}

# ------------------------------------------------------------------------------
# ULOŽENÍ DO JEDNOHO SOUBORU
# ------------------------------------------------------------------------------

# Varianta A: Všechny 3 lokusy vedle sebe v jednom panelu (1 řádek × 3 sloupce)
combined_plot <- plot_list[["811"]] + plot_list[["019"]] + plot_list[["056"]]
ggsave("association_mapping_flavonols_360nm_panel.pdf", plot = combined_plot, width = 12, height = 5)

# Varianta B: Vícestránkové PDF (každý lokus na samostatné stránce)
pdf("association_mapping_flavonols_360nm_multipage.pdf", width = 4, height = 5)
for (locus in loci) {
  print(plot_list[[locus]])
}
dev.off()





# Model 1: Full model (both SNPs and ploidy as covariates)
full_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                   freq_811 + 
                   freq_056 + 
                   ploidy, 
                 data = merged_data)

# Model 1: Full model (both SNPs and ploidy as covariates)
fulla_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                   freq_811 + 
                   freq_019 + 
                    freq_056, 
                 data = merged_data)

# Model 2: SNP 811 and ploidy
model_811 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_811 + 
                  ploidy, 
                data = merged_data)

# Model 3: SNP 019 and ploidy
model_019 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_019 + 
                  ploidy, 
                data = merged_data)


# Model 4: SNP 056 and ploidy
model_056 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_056 + 
                  ploidy, 
                data = merged_data)

# Model 5: Interaction model with ploidy
interaction_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                          freq_811 * 
                          freq_056 + 
                          ploidy, 
                        data = merged_data)

# Compare models using ANOVA
anova(full_model, model_811)
anova(full_model, model_019)
anova(full_model, model_056)
anova(full_model, interaction_model)

# Compare models using AIC
AIC(full_model, fulla_model, model_811,model_019, model_056, interaction_model)

# Summarize models
summary(full_model)
summary(fulla_model)
summary(interaction_model)

############## Association mapping DIPLOIDS
merged_datat<-subset(merged_data,merged_data$ploidy == 2)
lm_811 <- lm(anthocyanin_concentration_mg_per_g ~ freq_811, data = merged_datat)
summary(lm_811)
# Association mapping using linear regression for locus 019 with ploidy as covariate
lm_019 <- lm(anthocyanin_concentration_mg_per_g ~ freq_019, data = merged_datat)
summary(lm_019)
# Association mapping using linear regression for locus 056 with ploidy as covariate
lm_056 <- lm(anthocyanin_concentration_mg_per_g ~ freq_056, data = merged_datat)
summary(lm_056)

# Visualize results for locus 811
library(ggplot2)
ggplot(merged_datat, aes(x = as.factor(freq_811), 
                        y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Diploids",
       x = "AF pink, locus 811",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()
# Visualize results for locus 019
ggplot(merged_datat, aes(x = as.factor(freq_019), 
                         y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Diploids",
       x = "AF pink, locus 019",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()
# Visualize results for locus 056
ggplot(merged_datat, aes(x = as.factor(freq_056), 
                         y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Diploids",
       x = "AF pink, locus 056",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()

# Model 1: Full model (both SNPs and ploidy as covariates)
full_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                   freq_811 + 
                   freq_056,
                 data = merged_datat)
# Model 1: Full model (both SNPs and ploidy as covariates)
fulla_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                    freq_811 + 
                    freq_019 + 
                    freq_056, 
                  data = merged_datat)
# Model 2: SNP 811 and ploidy
model_811 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_811, 
                data = merged_datat)
# Model 3: SNP 019 and ploidy
model_019 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_019, 
                data = merged_datat)
# Model 4: SNP 056 and ploidy
model_056 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_056, 
                data = merged_datat)
# Model 5: Interaction model with ploidy
interaction_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                          freq_811 * 
                          freq_019 *
                          freq_056, 
                        data = merged_datat)
# Compare models using ANOVA
anova(full_model, model_811)
anova(full_model, model_019)
anova(full_model, model_056)
anova(full_model, interaction_model)
# Compare models using AIC
AIC(full_model, fulla_model, model_811,model_019, model_056, interaction_model)
# Summarize models
summary(full_model)
summary(fulla_model)
summary(interaction_model)

############## Association mapping TETRAPLOIDS
merged_datat<-subset(merged_data,merged_data$ploidy == 4)
lm_811 <- lm(anthocyanin_concentration_mg_per_g ~ freq_811, data = merged_datat)
summary(lm_811)
# Association mapping using linear regression for locus 019 with ploidy as covariate
lm_019 <- lm(anthocyanin_concentration_mg_per_g ~ freq_019, data = merged_datat)
summary(lm_019)
# Association mapping using linear regression for locus 056 with ploidy as covariate
lm_056 <- lm(anthocyanin_concentration_mg_per_g ~ freq_056, data = merged_datat)
summary(lm_056)
# Visualize results for locus 811
library(ggplot2)
ggplot(merged_datat, aes(x = as.factor(freq_811), 
                         y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 811",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()
# Visualize results for locus 019
ggplot(merged_datat, aes(x = as.factor(freq_019), 
                         y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 019",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()
# Visualize results for locus 056
ggplot(merged_datat, aes(x = as.factor(freq_056), 
                         y = anthocyanin_concentration_mg_per_g)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 056",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()

# Model 1: Full model (both SNPs and ploidy as covariates)
full_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                   freq_811 + 
                   freq_056,
                 data = merged_datat)
# Model 1: Full model (both SNPs and ploidy as covariates)
fulla_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                    freq_811 + 
                    freq_019 + 
                    freq_056, 
                  data = merged_datat)
# Model 2: SNP 811 and ploidy
model_811 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_811, 
                data = merged_datat)
# Model 3: SNP 019 and ploidy
model_019 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_019, 
                data = merged_datat)
# Model 4: SNP 056 and ploidy
model_056 <- lm(anthocyanin_concentration_mg_per_g ~ 
                  freq_056, 
                data = merged_datat)
# Model 5: Interaction model with ploidy
interaction_model <- lm(anthocyanin_concentration_mg_per_g ~ 
                          freq_811 * 
                          freq_019 *
                          freq_056, 
                        data = merged_datat)
# Compare models using ANOVA
anova(full_model, model_811)
anova(full_model, model_019)
anova(full_model, model_056)
anova(full_model, interaction_model)
# Compare models using AIC
AIC(full_model, fulla_model, model_811,model_019, model_056, interaction_model)
# Summarize models
summary(full_model)
summary(fulla_model)
summary(interaction_model)

#####################
############## Association mapping SEEDLINGS TETRAPLOIDS
merged_datat<-subset(merged_data,merged_data$ploidy == 4)
merged_datat<-subset(merged_datat,!merged_datat$seedlingColor %in% "na")

lm_811 <- lm(seedlingColor ~ freq_811, data = merged_datat)
summary(lm_811)
# Association mapping using linear regression for locus 019 with ploidy as covariate
lm_019 <- lm(seedlingColor ~ freq_019, data = merged_datat)
summary(lm_019)
# Association mapping using linear regression for locus 056 with ploidy as covariate
lm_056 <- lm(seedlingColor ~ freq_056, data = merged_datat)
summary(lm_056)
# Visualize results for locus 811
# Scatterplot with jitter for ordinal variables
library(ggplot2)
ggplot(merged_datat, aes(x = as.factor(freq_811), 
                        y = seedlingColor)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7, color = "blue") +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 811",
       y = "Seedling color") +
  theme_minimal()

# Visualize results for locus 019
# Scatterplot with jitter for ordinal variables
library(ggplot2)
ggplot(merged_datat, aes(x = as.factor(freq_019), 
                        y = seedlingColor)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7, color = "blue") +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 019",
       y = "Seedling color") +
  theme_minimal()
# Visualize results for locus 056
# Scatterplot with jitter for ordinal variables
library(ggplot2)
ggplot(merged_datat, aes(x = as.factor(freq_056), 
                        y = seedlingColor)) +
  geom_jitter(width = 0.2, height = 0.2, alpha = 0.7, color = "blue") +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 056",
       y = "Seedling color") +
  theme_minimal()

write.table(treatment_data,"treatmentData.txt",row.names = F)
write.table(merged_data,"mergedData.txt",row.names = F)




############ LEAF ASSOCIATION MAPPING
# Load necessary libraries
library(dplyr)

merged_data<-read.table("file:///home/aa/pigmentation/mappingExp/anthocyanins/mergedDataPetalLeaf.txt", header = TRUE)
merged_data <- merged_data %>%
  mutate(
    # Count 'p' occurrences in G_811, G_019, and G_056
    N_811 = stringr::str_count(G_811, "p"),
    N_019 = stringr::str_count(G_019, "p"),
    N_056 = stringr::str_count(G_056, "p")
  )
merged_data <- merged_data %>%
  rowwise() %>%
  mutate(
    ploidy = ifelse(nchar(G_811) == 2, 2, 4),
    freq_811 = N_811 / ploidy,
    freq_019 = N_019 / ploidy,
    freq_056 = N_056 / ploidy
  ) %>%
  ungroup()


library(tidyverse)
library(data.table)

# Filtrace na linie začínající "t" (podle vašeho zadání)
merged_data_t <- merged_data %>% filter(str_detect(seedLine, "^t"))

# --- 2. FUNKCE PRO VIZUALIZACI ---
# Tato funkce vytvoří graf pro zadaný lokus a uloží jej do PDF
plot_locus_association <- function(data, locus_name, freq_col) {
  
  # Dynamický název souboru a titulku
  file_name <- paste0("boxplot_anthocyanins_Locus", locus_name, "_ploidy_sampleN.pdf")
  title_text <- paste("Association Mapping: Anthocyanins vs Locus", locus_name)
  
  p <- ggplot(data, aes(x = as.factor(.data[[freq_col]]), 
                        y = anthocyanin_concentration_mg_per_gleaf)) +
    # Boxploty: transparentní fialová
    geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA) + 
    # Body: barva dle ploidie, tvar dle sampleN
    geom_jitter(aes(color = as.factor(ploidy), 
                    shape = as.factor(sampleN > 300)), 
                width = 0.2, size = 1.8, stroke = 1, alpha = 0.8) + 
    # Nastavení barev: 2x = šedá, 4x = černá
    scale_color_manual(values = c("2" = "gray50", "4" = "black")) + 
    # Nastavení tvarů: plné (16) pro ≤ 300, prázdné (21) pro > 300
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 21)) + 
    labs(title = title_text,
         x = paste("Allele Frequency at Locus", locus_name),
         y = "Leaf Anthocyanins (mg/g)") +  
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14),
          legend.position = "none") # Legenda odstraněna dle požadavku
  
  # Uložení grafu
  ggsave(file_name, plot = p, width = 4, height = 5)
  
  return(p)
}

# GENEROVÁNÍ GRAFŮ PRO VŠECHNY 3 LOKUSY ---
# Volání funkce pro každý lokus
plot_056 <- plot_locus_association(merged_data, "056", "freq_056")
plot_811 <- plot_locus_association(merged_data, "811", "freq_811")
plot_019 <- plot_locus_association(merged_data, "019", "freq_019")

# --- 4. STATISTICKÉ MODELY (pro kontrolu v konzoli) ---
# Používám název sloupce 'anthocyanin_concentration_mg_per_g_leaf' z CSV
models <- list(
  lm_811 = lm(anthocyanin_concentration_mg_per_gleaf ~ freq_811, data = merged_data_t),
  lm_019 = lm(anthocyanin_concentration_mg_per_gleaf ~ freq_019, data = merged_data_t),
  lm_056 = lm(anthocyanin_concentration_mg_per_gleaf ~ freq_056, data = merged_data_t)
)
# Výpis výsledků
lapply(models, summary)



# Model 1: Full model (both SNPs and ploidy as covariates)
full_model <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                   freq_811 + 
                   freq_056, 
                 data = merged_data)

# Model 1: Full model (both SNPs and ploidy as covariates)
fulla_model <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                    freq_811 + 
                    freq_019 + 
                    freq_056, 
                  data = merged_data)

# Model 2: SNP 811 and ploidy
model_811 <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                  freq_811 + 
                  ploidy, 
                data = merged_data)

# Model 3: SNP 019 and ploidy
model_019 <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                  freq_019 + 
                  ploidy, 
                data = merged_data)


# Model 4: SNP 056 and ploidy
model_056 <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                  freq_056 + 
                  ploidy, 
                data = merged_data)

# Model 5: Interaction model with ploidy
interaction_model <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                          freq_811 * 
                          freq_056 + 
                          ploidy, 
                        data = merged_data)

# Compare models using ANOVA
anova(full_model, model_811)
anova(full_model, model_019)
anova(full_model, model_056)
anova(full_model, interaction_model)

# Compare models using AIC
AIC(full_model, fulla_model, model_811,model_019, model_056, interaction_model)

# Summarize models
summary(full_model)
summary(fulla_model)
summary(interaction_model)

############## Association mapping DIPLOIDS
merged_datat<-subset(merged_data,merged_data$ploidy == 2)


############## Association mapping TETRAPLOIDS
merged_datat<-subset(merged_data,substr(merged_data$seedLine,1,1) == "t")
p <- ggplot(merged_datat, aes_string(x = "anthocyanin_concentration_mg_per_gpetal", y = "anthocyanin_concentration_mg_per_gleaf", label = "freq_056")) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  geom_text(vjust = -0.5, hjust = 0.5, size = 3) +  # Adjust text position and size
  labs(title = paste("Correlation petal - leaf"),
       x = "anthocyanin_concentration_mg_per_gpetal",
       y = "anthocyanin_concentration_mg_per_gleaf") +
  theme_minimal()
print(p)




lm_811 <- lm(anthocyanin_concentration_mg_per_gleaf ~ freq_811, data = merged_datat)
summary(lm_811)
# Association mapping using linear regression for locus 019 with ploidy as covariate
lm_019 <- lm(anthocyanin_concentration_mg_per_gleaf ~ freq_019, data = merged_datat)
summary(lm_019)
# Association mapping using linear regression for locus 056 with ploidy as covariate
lm_056 <- lm(anthocyanin_concentration_mg_per_gleaf ~ freq_056, data = merged_datat)
summary(lm_056)

lm_int <- lm(anthocyanin_concentration_mg_per_gleaf ~ freq_811*freq_056, data = merged_datat)
summary(lm_int)


# Visualize results for locus 811
library(ggplot2)
ggplot(merged_datat, aes(x = as.factor(freq_811), 
                         y = anthocyanin_concentration_mg_per_gleaf)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 811",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()
# Visualize results for locus 019
ggplot(merged_datat, aes(x = as.factor(freq_019), 
                         y = anthocyanin_concentration_mg_per_gleaf)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 019",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()
# Visualize results for locus 056
ggplot(merged_datat, aes(x = as.factor(freq_056), 
                         y = anthocyanin_concentration_mg_per_gleaf)) +
  geom_boxplot() +
  labs(title = "Association Mapping: Tetraploids",
       x = "AF pink, locus 056",
       y = "Anthocyanin concentration (mg/g)") +
  theme_minimal()

# Model 1: Full model (both SNPs and ploidy as covariates)
full_model <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                   freq_811 + 
                   freq_056,
                 data = merged_datat)
# Model 1: Full model (both SNPs and ploidy as covariates)
fulla_model <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                    freq_811 + 
                    freq_019 + 
                    freq_056, 
                  data = merged_datat)
# Model 2: SNP 811 and ploidy
model_811 <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                  freq_811, 
                data = merged_datat)
# Model 3: SNP 019 and ploidy
model_019 <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                  freq_019, 
                data = merged_datat)
# Model 4: SNP 056 and ploidy
model_056 <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                  freq_056, 
                data = merged_datat)
# Model 5: Interaction model with ploidy
interaction_model <- lm(anthocyanin_concentration_mg_per_gleaf ~ 
                          freq_811 * 
                          freq_019 *
                          freq_056, 
                        data = merged_datat)
# Compare models using ANOVA
anova(full_model, model_811)
anova(full_model, model_019)
anova(full_model, model_056)
anova(full_model, interaction_model)
# Compare models using AIC
AIC(full_model, fulla_model, model_811,model_019, model_056, interaction_model)
# Summarize models
summary(full_model)
summary(fulla_model)
summary(interaction_model)

#fwrite(merged_datat,"tetraploidsAllinfo.txt")
#




######## COMPARING TREATMENTS
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
setwd("/home/aa/pigmentation/mappingExp/anthocyanins")
#Remove the treatment suffix from `sampleN` in treatment_data
treatment_data<-read.table("/home/aa/pigmentation/mappingExp/anthocyanins/treatmentDataLL.txt",sep = ",",h=T)
merged_data<-read.table("/home/aa/pigmentation/mappingExp/anthocyanins/mergedData.txt",sep = ",",h=T)
treatment_data <- treatment_data %>%
  mutate(sampleN_base = gsub("(ll|hl)$", "", sampleN)) 
# Merge tables using the base `sampleN` from treatment_data and `sampleN` from merged_data
combined_data <- merge(
  treatment_data,
  merged_data,
  by.x = "sampleN_base",
  by.y = "sampleN",
  suffixes = c(".treatment", ".merged")
)
combined_data <- combined_data %>%
  mutate(
    treatment_group = case_when(
      grepl("ll$", sampleN) ~ "low")
  )
# Step 3: Reshape data to have one row per sample and treatment
reshaped_data <- combined_data %>%
  select(
    sampleN_base, sampleN,
    anthocyanin_concentration_mg_per_g.treatment, 
    anthocyanin_concentration_mg_per_g.merged, 
    treatment_group
  ) %>%
  pivot_longer(
    cols = c(anthocyanin_concentration_mg_per_g.treatment, anthocyanin_concentration_mg_per_g.merged),
    names_to = "treatment_type",
    values_to = "anthocyanin_concentration"
  ) %>%
  mutate(
    treatment_type = case_when(
      treatment_type == "anthocyanin_concentration_mg_per_g.treatment" ~ treatment_group,
      treatment_type == "anthocyanin_concentration_mg_per_g.merged" ~ "normal"
    )
  )
fwrite(reshaped_data,"treatmentDataFinal.txt")

# Read the dataset
data <- read.csv("treatmentDataFinal.txt", header=TRUE)
data$region <- ifelse(data$sampleN_base > 300, "foothill", "alpine")
# Convert region to factor with correct order
data$region <- factor(data$region, levels = c("foothill", "alpine"))
# Create the paired scatter plot
plot <- ggplot(data, aes(x = factor(treatment_type, levels = c("low", "normal")), 
                         y = anthocyanin_concentration, 
                         group = sampleN_base)) +
  geom_point(aes(color = treatment_type), size = 3) +  # Points colored by treatment type
  geom_line(aes(group = sampleN_base), linetype = "dashed", color = "grey") +  # Dashed lines for the same sample
  facet_wrap(~ region, scales = "fixed") +  # Fixed Y-axis across both facets
  scale_color_manual(
    values = c("low" = "blue", "normal" = "red"),
    labels = c("Low Light", "High Light")
  ) +
  labs(
    title = "Effect of Light Treatment on Anthocyanin Concentration",
    x = "Treatment",
    y = "Anthocyanin Concentration (mg/g)",
    color = "Treatment Type"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
# Save the plot as a PDF
ggsave("Anthocyanin_Treatment_Comparison.pdf", plot = plot, width = 10, height = 6, device = "pdf")

foothill<-subset(data,data$region %in% "foothill")
wilcox.test(subset(foothill,foothill$treatment_type %in% "low")$anthocyanin_concentration,subset(foothill,foothill$treatment_type %in% "normal")$anthocyanin_concentration, alternative = "greater")


### GWAS interpretation
library(data.table)
gwas<-fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/results/colCompGWAS19Jun.outGenes.0.95.0.3.txt")
dict<-fread("/home/aa/BetyProject/claraMS/cuticle/thePipeline/data/ALATdict.txt")
colnames(dict)<-c("Var1","AT")
data<-merge(x = gwas,y = dict,by = "Var1")
write.csv(data,"/home/aa/pigmentation/GWAScolor/gwasTop.csv")






# ---- balíčky ----
library(readxl)
library(dplyr)
library(ggplot2)
library(rstatix)   # pro wilcox_test a stat_pvalue_manual
library(stringr)

# ---- načtení dat ----
# uprav filename/sheet podle sebe:
setwd("/home/aa/pigmentation/termocamTatry/")
df <- read.csv("/home/aa/pigmentation/termocamTatry/measurmentsArenosa_edited.csv")

# rychlé čištění a nastavení typů
df <- df %>%
  mutate(
    timeSlot = as.factor(timeSlot),
    color    = factor(color, levels = c("W", "P")), # pořadí: W, P
    temp     = as.numeric(temp)
  ) %>%
  filter(!is.na(color), !is.na(temp), !is.na(timeSlot))

# ---- Wilcoxon pro každý timeSlot ----
wilx <- df %>%
  group_by(timeSlot) %>%
  wilcox_test(temp ~ color, detailed = TRUE) %>%
  # pozice popisku nad maximem v daném panelu
  left_join(
    df %>% group_by(timeSlot) %>% summarise(y = max(temp, na.rm = TRUE) + 0.7),
    by = "timeSlot"
  ) %>%
  mutate(
    group1 = "W", group2 = "P",
    p_label = paste0("Wilcoxon p = ", signif(p, 3))
  )

# ---- boxplot: temp vs color (facet podle timeSlot) ----
p <- ggplot(df, aes(x = color, y = temp, fill = color)) +
  geom_boxplot(alpha = 0.8, outlier.shape = NA) +
  geom_jitter(width = 0.15, size = 1.3, alpha = 0.85, color = "black") +
  facet_wrap(~ timeSlot, nrow = 1) +
  scale_fill_manual(values = c("W" = "white", "P" = "#f6a6c1")) +
  labs(
    x = "Color",
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )
print(p)
# uložení grafu
ggsave("temp_W_vs_P_by_timeSlot.pdf", plot = p, width = 8, height = 4.2)



# ---- průměrný rozdíl teplot P - W ----
# po timeSlotu
avg_diff_by_slot <- df %>%
  group_by(timeSlot) %>%
  summarise(
    mean_W = mean(temp[color == "W"], na.rm = TRUE),
    mean_P = mean(temp[color == "P"], na.rm = TRUE),
    diff_P_minus_W = mean_P - mean_W,
    n_W = sum(color == "W"),
    n_P = sum(color == "P"),
    .groups = "drop"
  )
print(avg_diff_by_slot)



############## OTHER SPECIES
## --- Load data (adjust read.csv2 if needed) ---
df <- read.csv("~/pigmentation/termocamTatrypollenAssay/otherSpecies/BrightnessStandSpecies.csv", stringsAsFactors = FALSE)

## --- Color order: red -> blue -> yellow -> purple/pink -> green -> white ---
color_levels <- c("red","blue","yellow","purple/pink","green","white")
color_levels <- c("red","blue","purple/pink","white")

df<-subset(df,df$Color %in% color_levels)
df$Color <- factor(df$Color, levels = color_levels)

## --- Order species by Color, then by median Mean_std within color (desc) ---
meds <- aggregate(Mean_std ~ Species + Color, df, median, na.rm = TRUE)

# Put unknown/NA colors to the end
meds$Color_ord <- as.integer(meds$Color)
meds$Color_ord[is.na(meds$Color_ord)] <- length(color_levels) + 1

# Sort: first by Color order, then by descending median Mean_std
meds <- meds[order(meds$Color_ord, -meds$Mean_std), ]

species_levels <- meds$Species
df$Species <- factor(df$Species, levels = species_levels)

## --- Plot: boxplot + individual data points (no title) ---
pdf("/home/aa/pigmentation/termocamTatrypollenAssay/otherSpecies/BrightnessStandSpeciesJun26.pdf",width = 9, height = 9)
op <- par(mar = c(10, 4, 1, 1))  # room for long x labels
boxplot(Mean_std ~ Species, data = df,
        las = 2, outline = FALSE,
        ylab = "Standardized mean temperature (sample - control)",
        main = "")
stripchart(Mean_std ~ Species, data = df,
           vertical = TRUE, method = "jitter",
           pch = 16, cex = 0.7,
           col = adjustcolor("black", alpha.f = 0.5),
           add = TRUE)
par(op)
dev.off()

## =========================================================================
## --- Statistical Testing: Does temperature (Mean_std) relate to Color? ---
## =========================================================================

# -------------------------------------------------------------------------
# APPROACH 1: Aggregated Approach (Species level)
# Highly recommended to avoid pseudoreplication since 'Color' is a species-level trait.
# Uses the 'meds' data frame which contains exactly 1 median value per species.
# -------------------------------------------------------------------------

# Run a standard One-Way ANOVA on species medians
anova_model <- aov(Mean_std ~ Color, data = meds)
cat("--- One-way ANOVA on Species Medians ---\n")
print(summary(anova_model))

# Run a post-hoc Tukey HSD test if the ANOVA is significant (p < 0.05)
cat("\n--- Tukey Honest Significant Difference (Post-hoc) ---\n")
print(TukeyHSD(anova_model))

# Non-parametric alternative (Kruskal-Wallis test) on species medians
# Useful if sample size is very small or normality assumptions are violated
cat("\n--- Kruskal-Wallis Test on Species Medians ---\n")
print(kruskal.test(Mean_std ~ Color, data = meds))


# -------------------------------------------------------------------------
# APPROACH 2: Linear Mixed-Effects Model (LMM)
# Uses the full dataset 'df' but treats 'Species' as a random effect
# to properly account for the nesting of individual measurements.
# -------------------------------------------------------------------------

# Load the nlme package (standard package included in R)
library(nlme)

# Fit the mixed model: Color as a fixed effect, Species as a random intercept
lme_model <- lme(Mean_std ~ Color, random = ~ 1 | Species, data = df)
cat("\n--- Linear Mixed-Effects Model (LMM) ANOVA Table ---\n")
print(anova(lme_model))




################################### HEATING
# Load required libraries
library(tidyverse)
library(stringr)

setwd("/home/aa/pigmentation/termocamTatrypollenAssay/UEBspring26/")
# Define path to directory
data_dir <- "/home/aa/pigmentation/termocamTatrypollenAssay/UEBspring26/"
files <- list.files(path = data_dir, pattern = "\\.csv$")

# Helper function to parse filename metadata using regex
parse_filename <- function(filepath) {
  filename <- basename(filepath)
  # Extract components based on pattern: color(1char), light(2chars), plant(r+digits), flower(f+digits)
  meta <- str_match(filename, "^([wp])([a-z]{2})(r\\d+)(f\\d+)")
  
  if (all(is.na(meta))) return(NULL)
  
  data.frame(
    filepath = filepath,
    color = meta[2],
    light = meta[3],
    plant = meta[4],
    flower = meta[5],
    id = paste0(meta[2], meta[4], meta[5]),
    stringsAsFactors = FALSE
  )
}

# Combine metadata for all files
file_metadata <- map_df(files, parse_filename)

# -------------------------------------------------------------------------
# Data Loading and Processing (Steps 1, 2 & 3)
# -------------------------------------------------------------------------

all_data <- list()
summary_list <- list()

for (i in 1:nrow(file_metadata)) {
  meta <- file_metadata[i, ]
  
  # Read CSV (skip header, 4th column contains the mean temperature)
  df <- read.csv(meta$filepath, header = TRUE)
  colnames(df)[4] <- "temp"
  
  # Add relative time index (0-indexed seconds based on row position)
  df$time_seq <- 0:(nrow(df) - 1)
  
  # CUT: Keep data up to 120 seconds for calculations
  df <- df %>% filter(time_seq <= 100)
  
  df$color <- meta$color
  df$light <- substr(meta$light,1,1)
  df$id <- meta$id
  
  # Calculations for Step 2: Base and Max temperature
  # Base: average of first 9 seconds (rows 1 to 9, index 0 to 8)
  base_temp <- mean(df$temp[1:9], na.rm = TRUE)
  
  # RELATIVE TEMP: Calculate difference from base for visualization
  df$temp_rel <- df$temp - base_temp
  
  # Store for plotting (Step 1)
  all_data[[i]] <- df %>% select(time_seq, temp, temp_rel, color, light, id)
  
  # Max calculated ONLY from interval 10 - 70s (index 10 to 70)
  heating_phase <- df %>% filter(time_seq > 10 & time_seq < 70)
  max_temp <- mean(sort(heating_phase$temp, decreasing = TRUE)[1:10], na.rm = TRUE)
  diff_temp <- max_temp - base_temp
  
  # Calculations for Step 3: Heating speed (minus 10s baseline)
  time_above_max <- df$time_seq[which(df$temp >= max_temp)[1]] - 10
  heating_speed <- if(!is.na(time_above_max)) time_above_max else NA
  
  summary_list[[i]] <- data.frame(
    color = meta$color,
    light = substr(meta$light,1,1),
    plant = meta$plant, # Included plant variable for paired analysis
    id = meta$id,
    base_temp = base_temp,
    max_temp = max_temp,
    diff_temp = diff_temp,
    heating_speed = heating_speed,
    stringsAsFactors = FALSE
  )
}

plot_data <- bind_rows(all_data)
summary_data <- bind_rows(summary_list)

# -------------------------------------------------------------------------
# Step 1: Visualization & PDF Export
# -------------------------------------------------------------------------
plot_data$light2<-substr(plot_data$light,1,1)
pdf("flower_heating_curves2.pdf", width = 11, height = 8.5)

light_sources <- unique(plot_data$light2) # light
 
for (l_src in light_sources) {
  p_data <- filter(plot_data, light2 == l_src)
  
  # NEW: Limit visualization data strictly to 100 seconds
  p_data <- filter(p_data, time_seq <= 100)
  p_data <- subset(p_data, p_data$temp_rel < 10)
  
  # Get positions for direct labeling at exactly 100 seconds
  label_data <- p_data %>%
    group_by(id) %>%
    filter(time_seq == max(time_seq)) %>%
    ungroup()
  
  p <- ggplot(p_data, aes(x = time_seq, y = temp_rel, group = id, color = color)) +
    geom_line(alpha = 0.7, size = 0.8) +
    # Highlight experimental phases (10s and 70s)
    geom_vline(xintercept = c(10, 70), linetype = "dashed", color = "gray40") +
    # LABELS: Positioned at x = 101 (just after the 100s limit)
    geom_text(data = label_data, aes(label = id), x = 101, hjust = 0, size = 2.5, show.legend = FALSE) +
    scale_color_manual(values = c("p" = "deeppink2", "w" = "dodgerblue3"),
                       labels = c("p" = "Pink (p)", "w" = "White (w)")) +
    # Adjust X axis limits to 112 to accommodate the labels properly
    scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 20)) +
    labs(title = paste("Light Source:", toupper(l_src)),
         x = "Time (seconds)",
         y = "Temperature Difference from Base (°C)",
         color = "Flower Color") +
    theme_minimal() +
    theme(title = element_text(face = "bold", size = 14),
          panel.grid.minor = element_blank())
  print(p)
}
dev.off()

# -------------------------------------------------------------------------
# Step 4: Statistical Testing & Descriptive Statistics
# -------------------------------------------------------------------------

cat("\n=== STATISTICAL ANALYSIS & DESCRIPTIVE STATISTICS ===\n")
for (l_src in light_sources) { # l_src = "u"
  cat("\n--------------------------------------------------\n")
  cat("LIGHT SOURCE:", toupper(l_src), "\n")
  cat("--------------------------------------------------\n")
  
  sub_summary <- filter(summary_data, light == l_src)
  
  if (length(unique(sub_summary$color)) == 2) {
    
    # --- ANALYSIS FOR UV & VI (Independent, One-sided Hypothesis) ---
    if (l_src %in% c("u", "v")) {
      
      cat("Descriptive Statistics (Means & SD):\n")
      means_summary <- sub_summary %>%
        group_by(color) %>%
        summarise(
          count = n(),
          mean_diff_temp = mean(diff_temp, na.rm = TRUE),
          sd_diff_temp   = sd(diff_temp, na.rm = TRUE),
          mean_speed     = mean(heating_speed, na.rm = TRUE),
          sd_speed       = sd(heating_speed, na.rm = TRUE),
          .groups = "drop"
        )
      print(as.data.frame(means_summary))
      cat("\n")
      
      # 1. Temp Difference: alternative="greater" tests if 'p' has higher temp difference than 'w'
      w_diff <- wilcox.test(diff_temp ~ color, data = sub_summary, alternative = "greater")
      cat("1. One-sided Wilcoxon Test (p > w): Temp Difference (Max - Base):\n")
      print(w_diff)
      
      # 2. Heating Speed: alternative="less" tests if 'p' takes fewer seconds to reach max than 'w'
      w_speed <- wilcox.test(heating_speed ~ color, data = sub_summary, alternative = "less")
      cat("2. One-sided Wilcoxon Test (p < w): Heating Speed (Time to reach max):\n")
      print(w_speed)
      
      # --- ANALYSIS FOR GL & NG (Paired Design by Plant) ---
    } else if (l_src %in% c("g", "n")) {
      
      cat("Descriptive Statistics (Averaged by Plant first to account for pairing):\n")
      # First, average multiple flowers within each unique Plant-Color pair to avoid pseudo-replication
      plant_summary <- sub_summary %>%
        group_by(plant, color) %>%
        summarise(
          plant_diff_temp = mean(diff_temp, na.rm = TRUE),
          plant_speed     = mean(heating_speed, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Second, compute grand averages and SD across the plants
      means_summary <- plant_summary %>%
        group_by(color) %>%
        summarise(
          count_plants   = n(),
          mean_diff_temp = mean(plant_diff_temp, na.rm = TRUE),
          sd_diff_temp   = sd(plant_diff_temp, na.rm = TRUE),
          mean_speed     = mean(plant_speed, na.rm = TRUE),
          sd_speed       = sd(plant_speed, na.rm = TRUE),
          .groups = "drop"
        )
      print(as.data.frame(means_summary))
      cat("\n")
      
      # Pivot wide to cleanly pair 'p' and 'w' observations for each plant
      paired_data <- plant_summary %>%
        pivot_wider(id_cols = plant, names_from = color, values_from = c(plant_diff_temp, plant_speed))
      
      # Retain only complete pairs
      paired_data <- paired_data %>% filter(!is.na(plant_diff_temp_p) & !is.na(plant_diff_temp_w))
      
      if (nrow(paired_data) >= 2) {
        # 1. Paired Test for Temp Difference
        w_diff <- wilcox.test(paired_data$plant_diff_temp_p, paired_data$plant_diff_temp_w, paired = TRUE)
        cat("1. Paired Wilcoxon Test (by Plant): Temp Difference (Max - Base):\n")
        print(w_diff)
        
        # 2. Paired Test for Heating Speed
        w_speed <- wilcox.test(paired_data$plant_speed_p, paired_data$plant_speed_w, paired = TRUE)
        cat("2. Paired Wilcoxon Test (by Plant): Heating Speed (Time to reach max):\n")
        print(w_speed)
      } else {
        cat("Not enough complete pairs to perform a paired test for this light source.\n")
      }
    }
    
  } else {
    cat("Skipping statistical test: Missing one of the color groups for this light source.\n")
  }
}



cat("\n=== STATISTICAL ANALYSIS & DESCRIPTIVE STATISTICS ===\n")
for (l_src in light_sources) {
  cat("\n--------------------------------------------------\n")
  cat("LIGHT SOURCE:", toupper(l_src), "\n")
  cat("--------------------------------------------------\n")
  
  sub_summary <- filter(summary_data, light == l_src)
  
  # Calculate and print means and SDs for each color group
  cat("Descriptive Statistics (Means & SD):\n")
  means_summary <- sub_summary %>%
    group_by(color) %>%
    summarise(
      count = n(),
      mean_diff_temp = mean(diff_temp, na.rm = TRUE),
      sd_diff_temp   = sd(diff_temp, na.rm = TRUE),
      mean_speed     = mean(heating_speed, na.rm = TRUE),
      sd_speed       = sd(heating_speed, na.rm = TRUE),
      .groups = "drop"
    )
  print(as.data.frame(means_summary))
  cat("\n")
  
  # Check if we have both colors to perform a comparison test
  if (length(unique(sub_summary$color)) == 2) {
    
    # Test 1: Temperature Difference (diff_temp)
    w_diff <- wilcox.test(diff_temp ~ color, data = sub_summary)
    cat("1. Wilcoxon Test: Temp Difference (Max - Base):\n")
    print(w_diff)
    
    # Test 2: Heating Speed
    w_speed <- wilcox.test(heating_speed ~ color, data = sub_summary)
    cat("2. Wilcoxon Test: Heating Speed (Time to reach max):\n")
    print(w_speed)
    
  } else {
    cat("Skipping statistical test: Missing one of the color groups for this light source.\n")
  }
}

# -------------------------------------------------------------------------
# NEW SECTION: Export All Summary Results to a CSV Table
# -------------------------------------------------------------------------
write.csv(summary_data, "flower_heating_summary_results.csv", row.names = FALSE)
write.csv(plot_data[,1:6], "flower_heating_all_primary_data.csv", row.names = FALSE)



########### BOXPLOT
library(tidyverse)
# Nastavení pracovního adresáře
setwd("~/pigmentation/termocamTatrypollenAssay/UEBspring26")

# =========================================================================
# ČÁST 1: Boxploty pro kontrolované ohřevy (U, V, G) z termokamery
# =========================================================================

# 1. Načtení dat ohřevu
summary_data <- read.csv("97flower_heating_summary_results.csv", header = TRUE, stringsAsFactors = FALSE)

# 2. Filtrace a úprava pořadí (nejdříve W, potom P)
plot_subset <- summary_data %>%
  filter(light %in% c("u", "v", "g")) %>%
  mutate(
    Group_Combination = paste0(toupper(light), " (", color, ")"),
    # Definice pořadí na ose X: nejdřív bílá (w), pak růžová (p)
    Group_Combination = factor(Group_Combination, levels = c(
      "U (w)", "U (p)", 
      "V (w)", "V (p)", 
      "G (w)", "G (p)"
    )),
    color = factor(color, levels = c("w", "p"))
  )

# 3. Tvorba grafu (tečky jsou plně vybarvené podle barvy květu)
p_diff_temp_boxplot <- ggplot(plot_subset, aes(x = Group_Combination, y = diff_temp, fill = color)) +
  geom_boxplot(alpha = 0.4, color = "black", outlier.shape = NA, width = 0.35, show.legend = FALSE) +  
  # shape = 21 umožňuje plné vybarvení vnitřku (fill) s tenkým černým okrajem (color)
  geom_jitter(aes(fill = color), shape = 21, width = 0.1, size = 1.2, color = "black", alpha = 0.8, show.legend = FALSE) +  
  scale_fill_manual(values = c("w" = "white", "p" = "deeppink2")) +
  labs(
    x = "Light Source & Flower Color", 
    y = "Temperature Difference from Base (°C)"
  ) +  
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5), 
    axis.text.y = element_text(size = 11),  
    axis.title = element_text(size = 13)
  )

# Export do PDF (šířka upravena na 5, aby se 6 kategorií nemačkalo)
pdf("flower_diff_temp_boxplots.pdf", width = 5, height = 4.5)
print(p_diff_temp_boxplot)
dev.off()


# =========================================================================
# ČÁST 2: Boxploty pro terénní měření (Strba & Vodopad Skok)
# =========================================================================

# 1. Načtení terénních dat z nové tabulky
loc_data <- read.csv("~/pigmentation/SI/101measurmentsFieldTemperTatry.csv", header = TRUE, stringsAsFactors = FALSE)

# 2. Filtrace požadovaných lokalit a sjednocení formátu (velká/malá písmena)
loc_subset <- loc_data %>%
  filter(locality %in% c("Strba", "Vodopad Skok", "Prague")) %>%
  mutate(
    # Převedení barev na malá písmena pro konzistenci s první částí (P -> p, W -> w)
    color_clean = tolower(color),
    
    # Vytvoření kombinace pro osu X (např. "Strba (w)")
    Group_Combination = paste0(locality, " (", color_clean, ")"),
    
    # Definice pořadí na ose X: pro každou lokalitu nejdřív bílá (w), pak růžová (p)
    Group_Combination = factor(Group_Combination, levels = c("Prague (w)", "Prague (p)",
      "Strba (w)", "Strba (p)",
      "Vodopad Skok (w)", "Vodopad Skok (p)"
    )),
    color_clean = factor(color_clean, levels = c("w", "p"))
  )

# 3. Tvorba grafu v identickém vizuálním stylu (y = temp, jelikož jde o absolutní teploty z terénu)
p_locality_boxplot <- ggplot(loc_subset, aes(x = Group_Combination, y = temp, fill = color_clean)) +
  geom_boxplot(alpha = 0.4, color = "black", outlier.shape = NA, width = 0.35, show.legend = FALSE) +  
  # Vybarvené terénní tečky s černým obrysem
  geom_jitter(aes(fill = color_clean), shape = 21, width = 0.1, size = 1.2, color = "black", alpha = 0.8, show.legend = FALSE) +  
  scale_fill_manual(values = c("w" = "white", "p" = "deeppink2")) +
  labs(
    x = "Locality & Flower Color", 
    y = "Measured Temperature (°C)"
  ) +  
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5), 
    axis.text.y = element_text(size = 11),  
    axis.title = element_text(size = 13)
  )

# Export druhého grafu do samostatného PDF
pdf("locality_temp_boxplots_Pragueincl.pdf", width = 4.5, height = 4.5)
print(p_locality_boxplot)
dev.off()
cat("\n=== DESKRIPTIVNÍ STATISTIKY PRO TERÉNNÍ LOKALITY ===\n")

## STATS
# 1. Základní přehled: Průměry a SD pro každou kombinaci lokality a barvy
locality_summary <- loc_subset %>%
  group_by(locality, color_clean) %>%
  summarise(
    count = n(),
    mean_temp = mean(temp, na.rm = TRUE),
    sd_temp   = sd(temp, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nPrůměrné teploty podle lokality a barvy květu:\n")
print(as.data.frame(locality_summary))
cat("\n--------------------------------------------------\n")

# 2. Výpočet průměrné změny (rozdílu) teploty mezi barvami (p vs w) pro každou lokalitu
locality_diff <- locality_summary %>%
  select(locality, color_clean, mean_temp) %>%
  pivot_wider(names_from = color_clean, values_from = mean_temp) %>%
  mutate(
    # Rozdíl: Růžová (p) mínus Bílá (w)
    mean_temp_difference = p - w
  )

cat("Průměrná změna (rozdíl) teploty mezi růžovou (p) a bílou (w) barvou:\n")
print(as.data.frame(locality_diff))
cat("--------------------------------------------------\n")

cat("\n=== STATISTICKÉ TESTOVÁNÍ (WILCOXON TEST) ===\n")

localities <- c("Strba", "Vodopad Skok", "Prague")

for (loc in localities) {
  cat("\n--------------------------------------------------\n")
  cat("LOKALITA:", toupper(loc), "\n")
  cat("--------------------------------------------------\n")
  
  # Filtrace dat pouze pro jednu konkrétní lokalitu
  loc_data_single <- filter(loc_subset, locality == loc)
  
  # Kontrola, zda máme pro test obě barvy
  if (length(unique(loc_data_single$color_clean)) == 2) {
    # Wilcoxonův test závislosti teploty na barvě květu
    w_test <- wilcox.test(temp ~ color_clean, data = loc_data_single,alternative = "less")
    print(w_test)
  } else {
    cat("Nelze provést test: Chybí jedna z barevných skupin.\n")
  }
}





############################################# POLLEN TUBE GROWTH
setwd("~/pigmentation/termocamTatrypollenAssay/pollenTubes")
# Load required libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(forcats) # Useful for strict factor ordering

# ==========================================
# 1. Merge Excel sheets and export to CSV
# ==========================================

excel_file <- "pollen-tubes-length-temperature-13-15.xlsx"
sheet_names <- excel_sheets(excel_file)

merged_data <- lapply(sheet_names, function(sheet) {
  read_excel(excel_file, sheet = sheet)
}) %>% bind_rows()

# Export the merged dataset to a CSV file
write.csv(merged_data, "pollen_tubes_merged.csv", row.names = FALSE)


# ==========================================
# 2. Standardize lengths within each replicate
# ==========================================

merged_data <- merged_data %>%
  group_by(Rep) %>%
  mutate(Standardized_Length = (Length - mean(Length, na.rm = TRUE)) / sd(Length, na.rm = TRUE)) %>%
  ungroup()


# ==========================================
# 3. Recode to precise temperatures
# ==========================================

merged_data <- merged_data %>%
  mutate(variant_precise = case_when(
    variant == "13°C" ~ "13.3°C",
    variant == "15°C" ~ "14.9°C",
    TRUE ~ variant
  ))


# ==========================================
# 4. Growth rate and timing calculations
# ==========================================

incubation_hours <- 3
style_length_um <- 2 * 1000 # 2 mm to µm

growth_rates <- merged_data %>%
  group_by(variant_precise) %>%
  summarise(
    Avg_Length_um = mean(Length, na.rm = TRUE),
    Growth_Rate_um_per_hr = Avg_Length_um / incubation_hours
  )

cat("\n--- Growth Rates per Temperature ---\n")
print(growth_rates)

rate_cold <- growth_rates %>% filter(variant_precise == "13.3°C") %>% pull(Growth_Rate_um_per_hr)
rate_warm <- growth_rates %>% filter(variant_precise == "14.9°C") %>% pull(Growth_Rate_um_per_hr)

time_cold_hrs <- style_length_um / rate_cold
time_warm_hrs <- style_length_um / rate_warm
time_difference_hrs <- time_cold_hrs - time_warm_hrs

diff_hours <- floor(time_difference_hrs)
diff_minutes <- round((time_difference_hrs - diff_hours) * 60)

cat("\n--- Pollination Timing Estimation (2 mm style) ---\n")
cat(sprintf("Time at 13.3°C: %.2f hours\n", time_cold_hrs))
cat(sprintf("Time at 14.9°C: %.2f hours\n", time_warm_hrs))
cat(sprintf("Warm plants are pollinated earlier by: %d hours and %d minutes\n", diff_hours, diff_minutes))

# ==========================================
# 5. Corrected visualization by replicates (Single Row, Very Tight)
# ==========================================

# First, strictly sort the data by Replicate and Temperature
merged_data <- merged_data %>%
  arrange(Rep, variant_precise)

# Create the interaction factor and explicitly freeze its levels based on the sorted data
# This completely prevents 'NA' categories and guarantees they are paired side-by-side
merged_data <- merged_data %>%
  mutate(Group_Combination = factor(
    paste(Rep, variant_precise, sep = "\n"),
    levels = unique(paste(Rep, variant_precise, sep = "\n"))
  ))

# Plot with tight, narrow spacing
p_rep_single_row <- ggplot(merged_data, aes(x = Group_Combination, y = Length)) +
  # Extremely narrow boxplots (width = 0.35)
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA, width = 0.35) +  
  # Tight dotplot strictly matching the narrow box size (width = 0.1)
  geom_jitter(width = 0.1, size = 0.8, color = "black", alpha = 0.4) +  
  labs(x = "Replicate & Temperature", y = "Raw Length (µm)") +  
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text.x = element_text(size = 9, face = "bold", angle = 0, vjust = 0.5), 
    axis.text.y = element_text(size = 11),  
    axis.title = element_text(size = 13)
  )

# Export to PDF (width is set to 9 to keep them perfectly aligned next to each other)
pdf("pollen_tubes_by_replicate_single_row_fixed.pdf", width = 9, height = 4.5)
print(p_rep_single_row)
dev.off()


# ==========================================
# 6. Global visualization (Standardized Length)
# ==========================================

p_global <- ggplot(merged_data, aes(x = as.factor(variant_precise), y = Standardized_Length)) +
  geom_boxplot(fill = "purple", alpha = 0.4, color = "black", outlier.shape = NA, width = 0.4) +  
  geom_jitter(width = 0.15, size = 1.2, color = "black", alpha = 0.6) +  
  labs(x = "Temperature (°C)", y = "Standardized Length (Z-score)") +  
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    axis.text = element_text(size = 12),  
    axis.title = element_text(size = 14)
  )

ggsave("pollen_tubes_global.pdf", plot = p_global, width = 3, height = 7, dpi = 300)

# ==========================================
# 7. Statistical testing
# ==========================================

cat("\n--- Wilcoxon Rank Sum Test Results ---\n")
print(wilcox.test(Standardized_Length ~ variant_precise, data = merged_data))

cat("\n--- Linear Model (accounting for Replicate) Results ---\n")
print(summary(lm(Standardized_Length ~ variant_precise + Rep, data = merged_data)))



##################### POLINATORS
# Load required libraries
library(tidyverse)
library(lme4)
library(car)

# 1. LOAD CSV FILE
data_path <- "/home/aa/pigmentation/pollinators/pollinatorCounts_EDITED.csv"
raw_data <- read.csv(data_path, stringsAsFactors = FALSE)

# Classify insect orders (Diptera vs Hymenoptera)
raw_data <- raw_data %>%
  mutate(order = case_when(
    str_detect(tolower(determination), "hoverfly|fly|diptera|syrphidae|muscidae|scatphagidae|cheilosia") ~ "Diptera",
    str_detect(tolower(determination), "bee|lasioglossum|halictus|wasp") ~ "Hymenoptera",
    TRUE ~ "Other"
  ))

# Helper function to parse time slots into duration in minutes
parse_duration <- function(time_str) {
  parts <- strsplit(time_str, "-")[[1]]
  t1 <- as.POSIXct(parts[1], format="%H:%M")
  t2 <- as.POSIXct(parts[2], format="%H:%M")
  return(as.numeric(difftime(t2, t1, units="mins")))
}

# Filter for Mountain Garden (A) & Diptera
mountain_diptera <- raw_data %>%
  filter(garden == "A" & order == "Diptera") %>%
  mutate(duration_min = sapply(time, parse_duration))

cat("\n--- MODEL 1: Testing Color Preference (All Mountain Diptera Data) ---\n")

# Total available flowers by color: 
# Pink (p) = 5 | White (w + m + t) = 1 + 2 + 5 = 8
color_counts <- c("P" = 5, "W" = 8)

# Aggregate raw counts by time slot and color
color_data <- mountain_diptera %>%
  group_by(time, duration_min, color) %>%
  summarise(visit_count = sum(NplantsVisited, na.rm = TRUE), .groups = 'drop')

# Complete matrix to ensure zero-visits are included
all_color_combos <- expand.grid(
  time = unique(mountain_diptera$time),
  color = c("P", "W"),
  stringsAsFactors = FALSE
) %>% 
  left_join(unique(mountain_diptera[, c("time", "duration_min")]), by = "time") %>%
  left_join(color_data, by = c("time", "duration_min", "color")) %>%
  mutate(
    visit_count = ifelse(is.na(visit_count), 0, visit_count),
    available_flowers = color_counts[color],
    exposure_hours = available_flowers * (duration_min / 60)
  )

# Fit GLM for Color
glm_color <- glm(visit_count ~ color + offset(log(exposure_hours)), 
                 data = all_color_combos, 
                 family = poisson(link = "log"))

print(Anova(glm_color, type = "II"))


cat("\n--- MODEL 2: Testing Height and Color Preference with Interaction ---\n")
# Filter rows where feedingPattern exists
height_tracked <- mountain_diptera %>%
  filter(!is.na(feedingPattern) & feedingPattern != "")

# Specific counts per flower type: 5 short pink (p), 1 short white (w), 2 middle white (m), 5 tall white (t)
flower_counts <- c("p" = 5, "w" = 1, "m" = 2, "t" = 5)

# Expand sequential feeding patterns into individual rows
expanded_height_visits <- height_tracked %>%
  mutate(pattern_clean = gsub("[()]", "", feedingPattern)) %>%
  separate_rows(pattern_clean, sep = "") %>%
  filter(pattern_clean %in% c("t", "m", "p", "w")) %>% # Fixed typo: removed '='
  rename(flower_type = pattern_clean)

# Complete matrix for height analysis (only for time slots where pattern was tracked)
all_height_combos <- expand.grid(
  time = unique(height_tracked$time),
  flower_type = c("t", "m", "p", "w"),
  stringsAsFactors = FALSE
) %>% 
  left_join(unique(height_tracked[, c("time", "duration_min")]), by = "time")

height_data_ready <- all_height_combos %>%
  left_join(
    expanded_height_visits %>% group_by(time, flower_type) %>% summarise(visit_count = n(), .groups = 'drop'),
    by = c("time", "flower_type")
  ) %>%
  mutate(
    visit_count = ifelse(is.na(visit_count), 0, visit_count),
    available_flowers = flower_counts[flower_type],
    exposure_hours = available_flowers * (duration_min / 60),
    # Define height categories
    height = factor(case_when(flower_type == "t" ~ "tall", flower_type == "m" ~ "middle", TRUE ~ "short")),
    # NEW: Define color categories to allow interaction testing
    color = factor(case_when(flower_type == "p" ~ "pink", TRUE ~ "white"))
  )

# Fit GLM with Interaction (height * color)
glm_height_int <- glm(visit_count ~ height * color + offset(log(exposure_hours)), 
                      data = height_data_ready, 
                      family = poisson(link = "log"))

print(summary(glm_height_int))
write.csv(raw_data, "~/pigmentation/pollinators/166pollinatorvisits.csv")
# ==============================================================================
# VISUALIZATION: Standardized Visit Rates by Flower Group
# ==============================================================================
library(ggplot2)

# Calculate mean visit rates per flower per hour for each group
graph_data <- height_data_ready %>%
  mutate(visit_rate = visit_count / exposure_hours) %>%
  group_by(flower_group) %>%
  summarise(
    mean_rate = mean(visit_rate, na.rm = TRUE),
    se_rate = sd(visit_rate, na.rm = TRUE) / sqrt(n()), # Standard Error
    .groups = 'drop'
  )

# Create the plot
pdf("/home/aa/pigmentation/pollinators/pollinatorVisits.pdf",width = 4,height = 6,pointsize = 12)
ggplot(graph_data, aes(x = flower_group, y = mean_rate, fill = flower_group)) +
  geom_bar(stat = "identity", color = "black", width = 0.6, show.legend = FALSE) +
  geom_errorbar(aes(ymin = mean_rate, ymax = mean_rate + se_rate), width = 0.2) +
  # Custom colors: soft pink for pink, variants of grey/white for heights of white flowers
  scale_fill_manual(values = c("short_pink" = "#F3A0B1", 
                               "short_white" = "#F0F0F0", 
                               "middle_white" = "#D9D9D9", 
                               "tall_white" = "#B3B3B3")) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Flower group",
    y = "Visit rate (visits / flower / hour)",
    title = "Alpine pollinator preferences"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank()
  )
dev.off()





###SACHOVNICE
# real_short_dist = počet přeletů po řadě/sloupci
# real_diagonal   = počet přeletů po diagonále
real_moves <- c(short_dist = 27, diagonal = 9) 
# Chi-kvadrát test dobré shody s teoretickým poměrem 2/3 a 1/3
chi_test <- chisq.test(real_moves, p = c(2/3, 1/3))
print(chi_test)

library(ggplot2)

# Vytvoření tabulky pro graf (příklad dat)
plot_data <- data.frame(
  Type = rep(c("Shorter Distance\n(Different Color)", "Diagonal Distance\n(Same Color)"), 2),
  Data_Type = c(rep("Observed (Real)", 2), rep("Expected (Random)", 2)),
  Percentage = c(84, 16,  66.7, 33.3) # Svá reálná procenta doplň do prvních dvou pozic
)

pdf("/home/aa/pigmentation/pollinators/pollinatorTransitions.pdf",width = 6,height = 6,pointsize = 12)
ggplot(plot_data, aes(x = Type, y = Percentage, fill = Data_Type)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7) +
  scale_fill_manual(values = c("Observed (Real)" = "pink", "Expected (Random)" = "#D8D8D8")) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Movement Choice (N = 36)",
    y = "Transitions (%)",
    fill = "Data Source"
  ) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
dev.off()



################
# Načtení potřebných knihoven
library(readxl)
library(dplyr)

# Definice cest k souborům
excel_path <- "/home/aa/lyrataWGD/submissionNevimKam/SupplementaryDatasets_BohutinskaEtAl_PlosGenetics_290724.xlsx"
pop_info_path <- "/home/aa/pigmentation/SI/popInfo19JunFinalColor.txt"
output_dir <- "/home/aa/pigmentation/SI"
output_file <- file.path(output_dir, "515genomicIndividuals.csv")

# 1. Načtení popInfo souboru a filtrace populací bez NA v colComp
popInfo <- read.table(pop_info_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

popInfo<-subset(popInfo, !popInfo$colComp %in% NA & !popInfo$suspiciousColor %in% 1)


# 2. Načtení prvního sheetu z Excelu
excel_data <- read_excel(excel_path, sheet = 1)

# 3. Filtrování řádků: první tři písmena 'newname2' musí odpovídat platným populacím
filtered_data <- excel_data %>%
  filter(substr(newname2, 1, 3) %in% popInfo$pop)

# 4. Vytvoření cílové složky (pokud neexistuje) a export do CSV
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write.csv(filtered_data, file = output_file, row.names = FALSE)
write.csv(popInfo, file = "/home/aa/pigmentation/SI/64popInfogenomic.csv", row.names = FALSE)



#################
# Načtení knihovny pro práci s daty (pokud není nainstalována: install.packages("dplyr"))
library(dplyr)

# 1. Definice cest k souborům
pop_file <- "/home/aa/pigmentation/SI/64popInfogenomic.csv"
snps_file <- "/home/aa/pigmentation/SI/105common_snps_with_A_thaliana_genes_cleaned.csv"

# 2. Načtení dat
pop_info <- read.csv(pop_file, check.names = FALSE, stringsAsFactors = FALSE)
snps_data <- read.csv(snps_file, check.names = FALSE, stringsAsFactors = FALSE)

# 3. Výběr ID populací s nadmořskou výškou pod 1000 m
# Předpokládá se, že identifikátor populace je v 1. sloupci souboru 64popInfogenomic.csv
lowland_pop_ids <- pop_info %>%
  filter(Altitude < 1000) %>%
  pull(1) %>%
  as.character()

# 4. Identifikace odpovídajících sloupců v tabulce se SNP
# Vyberou se pouze ty názvy sloupců z posledních 64, které odpovídají nížinným populacím
all_pop_cols <- tail(colnames(snps_data), 64)
lowland_cols <- intersect(all_pop_cols, lowland_pop_ids)

# 5. Výpočet statistik po řádcích (pro každý SNP)
af_matrix <- as.matrix(snps_data[, lowland_cols])

snps_data$lowland_AF_mean   <- rowMeans(af_matrix, na.rm = TRUE)
snps_data$lowland_AF_median <- apply(af_matrix, 1, median, na.rm = TRUE)
snps_data$lowland_AF_sd     <- apply(af_matrix, 1, sd, na.rm = TRUE)

# 6. Export upravené tabulky zpět do původního souboru
write.csv(snps_data, snps_file, row.names = FALSE)

# 6. Vykreslení histogramů pro prvních 6 řádků (SNP)
# Uložení do PDF souboru (volitelné – pokud chcete grafy uložit na disk)
pdf("/home/aa/pigmentation/SI/figures/standVarhistograms_top6_snps.pdf", width = 10, height = 7)

# Rozvržení okna na mřížku 2 řádky × 3 sloupce
par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for (i in 1:min(6, nrow(snps_data))) {
  af_values <- as.numeric(snps_data[i, lowland_cols])
  
  # Název SNP pro nadpis grafu (využije sloupec SNP_ID nebo číslo řádku)
  snp_label <- if ("SNP_ID" %in% colnames(snps_data)) snps_data$SNP_start[i] else paste("Řádek", i)
  
  hist(
    af_values,
    main = paste("scaffold2:", snp_label),
    xlab = "Allele frequency",
    ylab = "N pop",
    col = "skyblue",
    border = "white",
    breaks = seq(0, 1, by = 0.1),
    xlim = c(0, 1)
  )
}

# Uzavření grafického zařízení (uložení PDF)
dev.off()

# Obnovení původního nastavení vykreslování
par(mfrow = c(1, 1))
