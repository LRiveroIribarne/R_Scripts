#####################################################################################
### Descarga serie completa SSEBOP y agregacion en Cauquenes en desembocadura
### Lucas Rivero Iribarne - mail: lucas.rivero@ug.uchile.cl
### Laboratorio de monitoreo y modelaciÃ³n de ecosistemas (LabMME)
#####################################################################################

rm(list=ls()) 
cat("\014")
graphics.off()

# Librerias ----
library(rvest)
library(gtools)
library(terra)
library(lubridate)
library(tidyverse)
library(plotly)
library(parallel)
library(ggplot2)
library(sf)


# URL con datos y directorio de salida ----
url = "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/fews/web/global/monthly/eta/downloads/"
dir.out = "D:/FONDECYT_1171560_BD/SSEBOP/data"
Descargar_imgs = FALSE # TRUE = descargar serie SSEBOP, FALSE = NO descargar serie SSEBOP

imgs.files = read_html(url) %>% html_nodes("a") %>% html_text() 
imgs.files = imgs.files[6: length(imgs.files)]

if (Descargar_imgs) {
  
  # Descarga de .tif en .zip, descompresion y guardado en disco ----
  for (i in 1:length(imgs.files)) {
    
    img = paste0(url, imgs.files[i])
    temp <- tempfile()
    download.file(url = img, temp)
    unzip(zipfile=temp, exdir=dir.out)
    unlink(temp)
    
    print(paste("Descargando", imgs.files[i], "---", round((i/length(imgs.files))*100, 2), "%"))
    
  }
  
}

# Cargar rasters y ordenarlos por fecha ----

ETa_files = mixedsort(list.files(dir.out, pattern = ".tif$", full.names = TRUE))
Eta = rast(ETa_files)

Cau_Desemb = vect("D:/FONDECYT_1171560_BD/vect_general/Cau_desemb_LiDAR.shp")

Eta_Cau_Desemb = mask(crop(Eta, Cau_Desemb), Cau_Desemb)

date = seq(ymd('2003-01-01'), ymd('2020-12-01'), by = '1 month') # 216/12 = 18 annos


# Tabla con serie completa ----

Eta = tibble(date = date,
             Eta = global(Eta_Cau_Desemb, sum, na.rm=TRUE)[[1]])

Eta$year = year(Eta$date)

# ggplot2
g1=ggplot(data = Eta, aes(x = date, y = Eta)) +
  geom_point() + geom_line(aes(group = 1)) + 
  theme_bw() + scale_x_date(date_breaks = "6 month", date_labels =  "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Evapotranspiración actual (SSEBOP) - Cauquenes en desembocadura", x = "fecha", y = "ETa (mm)") 
g1
# Plotly
ggplotly(p = g1)

# Eta por año ----

Eta_year = Eta %>% group_by(year) %>% summarize(Eta_year=sum(Eta))

g2=ggplot(data = Eta_year, aes(x = year, y = Eta_year)) +
  geom_point() + geom_line(aes(group = 1)) + 
  theme_bw() + geom_vline(xintercept=2010, linetype="dashed", color = "red", size=1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Evapotranspiración actual (SSEBOP) - Cauquenes en desembocadura", x = "fecha", y = "ETa (mm)") 
g2
# Plotly
ggplotly(p = g2)

# Mapas ----
# periodo completo por anno 
Eta_Cau_Desemb_yearly = tapp(Eta_Cau_Desemb, Eta$year, sum)
Eta_Cau_Desemb_mean = app(x = Eta_Cau_Desemb_yearly, fun = mean, cores = detectCores() - 1)
Eta_Cau_Desemb_mean_df = as.data.frame(Eta_Cau_Desemb_mean, xy = TRUE) %>% drop_na()

ggplot() + 
  geom_raster(data = Eta_Cau_Desemb_mean_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = st_as_sf(as.data.frame(Cau_Desemb, geom = TRUE), wkt = "geometry", crs=crs(Cau_Desemb)), fill = "transparent") +
  scale_fill_viridis_c(name = "mm/año", direction = -1) +
  labs(x = "Longitud", y = "Latitud",
       title = "ETa promedio anual periodo 2003 - 2020") +
  cowplot::theme_cowplot() +
  theme(panel.grid.major = element_line(color = "black",
                                        linetype = "dashed",
                                        size = .5),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, color = "black"))

# Perido previo Megasequia (< 2010) SMD = Sin Mega drought 
Eta_Cau_Desemb_SMD_yearly = Eta_Cau_Desemb_yearly[[1:7]]
Eta_Cau_Desemb_SMD_mean = app(x = Eta_Cau_Desemb_SMD_yearly, fun = mean, cores = detectCores() - 1)
Eta_Cau_Desemb_SMD_mean_df = as.data.frame(Eta_Cau_Desemb_SMD_mean, xy = TRUE) %>% drop_na()

ggplot() + 
  geom_raster(data = Eta_Cau_Desemb_SMD_mean_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = st_as_sf(as.data.frame(Cau_Desemb, geom = TRUE), wkt = "geometry", crs=crs(Cau_Desemb)), fill = "transparent") +
  scale_fill_viridis_c(name = "mm/año", direction = -1) +
  labs(x = "Longitud", y = "Latitud",
       title = "ETa promedio anual periodo Sin megasequía 2003 - 2010") +
  cowplot::theme_cowplot() +
  theme(panel.grid.major = element_line(color = "black",
                                        linetype = "dashed",
                                        size = .5),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, color = "black"))


# Periodo Megasequia (>= 2010) MD = Mega drought
Eta_Cau_Desemb_MD_yearly = Eta_Cau_Desemb_yearly[[8:nlyr(Eta_Cau_Desemb_yearly)]]
Eta_Cau_Desemb_MD_mean = app(x = Eta_Cau_Desemb_MD_yearly, fun = mean, cores = detectCores() - 1)
Eta_Cau_Desemb_MD_mean_df = as.data.frame(Eta_Cau_Desemb_MD_mean, xy = TRUE) %>% drop_na()

ggplot() + 
  geom_raster(data = Eta_Cau_Desemb_MD_mean_df, aes(x = x, y = y, fill = mean)) +
  geom_sf(data = st_as_sf(as.data.frame(Cau_Desemb, geom = TRUE), wkt = "geometry", crs=crs(Cau_Desemb)), fill = "transparent") +
  scale_fill_viridis_c(name = "mm/año", direction = -1) +
  labs(x = "Longitud", y = "Latitud",
       title = "ETa promedio anual periodo megasequía 2010 - 2020") +
  cowplot::theme_cowplot() +
  theme(panel.grid.major = element_line(color = "black",
                                        linetype = "dashed",
                                        size = .5),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE,
        panel.background = element_rect(fill = NA, color = "black"))

# FIN ----