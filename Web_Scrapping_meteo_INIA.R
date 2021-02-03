###############################################################################################
### Web Scrapping estaciones meteo INIA 
### Lucas Rivero Iribarne - mail: lucas.rivero@ug.uchile.cl
### Laboratorio de monitoreo y modelacion de ecosistemas (LabMME)
###############################################################################################

rm(list=ls()) 
cat("\014")
graphics.off()

# Librerias ----
library(tidyverse)
library(rvest)
library(RSelenium)
library(lubridate)

# USER INPUT ----
url = "https://agrometeorologia.cl/"
estacion = c("Liceo Jean Buchanan, Peumo, INIA")
desde = "01-06-2009" # Primera fecha disponible plataforma INIA
hasta = paste0(day(today()-1), "-", month(today()-1), "-", year(today()-1)) # AYER ya que si se piden los datos por ejemplo al medio dia solo estara represtendo 0 - 12 y no 12 - 24 hrs.
dir.out = "D:/Crop_Coeff/meteo"
# -----------------------------------------------------------------------------------------------------------------------------
# Se separa en dos ya que la plataforma de INIA no permite descargar mas de 5 variables simultaneamente
vars1 = c("Temperatura del Aire",
              "Temperatura del Aire Mínima",
              "Temperatura del Aire Máxima",
              "Humedad Relativa",
              "Humedad Relativa Mínima")

vars2 = c("Humedad Relativa Máxima",
          "Precipitación Acumulada",
          "Presión Atmosférica",
          "Radiación Solar",
          "Velocidad de Viento")

intervalo = "Día"

# RSelenium web browser ----
rD <- rsDriver(verbose = TRUE,
               port=3030L, 
               browser = c('firefox'),
               geckover = "0.28.0",
               check = TRUE)
remDr <- rD$client

# vars 1 ----
# Navegar a la pagina ----
remDr$navigate(url)

# Click en caja de "Estaciones" ----
remDr$findElements(using = "id", value = "box-consultar")[[1]]$clickElement()

# Agregar estacion ----
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$clickElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$clearElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$sendKeysToElement(list(estacion, key="enter"))

# Agregar variables a descargar ----
# vars1 primero y mas abajo vars 2, esto se hace ya que INIA no permite descargar mas de 5 variables al mismo tiempo
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Variables']")[[1]]$clickElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Variables']")[[1]]$clearElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars1[1], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars1[2], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars1[3], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars1[4], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars1[5], key="enter"))

# intervalo de tiempo datos ----
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[2]/form/div[1]/div/div[3]/a/span")[[1]]$clickElement()
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[2]/form/div[1]/div/div[3]/div/div/input")[[1]]$sendKeysToElement(list(intervalo, key = "enter"))

# Intervalo fechas ----
# Desde
remDr$findElement(using = "id", value = "desde")$clearElement()
remDr$findElement(using = "id", value = "desde")$clickElement()
remDr$findElement(using = "xpath", value = "//*[@id='desde']")$sendKeysToElement(list(desde, key = "enter")) # dd-mm-yyyy

# Hasta
remDr$findElement(using = "id", value = "hasta")$clearElement()
remDr$findElement(using = "id", value = "hasta")$clickElement()
remDr$findElement(using = "id", value = "hasta")$sendKeysToElement(list(hasta, key = "enter")) # dd-mm-yyyy

# Formato ----
remDr$findElements(using = "xpath", "//label[@class = 'custom-control-label' and @for = 'csv']")[[1]]$clickElement()

# Click boton consulta y descarga ----
remDr$findElement(using = "id", value = "search-btn")$clickElement()

link_download_vars1 = remDr$findElement(using = "xpath", "/html/body/div[1]/main/section/div/h2/a")$getElementAttribute("href")[[1]]

setwd(dir.out)
download.file(url = link_download_vars1, "meteo_vars1.csv")

# vars 2 ----
# Navegar a la pagina ----
remDr$navigate(url)

# Click en caja de "Estaciones" ----
remDr$findElements(using = "id", value = "box-consultar")[[1]]$clickElement()

# Agregar estacion ----
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$clickElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$clearElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$sendKeysToElement(list(estacion, key="enter"))

# Agregar variables a descargar ----
# vars 2, esto se hace ya que INIA no permite descargar mas de 5 variables al mismo tiempo
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Variables']")[[1]]$clickElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Variables']")[[1]]$clearElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars2[1], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars2[2], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars2[3], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars2[4], key="enter"))
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input' and @value = 'Variables']")[[1]]$sendKeysToElement(list(vars2[5], key="enter"))

# intervalo de tiempo datos ----
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[2]/form/div[1]/div/div[3]/a/span")[[1]]$clickElement()
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[2]/form/div[1]/div/div[3]/div/div/input")[[1]]$sendKeysToElement(list(intervalo, key = "enter"))

# Intervalo fechas ----
# Desde
remDr$findElement(using = "id", value = "desde")$clearElement()
remDr$findElement(using = "id", value = "desde")$clickElement()
remDr$findElement(using = "xpath", value = "//*[@id='desde']")$sendKeysToElement(list(desde, key = "enter")) # dd-mm-yyyy

# Hasta
remDr$findElement(using = "id", value = "hasta")$clearElement()
remDr$findElement(using = "id", value = "hasta")$clickElement()
remDr$findElement(using = "id", value = "hasta")$sendKeysToElement(list(hasta, key = "enter")) # dd-mm-yyyy

# Formato ----
remDr$findElements(using = "xpath", "//label[@class = 'custom-control-label' and @for = 'csv']")[[1]]$clickElement()

# Click boton consulta y descarga ----
remDr$findElement(using = "id", value = "search-btn")$clickElement()

link_download_vars2 = remDr$findElement(using = "xpath", "/html/body/div[1]/main/section/div/h2/a")$getElementAttribute("href")[[1]]

setwd(dir.out)
download.file(url = link_download_vars2, "meteo_vars2.csv")

# Evapotranspiracion Penman-Monteith ----
# Navegar a la pagina ----
remDr$navigate(url)
remDr$findElements(using = "xpath", "/html/body/div[1]/header/nav[2]/div/div/label")[[1]]$clickElement()
remDr$findElements(using = "xpath", "/html/body/div[1]/header/nav[2]/div/div/ul[1]/li[6]/a")[[1]]$clickElement()

# Seleccionar estacion ETo ----
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[3]/form/div[1]/a/h5")[[1]]$clickElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$clearElement()
remDr$findElements(using = "xpath", "//input[@class = 'chosen-search-input default' and @value = 'Estaciones']")[[1]]$sendKeysToElement(list(estacion, key="enter"))

# intervalo de tiempo datos ----
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[3]/form/div[1]/div/div[2]/a/span")[[1]]$clickElement()
remDr$findElements(using = "xpath", "/html/body/div[1]/section/div/div[1]/div[3]/form/div[1]/div/div[2]/div/div/input")[[1]]$sendKeysToElement(list(intervalo, key = "enter"))

# Intervalo fechas ----
# Desde
remDr$findElement(using = "xpath", value = "//*[@id='desde']")$sendKeysToElement(list(desde, key = "enter")) # dd-mm-yyyy

# Hasta
remDr$findElement(using = "id", value = "hasta")$sendKeysToElement(list(hasta, key = "enter")) # dd-mm-yyyy

# Formato ----
remDr$findElements(using = "xpath", "//label[@class = 'custom-control-label' and @for = 'csv']")[[1]]$clickElement()

# Click boton consulta y descarga ----
remDr$findElement(using = "id", value = "search-btn")$clickElement()

link_download_ETo = remDr$findElement(using = "xpath", "/html/body/div[1]/main/section/div/h2/a")$getElementAttribute("href")[[1]]

setwd(dir.out)
download.file(url = link_download_ETo, "meteo_ETo.csv")


# FIN ----