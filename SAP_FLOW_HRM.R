########################################################################
### Sap flow Cauquenes - Heat Ratio Method (HRM)
########################################################################

# Clean enviroment ----

rm(list=ls()) 
cat("\014")
graphics.off()

# USER INPUTS ----

# SAVE RAW UNIFIED DATA?

 SAVE = TRUE

# set directories ----

dir.in = "E:/FONDECYT_1171560_BD/Sap_flows/ARAUCO/datos"
dir.out = "E:/FONDECYT_1171560_BD/Sap_flows/ARAUCO/datos/resultados"
# Load functions ----

source("E:/FONDECYT_1171560_BD/Sap_flows/ARAUCO/HRM_functions.R")

# Load packages ----

library(lubridate)
library(dplyr)
library(dygraphs)
library(xts) 
library(tidyverse)

# 0 Plant samples -------------------------------------------------------------------------------
# Hualo 1 - sens1H
H1_md = 0.370 # Masa seca (g)
H1_mf = 0.600 # Masa fresca (g)
H1_Vw = 0.750 # Volumen (cm3)
H1_x1 = 0.6
H1_x2 = -0.6
H1_SA = 38.3
# Hualo 2 - sens2H
H2_md = 0.370 # Masa seca (g)
H2_mf = 0.600 # Masa fresca (g)
H2_Vw = 0.750 # Volumen (cm3)
H2_x1 = 0.6
H2_x2 = -0.6
H2_SA = 38.3
# Pino 1 - sens1P
P1_md = 0.540 # Masa seca (g)
P1_mf = 1.020 # Masa fresca (g)
P1_Vw = 1.700 # Volumen (cm3)
P1_x1 = 0.6
P1_x2 = -0.6
P1_SA = 211.8
# Pino 2 - sens2P
P2_md = 0.540 # Masa seca (g)
P2_mf = 1.020 # Masa fresca (g)
P2_Vw = 1.700 # Volumen (cm3)
P2_x1 = 0.6
P2_x2 = -0.6
P2_SA = 211.8
# Matorral 1 - sens1M
M1_md = 0.540 # Masa seca (g)
M1_mf = 1.020 # Masa fresca (g)
M1_Vw = 1.700 # Volumen (cm3)
M1_x1 = 0.6
M1_x2 = -0.6
M1_SA = 211.8
# Matorral 2 - sens2M
M2_md = 0.540 # Masa seca (g)
M2_mf = 1.020 # Masa fresca (g)
M2_Vw = 1.700 # Volumen (cm3)
M2_x1 = 0.6
M2_x2 = -0.6
M2_SA = 211.8
  
  # 1 - Lectura datos crudos sap flows ------------------------------------------------------------
# Bosque nativo Molco (Hualo - Nothofagus glauca)
# ARBOL SONDA ---
BN_SF_s_p_path = list.files(paste0(dir.in, '/bosque_nativo/1_sonda'), pattern = '.SF$', full.names = T) ; BN_SF_s_p_path  # Bosque nativo, archivo .SF, arbol CON sonda y CON piranometro

names = c('ID_logger', 'XBee', 'date', 'time_UTC', 'sample_n', 'nn1', 'Voltage', 'T1T2', 'Vh_cmh', 'radiacion_Wm2', 'nn2')

BN_SF_s_p_df = data.frame('fecha_UTC' = character(),
                          'fecha_local' = character(),
                          'Voltage' = character(),
                          'T1T2' = character(),
                          'Vh_cmh' = character(),
                          'radiacion_Wm2' = character())

for (i in 1:length(BN_SF_s_p_path)) { # Ciclo para unir datos generados diarimente en un unico archivo
  df = read.table(BN_SF_s_p_path[i], header = FALSE, sep =',', stringsAsFactors = FALSE, skip = 1)
  names(df) = names
  year  = substr(df$date, start = 7, stop = 10)
  month = substr(df$date, start = 4, stop = 5)
  day   = substr(df$date, start = 1, stop = 2)
  date  = paste0(year,"-",month,"-", day)
  
  df1 = data.frame('fecha_UTC' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "GMT"),                    # Date Time UTC
                   'fecha_local' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "America/Santiago"),     # Date Time Local
                   'Voltage' = df$Voltage,                                                                     # Voltage (V or "low Voltage")
                   'T1T2' = df$`T1T2`,                                                                         # DeltaT1 / DeltaT2
                   'Vh_cmh' = df$Vh_cmh,                                                                       # Vh (cm/hr)
                   'radiacion_Wm2' = df$radiacion_Wm2)                                                         # Radiation (W/m2)
  
  BN_SF_s_p_df = rbind(BN_SF_s_p_df, df1)
  print(paste('Archivo ', BN_SF_s_p_path[i], 'Fechas entre: ', df1$fecha_local[1], 'y',df1$fecha_local[nrow(df1)]))
}  
  
BN_SF_s_p_df = filter(BN_SF_s_p_df, fecha_local >= "2020-02-20")

# ARBOL SIN SONDA ---
BN_SF_2_path = list.files(paste0(dir.in, '/bosque_nativo/2'), pattern = '.SF$', full.names = T) ; BN_SF_2_path  # Bosque nativo, archivo .SF, arbol SIN sonda y SIN piranometro

names = c('ID_logger', 'XBee', 'date', 'time_UTC', 'sample_n', 'nn1', 'Voltage', 'T1T2', 'Vh_cmh', 'radiacion_Wm2', 'nn2')

BN_SF_2_df = data.frame('fecha_UTC' = character(),
                        'fecha_local' = character(),
                        'Voltage' = character(),
                        'T1T2' = character(),
                        'Vh_cmh' = character(),
                        'radiacion_Wm2' = character())

for (i in 1:length(BN_SF_2_path)) { # Ciclo para unir datos generados diarimente en un unico archivo
  df = read.table(BN_SF_2_path[i], header = FALSE, sep =',', stringsAsFactors = FALSE, skip = 1)
  names(df) = names
  year  = substr(df$date, start = 7, stop = 10)
  month = substr(df$date, start = 4, stop = 5)
  day   = substr(df$date, start = 1, stop = 2)
  date  = paste0(year,"-",month,"-", day)
  
  df1 = data.frame('fecha_UTC' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "GMT"),                    # Date Time UTC
                   'fecha_local' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "America/Santiago"),     # Date Time Local
                   'Voltage' = df$Voltage,                                                                     # Voltage (V or "low Voltage")
                   'T1T2' = df$`T1T2`,                                                                         # DeltaT1 / DeltaT2
                   'Vh_cmh' = df$Vh_cmh,                                                                       # Vh (cm/hr)
                   'radiacion_Wm2' = df$radiacion_Wm2)                                                         # Radiation (W/m2)
  
  BN_SF_2_df = rbind(BN_SF_2_df, df1)
  print(paste('Archivo ', BN_SF_2_path[i], 'Fechas entre: ', df1$fecha_local[1], 'y',df1$fecha_local[nrow(df1)]))
}

# Plantacion de pinos Molco (Pino - Pinnus radiata)
# ARBOL CON SONDA ---
P_SF_s_p_path = list.files(paste0(dir.in, '/plantacion_pinos/1_sonda'), pattern = '.SF$', full.names = T) ; P_SF_s_p_path  # Pino, archivo .SF, arbol CON sonda y CON piranometro

names = c('ID_logger', 'XBee', 'date', 'time_UTC', 'sample_n', 'nn1', 'Voltage', 'T1T2', 'Vh_cmh', 'radiacion_Wm2', 'nn2')

P_SF_s_p_df = data.frame('fecha_UTC' = character(),
                         'fecha_local' = character(),
                         'Voltage' = character(),
                         'T1T2' = character(),
                         'Vh_cmh' = character(),
                         'radiacion_Wm2' = character())

for (i in 1:length(P_SF_s_p_path)) { # Ciclo para unir datos generados diarimente en un unico archivo
  df = read.table(P_SF_s_p_path[i], header = FALSE, sep =',', stringsAsFactors = FALSE, skip = 1)
  names(df) = names
  year  = substr(df$date, start = 7, stop = 10)
  month = substr(df$date, start = 4, stop = 5)
  day   = substr(df$date, start = 1, stop = 2)
  date  = paste0(year,"-",month,"-", day)
  
  df1 = data.frame('fecha_UTC' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "GMT"),                    # Date Time UTC
                   'fecha_local' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "America/Santiago"),     # Date Time Local
                   'Voltage' = df$Voltage,                                                                     # Voltage (V or "low Voltage")
                   'T1T2' = df$`T1T2`,                                                                         # DeltaT1 / DeltaT2
                   'Vh_cmh' = df$Vh_cmh,                                                                       # Vh (cm/hr)
                   'radiacion_Wm2' = df$radiacion_Wm2)                                                         # Radiation (W/m2)
  P_SF_s_p_df = rbind(P_SF_s_p_df, df1)
  print(paste('Archivo ', P_SF_s_p_path[i], 'Fechas entre: ', df1$fecha_local[1], 'y',df1$fecha_local[nrow(df1)]))
}


# ARBOL SIN SONDA ---
P_SF_path = list.files(paste0(dir.in, '/plantacion_pinos/2'), pattern = '.SF$', full.names = T) ; P_SF_path  # Pino, archivo .SF, arbol SIN sonda y SIN piranometro

names = c('ID_logger', 'XBee', 'date', 'time_UTC', 'sample_n', 'nn1', 'Voltage', 'T1T2', 'Vh_cmh', 'radiacion_Wm2', 'nn2')

P_SF_df = data.frame('fecha_UTC' = character(),
                     'fecha_local' = character(),
                     'Voltage' = character(),
                     'T1T2' = character(),
                     'Vh_cmh' = character(),
                     'radiacion_Wm2' = character())

for (i in 1:length(P_SF_path)) { # Ciclo para unir datos generados diarimente en un unico archivo
  df = read.table(P_SF_path[i], header = FALSE, sep =',', stringsAsFactors = FALSE, skip = 1)
  names(df) = names
  year  = substr(df$date, start = 7, stop = 10)
  month = substr(df$date, start = 4, stop = 5)
  day   = substr(df$date, start = 1, stop = 2)
  date  = paste0(year,"-",month,"-", day)
 
  df1 = data.frame('fecha_UTC' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "GMT"),                    # Date Time UTC
                   'fecha_local' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "America/Santiago"),     # Date Time Local
                   'Voltage' = df$Voltage,                                                                     # Voltage (V or "low Voltage")
                   'T1T2' = df$`T1T2`,                                                                         # DeltaT1 / DeltaT2
                   'Vh_cmh' = df$Vh_cmh,                                                                       # Vh (cm/hr)
                   'radiacion_Wm2' = df$radiacion_Wm2)                                                         # Radiation (W/m2)
  
  P_SF_df = rbind(P_SF_df, df1)
  print(paste('Archivo ', P_SF_path[i], 'Fechas entre: ', df1$fecha_local[1], 'y',df1$fecha_local[nrow(df1)]))
}

# Matorral de espinos INIA (Espino - Acavia caven)
# ARBOL CON SONDA ---
MAT_SF_s_path = list.files(paste0(dir.in, '/matorral_espinos/1_sonda'), pattern = '.SF$', full.names = T) ; MAT_SF_s_path  # Matorral, archivo .SF, arbol CON sonda y SIN piranometro

names = c('ID_logger', 'XBee', 'date', 'time_UTC', 'sample_n', 'nn1', 'Voltage', 'T1T2', 'Vh_cmh', 'radiacion_Wm2', 'nn2')

MAT_SF_s_df = data.frame('fecha_UTC' = character(),
                         'fecha_local' = character(),
                         'Voltage' = character(),
                         'T1T2' = character(),
                         'Vh_cmh' = character(),
                         'radiacion_Wm2' = character())

for (i in 1:length(MAT_SF_s_path)) { # Ciclo para unir datos generados diarimente en un unico archivo
  df = read.table(MAT_SF_s_path[i], header = FALSE, sep =',', stringsAsFactors = FALSE, skip = 1)
  names(df) = names
  year  = substr(df$date, start = 7, stop = 10)
  month = substr(df$date, start = 4, stop = 5)
  day   = substr(df$date, start = 1, stop = 2)
  date  = paste0(year,"-",month,"-", day)
 
   df1 = data.frame('fecha_UTC' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "GMT"),                    # Date Time UTC
                    'fecha_local' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "America/Santiago"),     # Date Time Local
                    'Voltage' = df$Voltage,                                                                     # Voltage (V or "low Voltage")
                    'T1T2' = df$`T1T2`,                                                                         # DeltaT1 / DeltaT2
                    'Vh_cmh' = df$Vh_cmh,                                                                       # Vh (cm/hr)
                    'radiacion_Wm2' = df$radiacion_Wm2)                                                         # Radiation (W/m2)
  
  MAT_SF_s_df = rbind(MAT_SF_s_df, df1)
  print(paste('Archivo ', MAT_SF_s_path[i], 'Fechas entre: ', df1$fecha_local[1], 'y',df1$fecha_local[nrow(df1)]))
}

MAT_SF_s_df = filter(MAT_SF_s_df, fecha_local >= "2020-01-01")


# ARBOL SIN SONDA ---
MAT_SF_p_path = list.files(paste0(dir.in, '/matorral_espinos/2'), pattern = '.SF$', full.names = T) ; MAT_SF_p_path  # Matorral, archivo .SF, arbol CON sonda y SIN piranometro

names = c('ID_logger', 'XBee', 'date', 'time_UTC', 'sample_n', 'nn1', 'Voltage', 'T1T2', 'Vh_cmh', 'radiacion_Wm2', 'nn2')

MAT_SF_p_df = data.frame('fecha_UTC' = character(),
                         'fecha_local' = character(),
                         'Voltage' = character(),
                         'T1T2' = character(),
                         'Vh_cmh' = character(),
                         'radiacion_Wm2' = character())

for (i in 1:length(MAT_SF_p_path)) { # Ciclo para unir datos generados diarimente en un unico archivo
  df = read.table(MAT_SF_p_path[i], header = FALSE, sep =',', stringsAsFactors = FALSE, skip = 1)
  names(df) = names
  year  = substr(df$date, start = 7, stop = 10)
  month = substr(df$date, start = 4, stop = 5)
  day   = substr(df$date, start = 1, stop = 2)
  date  = paste0(year,"-",month,"-", day)
 
  df1 = data.frame('fecha_UTC' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "GMT"),                    # Date Time UTC
                   'fecha_local' = with_tz(ymd_hms(paste(date, df$time_UTC)), tzone = "America/Santiago"),     # Date Time Local
                   'Voltage' = df$Voltage,                                                                     # Voltage (V or "low Voltage")
                   'T1T2' = df$`T1T2`,                                                                         # DeltaT1 / DeltaT2
                   'Vh_cmh' = df$Vh_cmh,                                                                       # Vh (cm/hr)
                   'radiacion_Wm2' = df$radiacion_Wm2)                                                         # Radiation (W/m2)
  
  MAT_SF_p_df = rbind(MAT_SF_p_df, df1)
  print(paste('Archivo ', MAT_SF_p_path[i], 'Fechas entre: ', df1$fecha_local[1], 'y',df1$fecha_local[nrow(df1)]))
}

# Rename data frames ----

sens1H = BN_SF_s_p_df
sens2H = BN_SF_2_df
sens1P = P_SF_s_p_df
sens2P = P_SF_df
sens1M = MAT_SF_s_df
sens2M = MAT_SF_p_df

# 2 - Limpieza de datos -----------------------------------------------------------------------
sens1H$radiacion_Wm2 = replace(sens1H$radiacion_Wm2, which(sens1H$radiacion_Wm2 ==-9999),NA)
sens2H$radiacion_Wm2 = replace(sens2H$radiacion_Wm2, which(sens2H$radiacion_Wm2 ==-9999),NA)
sens1P$radiacion_Wm2 = replace(sens1P$radiacion_Wm2, which(sens1P$radiacion_Wm2 ==-9999),NA)
sens2P$radiacion_Wm2 = replace(sens2P$radiacion_Wm2, which(sens2P$radiacion_Wm2 ==-9999),NA)
sens1M$radiacion_Wm2 = replace(sens1M$radiacion_Wm2, which(sens1M$radiacion_Wm2 ==-9999),NA)
sens2M$radiacion_Wm2 = replace(sens2M$radiacion_Wm2, which(sens2M$radiacion_Wm2 ==-9999),NA)

# Radiation
sens1H$Radiacion2 = sens1H$radiacion_Wm2 * 5 * 0.0078125
sens2H$Radiacion2 = sens2H$radiacion_Wm2 * 5 * 0.0078125
sens1P$Radiacion2 = sens1P$radiacion_Wm2 * 5 * 0.0078125
sens2P$Radiacion2 = sens2P$radiacion_Wm2 * 5 * 0.0078125
sens1M$Radiacion2 = sens1M$radiacion_Wm2 * 5 * 0.0078125
sens2M$Radiacion2 = sens2M$radiacion_Wm2 * 5 * 0.0078125

# Replaces all negative heat ratios and low voltage
sens1H$Vh_cmh = replace(sens1H$Vh_cmh, which(sens1H$T1T2 <0),NA)
sens2H$Vh_cmh = replace(sens2H$Vh_cmh, which(sens2H$T1T2 <0),NA)
sens1P$Vh_cmh = replace(sens1P$Vh_cmh, which(sens1P$T1T2 <0),NA)
sens2P$Vh_cmh = replace(sens2P$Vh_cmh, which(sens2P$T1T2 <0),NA)
sens1M$Vh_cmh = replace(sens1M$Vh_cmh, which(sens1M$T1T2 <0),NA)
sens2M$Vh_cmh = replace(sens2M$Vh_cmh, which(sens2M$T1T2 <0),NA)  

# Save raw unified data ----

if (SAVE) {
  
  write.csv(sens1H, paste0(dir.out, "/raw_unified_data/sens1H.csv"))
  write.csv(sens2H, paste0(dir.out, "/raw_unified_data/sens2H.csv"))
  write.csv(sens1P, paste0(dir.out, "/raw_unified_data/sens1P.csv"))
  write.csv(sens2P, paste0(dir.out, "/raw_unified_data/sens2P.csv"))
  write.csv(sens1M, paste0(dir.out, "/raw_unified_data/sens1M.csv"))
  write.csv(sens2M, paste0(dir.out, "/raw_unified_data/sens2M.csv"))
}

# All corrections sens1H ----

sens1H$Vh_cmh2 = Vh(k = 0.0025, x = 0.6, dif_delta_T = sens1H$T1T2) # Heat pulse velocity (Vh) in cm/hr
sens1H$Mc = Mc(Mf = H1_mf, Md = H1_md) # Mc (-)
sens1H$Rho = Rho(Mf = H1_mf, Vw = H1_Vw) # (g/cm3)
sens1H$rho_b = Rho_b(Md = H1_md, Vw = H1_Vw) # (g/cm3)
sens1H$Vf_h2o = Vf_h2o(Mf = H1_mf, Md = H1_md, Vw = H1_Vw) # (g/cm3)
sens1H$C = C(Md = H1_md, Cw = 1200, Cs = 4182, Mf = H1_mf) # (J / kg * °C)
sens1H$Fv = Fv(rho_b = sens1H$rho_b, Vf_h2o = sens1H$Vf_h2o)
sens1H$Kw = Kw(Fv = sens1H$Fv)
sens1H$kgw = kgw(ks = 0.5984, Mc = sens1H$Mc, rho_b = sens1H$rho_b, rho_s = 1, kw = sens1H$Kw)
sens1H$K_adj = k_adj(kgw = sens1H$kgw, rho = sens1H$Rho, C = sens1H$C)
sens1H$Vh_diff_adj = sens1H$K_adj/0.25 * sens1H$Vh_cmh2  

# missaligment correction from White, 2021

Hour <- as.POSIXlt(sens1H$fecha_local)$hour
Align_Correct <- mean(subset(sens1H, Hour > 23 | Hour <=4,)$Vh_diff_adj, na.rm = TRUE)

sens1H$Vh_cmhr_corr <- sens1H$Vh_diff_adj - Align_Correct 

# All corrections sens2H ----

sens2H$Vh_cmh2 = Vh(k = 0.0025, x = 0.6, dif_delta_T = sens2H$T1T2) # Heat pulse velocity (Vh) in cm/hr
sens2H$Mc = Mc(Mf = H2_mf, Md = H2_md) # Mc (-)
sens2H$Rho = Rho(Mf = H2_mf, Vw = H2_Vw) # (g/cm3)
sens2H$rho_b = Rho_b(Md = H2_md, Vw = H2_Vw) # (g/cm3)
sens2H$Vf_h2o = Vf_h2o(Mf = H2_mf, Md = H2_md, Vw = H2_Vw) # (g/cm3)
sens2H$C = C(Md = H2_md, Cw = 1200, Cs = 4182, Mf = H2_mf) # (J / kg * °C)
sens2H$Fv = Fv(rho_b = sens2H$rho_b, Vf_h2o = sens2H$Vf_h2o)
sens2H$Kw = Kw(Fv = sens2H$Fv)
sens2H$kgw = kgw(ks = 0.5984, Mc = sens2H$Mc, rho_b = sens2H$rho_b, rho_s = 1, kw = sens2H$Kw)
sens2H$K_adj = k_adj(kgw = sens2H$kgw, rho = sens2H$Rho, C = sens2H$C)
sens2H$Vh_diff_adj = sens2H$K_adj/0.25 * sens2H$Vh_cmh2  

# missaligment correction from White, 2021

Hour <- as.POSIXlt(sens2H$fecha_local)$hour
Align_Correct <- mean(subset(sens2H, Hour > 22 | Hour <=4,)$Vh_diff_adj, na.rm = TRUE)

sens2H$Vh_cmhr_corr <- sens2H$Vh_diff_adj - Align_Correct 

# All corrections sens1P ----

sens1P$Vh_cmh2 = Vh(k = 0.0025, x = 0.6, dif_delta_T = sens1P$T1T2) # Heat pulse velocity (Vh) in cm/hr
sens1P$Mc = Mc(Mf = P1_mf, Md = P1_md) # Mc (-)
sens1P$Rho = Rho(Mf = P1_mf, Vw = P1_Vw) # (g/cm3)
sens1P$rho_b = Rho_b(Md = P1_md, Vw = P1_Vw) # (g/cm3)
sens1P$Vf_h2o = Vf_h2o(Mf = P1_mf, Md = P1_md, Vw = P1_Vw) # (g/cm3)
sens1P$C = C(Md = P1_md, Cw = 1200, Cs = 4182, Mf = P1_mf) # (J / kg * °C)
sens1P$Fv = Fv(rho_b = sens1P$rho_b, Vf_h2o = sens1P$Vf_h2o)
sens1P$Kw = Kw(Fv = sens1P$Fv)
sens1P$kgw = kgw(ks = 0.5984, Mc = sens1P$Mc, rho_b = sens1P$rho_b, rho_s = 1, kw = sens1P$Kw)
sens1P$K_adj = k_adj(kgw = sens1P$kgw, rho = sens1P$Rho, C = sens1P$C)
sens1P$Vh_diff_adj = sens1P$K_adj/0.25 * sens1P$Vh_cmh2  

# missaligment correction from White, 2021

Hour <- as.POSIXlt(sens1P$fecha_local)$hour
Align_Correct <- mean(subset(sens1P, Hour > 23 | Hour <=4,)$Vh_diff_adj, na.rm = TRUE)

sens1P$Vh_cmhr_corr <- sens1P$Vh_diff_adj - Align_Correct

# All corrections sens2P ----

sens2P$Vh_cmh2 = Vh(k = 0.0025, x = 0.6, dif_delta_T = sens2P$T1T2) # Heat pulse velocity (Vh) in cm/hr
sens2P$Mc = Mc(Mf = P2_mf, Md = P2_md) # Mc (-)
sens2P$Rho = Rho(Mf = P2_mf, Vw = P2_Vw) # (g/cm3)
sens2P$rho_b = Rho_b(Md = P2_md, Vw = P2_Vw) # (g/cm3)
sens2P$Vf_h2o = Vf_h2o(Mf = P2_mf, Md = P2_md, Vw = P2_Vw) # (g/cm3)
sens2P$C = C(Md = P2_md, Cw = 1200, Cs = 4182, Mf = P2_mf) # (J / kg * °C)
sens2P$Fv = Fv(rho_b = sens2P$rho_b, Vf_h2o = sens2P$Vf_h2o)
sens2P$Kw = Kw(Fv = sens2P$Fv)
sens2P$kgw = kgw(ks = 0.5984, Mc = sens2P$Mc, rho_b = sens2P$rho_b, rho_s = 1, kw = sens2P$Kw)
sens2P$K_adj = k_adj(kgw = sens2P$kgw, rho = sens2P$Rho, C = sens2P$C)
sens2P$Vh_diff_adj = sens2P$K_adj/0.25 * sens2P$Vh_cmh2  

# missaligment correction from White, 2021

Hour <- as.POSIXlt(sens2P$fecha_local)$hour
Align_Correct <- mean(subset(sens2P, Hour > 23 | Hour <=4,)$Vh_diff_adj, na.rm = TRUE)

sens2P$Vh_cmhr_corr <- sens2P$Vh_diff_adj - Align_Correct

# All corrections sens1M ----

sens1M$Vh_cmh2 = Vh(k = 0.0025, x = 0.6, dif_delta_T = sens1M$T1T2) # Heat pulse velocity (Vh) in cm/hr
sens1M$Mc = Mc(Mf = M1_mf, Md = M1_md) # Mc (-)
sens1M$Rho = Rho(Mf = M1_mf, Vw = M1_Vw) # (g/cm3)
sens1M$rho_b = Rho_b(Md = M1_md, Vw = M1_Vw) # (g/cm3)
sens1M$Vf_h2o = Vf_h2o(Mf = M1_mf, Md = M1_md, Vw = M1_Vw) # (g/cm3)
sens1M$C = C(Md = M1_md, Cw = 1200, Cs = 4182, Mf = M1_mf) # (J / kg * °C)
sens1M$Fv = Fv(rho_b = sens1M$rho_b, Vf_h2o = sens1M$Vf_h2o)
sens1M$Kw = Kw(Fv = sens1M$Fv)
sens1M$kgw = kgw(ks = 0.5984, Mc = sens1M$Mc, rho_b = sens1M$rho_b, rho_s = 1, kw = sens1M$Kw)
sens1M$K_adj = k_adj(kgw = sens1M$kgw, rho = sens1M$Rho, C = sens1M$C)
sens1M$Vh_diff_adj = sens1M$K_adj/0.25 * sens1M$Vh_cmh2  

# missaligment correction from White, 2021

Hour <- as.POSIXlt(sens1M$fecha_local)$hour
Align_Correct <- mean(subset(sens1M, Hour > 23 | Hour <=4,)$Vh_diff_adj, na.rm = TRUE)

sens1M$Vh_cmhr_corr <- sens1M$Vh_diff_adj - Align_Correct

# All corrections sens2M ----

sens2M$Vh_cmh2 = Vh(k = 0.0025, x = 0.6, dif_delta_T = sens2M$T1T2) # Heat pulse velocity (Vh) in cm/hr
sens2M$Mc = Mc(Mf = M2_mf, Md = M2_md) # Mc (-)
sens2M$Rho = Rho(Mf = M2_mf, Vw = M2_Vw) # (g/cm3)
sens2M$rho_b = Rho_b(Md = M2_md, Vw = M2_Vw) # (g/cm3)
sens2M$Vf_h2o = Vf_h2o(Mf = M2_mf, Md = M2_md, Vw = M2_Vw) # (g/cm3)
sens2M$C = C(Md = M2_md, Cw = 1200, Cs = 4182, Mf = M2_mf) # (J / kg * °C)
sens2M$Fv = Fv(rho_b = sens2M$rho_b, Vf_h2o = sens2M$Vf_h2o)
sens2M$Kw = Kw(Fv = sens2M$Fv)
sens2M$kgw = kgw(ks = 0.5984, Mc = sens2M$Mc, rho_b = sens2M$rho_b, rho_s = 1, kw = sens2M$Kw)
sens2M$K_adj = k_adj(kgw = sens2M$kgw, rho = sens2M$Rho, C = sens2M$C)
sens2M$Vh_diff_adj = sens2M$K_adj/0.25 * sens2M$Vh_cmh2  

# missaligment correction from White, 2021

Hour <- as.POSIXlt(sens2M$fecha_local)$hour
Align_Correct <- mean(subset(sens2M, Hour > 23 | Hour <=4,)$Vh_diff_adj, na.rm = TRUE)

sens2M$Vh_cmhr_corr <- sens2M$Vh_diff_adj - Align_Correct


# Plots ----

sensHualo = left_join(sens1H, sens2H, "fecha_local")
sensPinos = left_join(sens1P, sens2P, "fecha_local")
sensMat   = left_join(sens1M, sens2M, "fecha_local")

# It does! Let's go to the its format like seen above, and make the dygraph
don <- xts(x = sens1M$Vh_cmhr_corr, order.by = sens1M$fecha_local)

# Chart
p <- dygraph(don)
p

##### otro plot

# Then you can create the xts format:
don=xts( x=sensMat[,c(19,37)], order.by=sensMat$fecha_local)

# Chart
p <- dygraph(don)
p


