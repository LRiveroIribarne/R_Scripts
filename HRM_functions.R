########################################################################
### Functions to calculate sap flow with Heat Ratio Method (HRM)
########################################################################

# Functions ----

# 1. WOOD PROPERTIES ----

Mc = function(Mf = NULL, Md = NULL) { # equation 1
  
  # Mc = Moisture content of the sample (-)
  # Mf = Fresh mass (g)
  # Md = Dry mass (g)
  
  Mc = (Mf - Md) / Mf
  return(Mc)
  
}

Rho = function(Mf = NULL, Vw = NULL) { # equation 2a
  
  # Rho = density of green wood (g/cm3)
  # Mf  = Fresh mass (g)
  # Vw  = Volume of the sample (cm3)
  
  Rho = Mf / Vw
  return(Rho)

}

Rho_b = function(Md = NULL, Vw = NULL){ # equation 2b
  
  # Rho_b = basic wood density (g/cm3)
  # Md = Dry mass (g)
  # Vw  = Volume of the sample (cm3)
  
  Rho_b = Md / Vw
  return(Rho_b)
  
}

Vf_h2o = function(Mf = NULL, Md = NULL, Vw = NULL){ # equation 3
   
 # Vf_h2o = volume fraction of water (g/cm3)
 # Mf  = Fresh mass (g)
 # Md = Dry mass (g)
 # Vw  = Volume of the sample (cm3) 
 
  Vf_h2o = (Mf - Md) / Vw
  return(Vf_h2o)
  
}

# 2 CALCULATING SAP VELOCITY AND TREE WATER USE (SAP FLUX DENSITY) ----
# 2.1 UNCORRECTED HEAT PULSE VELOCITY

Vh = function(k = 0.0025, x = 0.6, dif_delta_T = NULL) { # Burgess, 2001 # equation 4
  
  # Vh = Heat pulse velocity (cm/hr)
  # k = nominal thermal diffusivity for green wood (cm2 / s)
  # x = target distance between heater and each thermocouple (cm)
  # dif_delta_T =  average increase in temperature downstream (C°) / average increase in temperature upstream (C°)
  
  Vh = (k / x) * log(dif_delta_T) * 3600
  return(Vh)
  
}


# 2.2 CORRECTING FOR PROBE ALIGNMENT ERRORS ----

V_adj = function(k = 0.0025, t = 80, dif_delta_T = NULL, x1 = 0.6, x2 = 0.6) { # equation 5
  
  # V_adj = Heat pulse velocity adjusted for probe misalignment (cm/hr)
  # k = nominal thermal diffusivity for green wood (cm2 / s)
  # t = time between the heat pulse and measurement (seg.) 
  # delta_T1 =  average increase in temperature downstream (C°)
  # delta_T2 =  average increase in temperature upstream (C°)
  # x1 = actual distance to the downstream sensor (cm)
  # x2 = actual distance to the upstream sensor (cm)
  
  V_adj = ((2 * k * t * log(dif_delta_T)) - x2^2 + x1^2) / (2 * t * (x1 - x2)) * 3600 
  return(V_adj)
  
}


# 2.3 CALCULATING ACTUAL THERMAL DIFFUSIVITY ----

k_adj = function(kgw = NULL, rho = NULL , C = NULL){ # equation 6
  
  # k_adj = actual thermal diffusivity of sapwood (cm2/s)
  # Kgw = green wood thermal conductivity (J / m * C)
  # rho = density of green wood (kg/m3) - equation 2a
  # C = green wood specific heat capacity (J / kg * C)
  
  k_adj = (kgw / ((rho * 1000) * C)) * 10000
  return(k_adj)
  
}

kgw = function(ks = 0.5984, Mc = NULL, rho_b = NULL, rho_s = 1, kw = NULL){ # equation 7
  
  # Kgw = green wood thermal conductivity (J / m * C)
  # ks = thermal conductivity of sap (water) (J / m * s * C)
  # Mc = moisture content of green sapwood - equation 1 (-)
  # Rho_b = basic density of wood - equation 2b (g/cm3)
  # Rho_s = density of sap (water) (g/cm3) 1 ONLY when you use g/cm3 in rho_b
  # kw = thermal conductivity of dry wood 
  
  kgw = (ks * (Mc * 1000) * ((rho_b * 1000) / (rho_s * 1000))) + (kw * (1 - (Mc * 1000)) * ((rho_b * 1000) / (rho_s * 1000))) 
  return(kgw)
}


Kw = function(Fv = NULL){ # equation 8
  
  # Kw = thermal conductivity of dry wood 
  # Fv = void fraction of wood 
  
  Kw = 0.04182 * (21.0 - 20.0 * Fv)
  return(Kw)
}

Fv = function(rho_b = NULL, Vf_h2o = NULL){ # equation 9 
  
  # Fv =  void fraction of wood 
  # rho_b =  basic density of wood - equation 2b (g/cm3)
  # Vf_h2o = volume fraction of water - equation 3 
  
  Fv = 1 - rho_b * (0.6536 + Vf_h2o)
  return(Fv)
}

C = function(Md = NULL , Cw = 1200, Cs = 4182, Mf = NULL){ # equation 10
  
  # C = specific heat of green wood (J / Kg * °C) 
  # Md = Dry mass (g)
  # Cw = specific heat of dry wood (1200 J / Kg * °C  at 20°C)
  # Cs = specific heat of sap (assumed same as water, 4182 J / kg * °C at 20°C)
  # Mf = Fresh mass (g)

  C = ((Md * 1000) * Cw + (Cs * ((Mf - Md) * 1000))) / (Mf * 1000)
  return(C)
  
}

# 2.4 CORRECTING FOR WOUNDING ----

beta = function(w = NULL) { # equation 11
  
  # beta = wound correction factor
  # w = wound width
  
  beta = 7.06 * w^3 + 6.47 * w^2+ 0.54 * w + 1.53
    return(beta)
  
}


Vc = function(k_adj = NULL, k = NULL, beta = NULL, V_adj = NULL) { # equation 12
  
 # Vc = Heat pulse velocity corrected for both diffusivity and wounding
 # k_adj = actual thermal diffusivity of sapwood (cm2 / s) 
 # k  = thermal diffusivity (cm2 / s)
 # beta = wound correction factor 
 # V_adj = Heat pulse velocity adjusted for probe misalignment (cm / hr)
  
  Vc = (k_adj/k) * beta * V_adj
  return(Vc)
}

# 2.5 CORRECTING FOR WOOD COMPOSITION

Vs = function(Vc = NULL, Rho_b = NULL, Cw = NULL, Mc = NULL, Cs = NULL, rho_s = NULL) { # equation 13
  
  # Vs = 
  
  Vs = (Vc * Rho_b * (Cw + Mc * Cs)) / (rho_s * Cs)
  return(Vs)
}

# 3. CALCULATING TREE AND STAND TRANSPIRATION ----

Js = function(As = NULL, Vs = NULL){ # equation 14
  
  Js = As * Vs * 1000
  return(Js)
  
}

# FIN ----
