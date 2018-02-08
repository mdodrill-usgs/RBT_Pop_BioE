###############################################################################
#                                                                      Feb 2018
#
#          Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * These are the functions...
#
#  To do:
#  
###############################################################################

# See Yards spreadsheet for validation the metabolizer
# (Macro_TemperatureDependentMetabolism_2017_07_11_1132)

# prop.s = 0.521527778
# Temp = 12.96354167
# # W = 86.47728289
# W = 4.93487736941781
# 
# tot.s = 60*60*24
# sec = prop.s * tot.s

# metabolizer(sec, Temp, W)

metabolizer = function(sec, Temp, W, with.plants = FALSE){
  
  tot.s = 60*60*24
  
  pDL = sec / tot.s  # = this is the day length in seconds (input file)
  # pDL = prop.s   # = this is the day length in seconds (input file)
  
  # KW = Individual fish weight, units are Kg in wet weight; therefore divide W by 1000
  KW = W / 1000
  
  #----------------------
  # Rs is the energy expenditure for standard metabolism, units are joules per
  # gram of fish 'wet weight 'per day (Reference Elliot 1976, Table 5, pg 933,
  # based on minimum rations)
  
  # Units are small calories per day, c da-1; therefore, value needs to be converted to joules
  
  # Rs params
  a1 = 8.277       # basal metabolism constant
  b1 = 0.731       # fish weight coef
  b2 = 0.0938      # temperature coef
  
  Rs = exp(log(a1) + (b1 * log(W)) + (b2 * Temp)) * 4.184
  
  #----------------------
  # Ra is the cost for swimming activity
  
  # Units are joules per day, J da-1 (actually Watts converted to J per sec (W =
  # J s-1 accounted for by seconds in a day.
  
  # pDL = this is the day length in seconds (input file)
  # KW = Individual fish weight, units are Kg in wet weight; therefore divide W by 1000
  
  # Ra params
  
  # intercept for preferred swimming speed (Upref) Tudorache et al. 2011 20 cm/s
  # sustained swimming speed for a 200 g brook charr
  a3 = 0.25	      
  
  b4 = 0.136 	        # velocity dependence coefficient 
  c1 = 2.42	          # velocity exponent (ms-1)
  
  sec.day = 60 * 60 * 24  # seconds per day (sd-1)
  
  Ra = ((a3 ^ c1 * (KW ^ b4) ^ c1) * pDL * sec.day) + (0.00 ^ c1 * ((KW) ^ b4) ^ c1 * ((1 - pDL) * (sec.day)))
  
  Upref = (a3 ^ c1) * (KW ^ b4)	# preferred swimming speed, units = ms-1
  
  #----------------------
  # Rd is the cost associated with digestion and protein synthesis (specific
  # dynamic action, SDA)
  
  # Units are J da-1
  
  # pDp, is the diet proportion consisting of invertebrates in trout diet based on
  # fish size and its inverse (1 - pD_i for plants). This is an empirical
  # relationship used to estimate the energetic contribution from algae
  # (unpublished data, Yard, R2 = 0.87).
  
  # Rd params
  a4 = -0.32	     # intercept of plant diet proportion
  b5 = 0.253	     # coefficient of plant diet proportion
  
  pSi = 0.252	     # this is the proportional cost of digestion for invertebrates 
  pSp = 0.103 	     # this is the proportional cost of digestion for plants
  
  # pDp = a4 + b5 * log(W)
  
  # pDp = ifelse(-0.32 + 0.253 * (log(W, 10)) <= 0, 0,  -0.32 + 0.253 * (log(W, 10)))
  
  pDp = if(-0.32 + 0.253 * (log(W, 10)) <= 0){
    0
  } else {
    -0.32 + 0.253 * (log(W, 10))
  }
  
  # This will turn off or on the plant proportion in the consumption estimate
  if(with.plants == TRUE){
    pDi = 1 - pDp 	 # this is the proportion of invertebrates in the diet based on weight (g)
  } else {
    pDi = 1
    pDp = 0 
  }

  Rd = (((Rs + Ra) / (1 - pSi)) - (Rs + Ra)) * (pDi) + (((Rs + Ra) / (1 - pSp)) - (Rs + Ra)) * (1 - pDi)
  
  #----------------------
  # E is the excretory cost per day (E; J da-1)
  
  # pE is the proportion of energy lost to total metabolism (Rt) from excretion
  
  pE = 0.079
  
  Rt = Rs + Ra + Rd
  
  E = pE * Rt
  
  In_E = Rt + E
  
  #----------------------
  # F is the amount of unassimilated energy lost through defecation, which is solved by difference.
  
  # Units are joules per day, J da-1
  
  # CInv is the amount of energy that can be ascribed to the consumption of
  # invertebrates if the 'proportion is know or if we decide to use the allometric
  # relationship between fish size and plant 'matter consumed that was observed
  # during the mechanical removal period.
  
  Ai = 0.783      # Assimilation efficiency of invertebrates 
  Ap = 0.11       # assimilation efficiency of plants 
  
  
  Cmin = (Rt + E) / ((Ai * pDi) + Ap * (1 - pDi))
  
  
  Cmin.Inv  = Cmin * pDi	# total energy consumed from invertebrates (CInv)
  Cmin.Pla  = Cmin * pDp	# total energy consumed from plants (MD)  (CPla)
  
  # F = CInv - CInv  * Ai	#F represents the proportion of unassimilated ingesta
  
  
  Cmin.kJ = Cmin / 1000  # MD:convert to KJ to match Yard
  
  Fu.p = Cmin.Pla - (Cmin.Pla * Ap)  # energy unassimilated from plants
  
  Fu.i = Cmin.Inv - (Cmin.Inv * Ai)  # energy unassimilated from inverts
  
  F.tot = Fu.p + Fu.i  # total energy unassimilated (inverts + plants)  
  
  return(list(Cmin.kJ = Cmin.kJ, Cmin.Inv = Cmin.Inv, Cmin.Pla = Cmin.Pla,
              Fu.p = Fu.p, Fu.i = Fu.i, F.tot = F.tot,
              pDi = pDi))
}

# 
# prop.s = 0.521527778
# Temp = 12.96354167
# W = 86.47728289
# 
# out = metabolizer(prop.s, Temp, W)
