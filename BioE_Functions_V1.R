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

#-----------------------------------------------------------------------------#
# get the directory from where the script lives (not using Rstudio to 'source',
# or when you use source in a script)
str.dir = getSrcDirectory(function(x) {x})

data.dir = paste0(str.dir, "/Data_In")
setwd(data.dir)

#-----------------------------------------------------------------------------#
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
  
  tot.s = 60 * 60 * 24
  
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
#-----------------------------------------------------------------------------#

###############################################################################
#                                                                      Feb 2018
#
#          Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * Made a function for the metabolic calc. and added this to BioE_Functions_V1
#  * Need to change the directory for sourcing the 'BioE_Functions_V1.R', but
#    this will find the correct directory for the data.
#  * This script will do the per capita metabolic calcs. 
#
#  To do:
#  * 
#  
###############################################################################
# rm(list = ls(all = TRUE))

calc_metabolism = function(project = NULL, temp.adj = 0, with.plants = FALSE){
  require(reshape2)
  require(dplyr)
  
  # load functions stored in R.script
  source("C:/Users/mdodrill/Desktop/RBT_BioE/Git/RBT_Pop_BioE/BioE_Functions_V1.R", chdir = F)
  
  if(project == "NO"){
    day.in = read.table(file = "Input_File_Date_DL_T.csv", header = T, sep = ",")
    
    day.in$Date = as.Date(day.in$Date, format = "%m/%d/%Y")
    
    w.in = read.table(file = "Input_File_Date_MidSz_Weight.csv", header = T, sep = ",")
    
    # exclude blank cols that may get imported (probably a better way to do this?)
    w.in.2 = w.in[,which(!is.na(colSums(w.in)))]
    
    # change the col names to the date (not the funky format which is imported)
    #----------> can't have dates as the column name, so go with character
    
    names(w.in.2)[2:ncol(w.in.2)] = as.character(as.Date(substr(names(w.in.2)[2:ncol(w.in.2)], 2,
                                                                nchar(names(w.in.2)[2:ncol(w.in.2)])),
                                                         format = "%m.%d.%Y"))
    
    # do some formatting to the fish mass data
    w.in.3 = melt(w.in.2, id.vars = c("MidSz"))
    names(w.in.3)[2:3] = c("Date", "fish.mass")
    
    w.in.3$Date = as.Date(w.in.3$Date, format = "%Y-%m-%d")
    
    dat.in.all = left_join(w.in.3, day.in, by = c("Date")) 
    
    # temperature adjustment
    dat.in.all$T = dat.in.all$T + temp.adj
    
  } else {
    if(project == "monitoring"){
      day.in = read.table(file = "Pgrow and CF for AGF LF 1991 to 2016 DD NOtoAGF.csv", header = T, sep = ",")
      day.in$Date = as.Date(day.in$Date, format = "%m/%d/%Y")
      
      dat.in.all = day.in[,c(2:7)]
      
      # temperature adjustment
      dat.in.all$T = dat.in.all$T + temp.adj
    }
  }
  
  #-----------------------------------------------------------------------------#
  # Below this runs the function 'metabolizer' for all the input data
  # Set the argument with.plants to TRUE or FALSE
  
  base_mat = dat.in.all
  
  index = seq(1:nrow(dat.in.all))
  
  tmp = mapply(metabolizer,
               sec = dat.in.all[index,4],
               Temp = dat.in.all[index,5],
               W = dat.in.all[index,3],
               # with.plants = TRUE,              
               # with.plants = FALSE,             
               SIMPLIFY = F)
  
  base_mat[,6:12] = matrix(unlist(do.call(rbind, tmp)), ncol = 7)
  names(base_mat)[6:12] = names(tmp[[1]])
  
  if(project == "monitoring"){
    base_mat$Growth = day.in$del_G  
  }
  
  attr(dat.in.all, 'project') <- project
  attr(base_mat, 'project') <- project
  
  return(list("dat.in.all" = dat.in.all, "base_mat" = base_mat))
}


#-----------------------------------------------------------------------------#
###############################################################################
#                                                                      Feb 2018
#
#   Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * This script takes the basal metabolism (calculated in
#    "1_Calc_Metabolism.R") and scales up to population level metrics using the
#    abundance and growth data provided by Korman
#  * Run "1_Calc_Metabolism.R" first, then this script
#
#  To do:
#  * 
###############################################################################

pop_expand = function(base_mat, dat.in.all){
  
  if(attributes(dat.in.all)$project == "NO"){
    load("Interp.RData")
    
    #-----------------------------------------------------------------------------#
    # do some formatting of the data Josh sent
    my.N = tbl_df(as.data.frame(N))
    dates = seq.Date(from = as.Date("2012/4/1"), to = as.Date("2012/4/1") + Ndays - 1, by = "day")
    
    colnames(my.N) = as.character(dates)
    my.N$MidSz = MidLen + 3    # Yard and Korman call the size bins different things 
    
    my.N.2 = melt(my.N, id.vars = c("MidSz"))
    names(my.N.2)[2:3] = c("Date", "N")
    
    
    my.Growth = tbl_df(as.data.frame(Growth))
    dates = seq.Date(from = as.Date("2012/4/1"), to = as.Date("2012/4/1") + Ndays - 1, by = "day")
    
    colnames(my.Growth) = as.character(dates)
    my.Growth$MidSz = MidLen + 3    # Yard and Korman call the size bins different things 
    
    my.G.2 = melt(my.Growth, id.vars = c("MidSz"))
    names(my.G.2)[2:3] = c("Date", "Growth")
    
    N.G = left_join(my.N.2, my.G.2, by = c("MidSz", "Date"))
    N.G$Date = as.Date(N.G$Date, format = "%Y-%m-%d")
    
    #-----------------------------------------------------------------------------#
    # Match up the base matabolism estimates with the N & Growth data for each sz bin and date
    
    all = left_join(N.G, base_mat, by = c("MidSz", "Date"))
    
    # since the input sizes don't match, cut down the data to only what matches
    # between the input from Yard and that from Korman (N & G)
    all2 = all[which(!is.na(all$DL)),]  
    
  } else {
    
    if(attributes(dat.in.all)$project == "monitoring"){
      # Organize the base matabolism estimates (so this matches the col. order in
      # '2_Pop_Expand.r')
      
      all2 = cbind(base_mat[,c(1:2)],
                   N = dat.in.all$N,
                   Growth = base_mat$Growth,
                   base_mat[,c(3:12)])
    }
  }
  
  #-----------------------------------------------------------------------------#
  # Conversions & Constants
  gwwtoJ = 7 * 10 ^ 9	   # wet weight (g) to J
  KgwwtoMJ = gwwtoJ * 0.001 / 1000000		# wet weight (Kg) to MJ
  wwtoafdm = 0.165	# wet weight to AFDM, median value of amphipods, Ricciardi and Bourget 1998)
  GCLengthm = 25406.11 # meters
  GCWidthm = 123.83 # meters
  
  #-----------------------------------------------------------------------------#
  # PopDadelGMJ is the amount of daily growth converted to units of energy (MJ)
  # consumed. This is scaled up from an individual to a population level (N of
  # size-bin), which is converted from wet-weight (g) Kg to units of energy MJ.
  # Units are MJ of delG in Lees Ferry da-1 (energy source unknown [plants,
  # invertebrates, or fish]).
  
  # PopDadelGMJ = IndDadelGgww * DaN * gtoKg * KgwwtoMJ
  
  all2$PopDadelGMJ = all2$Growth * all2$N * .001 * KgwwtoMJ                            
  
  #---------------------
  # PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from
  # invertebrates consumed at a population level (N of size-bin). Energy units are
  # MJ. (pDi is the proportion of the diet composed of invertebrates, and Ai is
  # the assimilation efficiency of invertebrates)
  
  # PopDadelGInvMJ = PopDadelGMJ * (pDi) / (Ai) 
  # Ai = 0.783      # Assimilation efficiency of invertebrates
  
  all2$PopDadelGInvMJ = all2$PopDadelGMJ * all2$pDi / 0.783                            
  
  #---------------------
  # PopDadelGPlaMJ is the amount of daily energy ascribed to growth derived from
  # plant matter consumed at a population level (N of size-bin). Energy units are
  # MJ. (pDp is the proportion of the diet composed of plant matter, and Ap is the
  # assimilation efficiency of plant matter)
  
  # PopDadelGPlaMJ = PopDadelGMJ * (pDp) / (Ap) 
  # Ap = 0.11       # assimilation efficiency of plants 
  
  all2$PopDadelGPlaMJ = all2$PopDadelGMJ * (1 - all2$pDi) / 0.11                       
  
  #---------------------
  # PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on
  # the consumption of invertebrates) from an individual level that is scaled up
  # to a population level (N of size-bin) and converted from KJ to MJ. Units are
  # MJ consumed in Lees Ferry da-1.
  
  # PopDaCMinInvMJ = IndCMinInv * DaN * KJtoMJ
  
  all2$PopDaCMinInvMJ = all2$Cmin.Inv * 0.001 * all2$N * 0.001                  
  
  #---------------------
  # PopDaCMinPlaMJ expands Cmin (energy consumed for maintenance levels based on
  # the consumption of plant matter) from an individual level that is scaled up to
  # a population level (N of size-bin) and converted from KJ to MJ. Units are MJ
  # consumed in Lees Ferry da-1.
  
  # PopDaCMinPlaMJ = IndivCMinPlant * DaN * KJtoMJ
  
  all2$PopDaCMinPlaMJ = all2$Cmin.Pla * 0.001 * all2$N * 0.001                  
  
  #---------------------
  # PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
  # level (N of size-bin) that is derived from solely from invertebrates. Units
  # are MJ consumed in Lees Ferry da-1.
  
  # PopDaCTotInvMJ = PopDaCMinInvMJ + PopDadelGInvMJ
  
  all2$PopDaCTotInvMJ = all2$PopDaCMinInvMJ + all2$PopDadelGInvMJ
  
  #---------------------
  # Calculation converts Total MJ in Lees Ferry reach per day to Kg 
  
  # PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed
  # at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry
  # da-1.
  
  # PopDaCTotInvKgafdm = PopDaCTotInvMJ  / (KgwwtoMJ  * wwtoafdw)              
  
  # all2$PopDaCTotInvKgafdm = all2$PopDaCTotInvMJ  / (KgwwtoMJ  * wwtoafdm)  #bad
  all2$PopDaCTotInvKgafdm = (all2$PopDaCTotInvMJ  / KgwwtoMJ)  * wwtoafdm   # good
  
  #---------------------
  # Calculation converts Kg AFDM in Lees Ferry reach to g AFDM m-2 da-1 
  
  # PopDaCTotInvgafdmm2 is the average amount of daily invertebrate biomass
  # consumed per m^2 . Biomass units are g AFDM m-2 da-1 in Lees Ferry
  
  # PopDaCTotInvgafdmm2 = PopDaCTotInvKgafdm / (GCLengthm * GCWidthm * Kgtog)    # bad
  
  # all2$PopDaCTotInvgafdmm2 = (all2$PopDaCTotInvKgafdm / GCLengthm * GCWidthm) * 1000    # BAD !
  
  all2$PopDaCTotInvgafdmm2 = (all2$PopDaCTotInvKgafdm * 1000) / (GCLengthm * GCWidthm)  # GOOD
  #---------------------
  # (MD added this one)
  # Calculation converts Total MJ in Lees Ferry reach per day to Kg 
  
  # PopDaCMinInvKgafdm is the Cmin amount of daily invertebrate biomass consumed
  # at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry
  # da-1.
  
  
  all2$PopDaCMinInvKgafdm = (all2$PopDaCMinInvMJ  / KgwwtoMJ)  * wwtoafdm   # good
  
  #---------------------
  # (MD added this one)
  # Calculation converts Total MJ in Lees Ferry reach per day to Kg 
  
  # PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from
  # invertebrates consumed at a population level (N of size-bin). Biomass units
  # are Kg AFDM in Lees Ferry da-1.
  
  
  all2$PopDadelGInvKgafdm = (all2$PopDadelGInvMJ  / KgwwtoMJ)  * wwtoafdm   # good
  
  #-----------------------------------------------------------------------------#
  
  # write.table(all2, file = "RBT_BioE_Output_Values_11_14_17_No_Plants.csv", sep = ",", row.names = F)
  # write.table(Bio, file = "RBT_BioE_Josh_Bio_11_15_17.csv", sep = ",", row.names = F)
  # write.table(Growth, file = "RBT_BioE_Josh_Growth_11_15_17.csv", sep = ",", row.names = F)
  # write.table(MidWgt, file = "RBT_BioE_Josh_Midwgt_11_15_17.csv", sep = ",", row.names = F)
  # write.table(N, file = "RBT_BioE_Josh_N_11_15_17.csv", sep = ",", row.names = F)
  
  #-----------------------------------------------------------------------------#
  
  return(all2)
}
