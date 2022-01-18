# two functions:

# 1). metabolizer - takes physical/biological inputs and calculates bioenergetics estimates
# * arguments are:
#   sec - seconds of daylight for foraging
#   temp - water temperature, degrees C
#   del_G - delta growth over the interval, grams
#   N - rainbow trout abundance, numbers of individuals
#   with.plants - logical, turns plants in the diet on/off

# 2). calc_metabolism - creates inputs and runs 'metabolizer()'
# * arguments are:
#   temp.adj - temperature adjustment (not used, see note below), degrees C
#   with.plants logical, turns plants in the diet on/off
# * data sources are:
#   InputTempDL_11.06.2021.csv - temperature, daylength for each date
#   Interp.Rdata - rainbow trout data (inputs to 'metabolizer()')
# * return - both a data.frame with results and writes to file (.csv)
#   note this will overwrite the csv, if run on the same day (as the date is
#   appended)


#-----------------------------------------------------------------------------#
metabolizer = function(sec, Temp, M, del_G, N, with.plants = NULL){
  #----------------------
  # Equation S1
  # Rr is the energy expenditure for standard metabolism, units are joules per
  # gram of fish 'wet weight 'per day (Reference Elliot 1976, Table 5, pg 933,
  # based on minimum rations), 
  
  # Units are small calories per day, c da-1; therefore, value needs to be converted to joules
  
  # Rs params
  a1 = 8.277       # basal metabolism constant
  b1 = 0.731       # fish mass coef
  b2 = 0.094       # temperature coef
  K2 = 4.184       # converts small calories into Joules
  
  Rr = exp(log(a1) + (b1 * log(M)) + (b2 * Temp)) * K2
  #----------------------
  # Equation S2
  # Ra is the cost for swimming activity
  
  # Units are joules per day, J da-1 (actually Watts converted to J per sec (M =
  # J s-1 accounted for by seconds in a day.
  
  # sec = this is the day length in seconds (input file)
  tot.s = 60 * 60 * 24 # seconds in a day
  
  pDL = sec / tot.s # daylight proportion
  # KW = Individual fish weight, units are Kg in wet weight; therefore divide M by 1000
  
  # Ra params
  a2 = 1.24 #From Tucker 1973 used to estimate metabolic costs (units are J m^-1) for a swimming pokiliotherm, active metabolism intercept, Peters 1983,
  b3 = 0.705 # mass exponent
  
  #From Ware 1978 used to estimate optimal velocity for a swimming pokiliotherm (salmon)
  a3 = 0.285 # preferred velocity intercept, Tudorache et al. 2011, Modified relationship so that a trout 200 mm swam 20 cm/sec
  b4 = 0.136 # Velocity coefficient 
  
  
  Vp = a3 * (M * 0.001)^b4 # optimal swimming speed #This is equivalent to a 200 mm fish swimming 20 cm/sec almost equivalent to 
  
  Ra = a2 * (M * 0.001)^b3 * Vp * pDL * tot.s  
  
  # Example, Mdz = 78, M = 4.9*.001 Kg, pD = 0.4 then Ra = 140 J/da
  
  #----------------------
  # Equation S4 and S5
  # Rd is the cost associated with digestion and protein synthesis (specific
  # dynamic action, SDA)
  
  # Units are J da-1
  
  # pI, is the diet proportion consisting of invertebrates in trout diet based on
  # fish size and its inverse (1 - pA for plants). This is an empirical
  # relationship used to estimate the energetic contribution from algae
  # (unpublished data, Yard, R2 = 0.87).
  
  # Rd params
  a4 = 0.068   # Algae diet intercept
  b5 = 0.00069 # Algae diet coefficient
  
  SI = .252   # Invertebrate SDA proportion, Peters 1983, modified from Ricklefs 1974
  SA = .101   # Algae SDA proportion, Hanson et al. 1997 
  
  
  pA = a4 + (b5 * M)
  
  # This will turn off or on the plant proportion in the consumption estimate
  if(with.plants == TRUE){
    pI = 1 - pA 	 # this is the proportion of invertebrates in the diet based on weight (g)
  } else {
    pI = 1
    pA = 0 
  }
  
  Rd = pI * ((Rr + Ra) / (1 - SI)) + pA * ((Rr + Ra) / (1 - SA)) - (Rr + Ra) 
  
  #----------------------
  # Equation S6
  # E is the excretory cost per day (E; J da-1)
  
  # pE is the proportion of energy lost to total metabolism (Rt) from excretion
  
  pE = 0.078 # data from Xie et al. 1997
  
  Rt = Rr + Ra + Rd
  
  E = pE * Rt
  
  #----------------------
  # equation 4, main text 
  # Total amount of ingested energy
  # del G is the daily change in fish mass (gains or losses from somatic and
  # gonadal growth) at an individual level
  
  # del_G is in grams wet weight --> convert to joules
  # wwtoJ = 7000 # wet weight (ww) to joules (J)
  
  del_G_joules = del_G * (7*10^3) 
  
  I = Rr + Ra + Rd + E + del_G_joules
  
  #----------------------
  # equation 5, main text 
  # consumption
  
  AI = 0.783 # invertebrate assimilation efficiency, Brocksen and Bugge 1974
  AP = 0.11  # plant assimilation efficiency, Liebfried 1987
  
  Con.J = I / (AI + ((pA / pI) * AP)) # units of joules
  
  # Conversion from joules to invertebrate biomass (afdm)
  
  # K1 is constant in manuscript that converts to joules to afdm, 
  # Source material from Peters 1983; Ricciardi and Bourget 1988; Ricklefs 1974
  # Reference excel spreadsheet called Conversion_Factor_from_J_to_g_afdm.xlsx
  
  K1 = 3.264 * 10^-5
  
  Con.g = Con.J * (K1) # units of g afdm
  Pop.Con.Kg = N * Con.g * .001
  
  #----------------------
  # Estimate Cmin (minimum daily ration) in units of afdm.
  
  I.min.J = I - del_G_joules
  
  Cmin.J = I.min.J / (AI + ((pA / pI) * AP)) # units of joules
  
  Cmin.g = Cmin.J * (K1)  # units of afdm
  
  #----------------------
  # Estimate biomass per meter squared of invertebrates consumed by trout
  # Estimate is a per meter squared assuming that invertebrates are uniformly consumed
  # from the benthos in Glen Canyon
  
  L = 25500 	# Stream length (m) of Glen Canyon
  W = 120	    # Average stream width (m) of Glen Canyon
  
  Con.g.m2 = (Pop.Con.Kg * 1000) / (L * W)
  
  #----------------------  
  # rename these here for what Yard prefers 
  return(list(Rr.J.g.day = Rr, Rd.J.day = Rd, Ra.J.day = Ra, E.J.day = E, DelG.J.day = del_G_joules,
              I.J.day = I, Cmin.J.day = Cmin.J, Cmin.g.afdm.day = Cmin.g,
              Con.J.day = Con.J, Con.g.afdm.day = Con.g, Con.g.afdm.m2.day = Con.g.m2,
              Pop.Con.Kg.afdm.day = Pop.Con.Kg))
}

#-----------------------------------------------------------------------------#
# Note: temp.adj is not used in manuscript (i.e., temp.adj = 0)

calc_metabolism = function(temp.adj = 0, with.plants = NULL){
  # catch case where with.plants is not supplied 
  if(is.null(with.plants) == TRUE) {
    stop("with.plants argument required, either TRUE or FALSE")
  }
  
  # only accept T or F for with.plants
  if(any(with.plants %in% c("TRUE", "FALSE")) == FALSE){
    stop("with.plants can only be TRUE or FLASE")
  }
  
  # update 11/5/21 
  day.in = read.table(file = "InputTempDL_11.06.2021.csv", header = T, sep = ",") %>% 
    select(Date, DL, T = Avg_Temp_C)
  
  day.in$Date = as.Date(day.in$Date, format = "%m/%d/%Y")
  
  #-----------------------------------------------------------------------------#
  load("Interp.RData")
  
  w.in = as_tibble(as.data.frame(MidWgt))
  dates = seq.Date(from = as.Date("2012/4/19"), to = as.Date("2012/4/19") + Ndays - 1, by = "day")
  
  colnames(w.in) = as.character(dates)
  w.in$MidSz = MidLen + 3    # names of size binds are different (represent the same bin), adjust here
  
  w.in.2 = melt(w.in, id.vars = c("MidSz"))
  names(w.in.2)[2:3] = c("Date", "fish.mass")
  
  w.in.2$Date = as.Date(w.in.2$Date, format = "%Y-%m-%d")
  
  dat.in.all = left_join(w.in.2, day.in, by = c("Date")) 
  
  # temperature adjustment
  dat.in.all$T = dat.in.all$T + temp.adj
  
  base_mat = dat.in.all
  
  #-----------------------------------------------------------------------------#
  # do some formatting of the data Josh sent
  
  load("Interp.RData")
  
  my.N = as_tibble(as.data.frame(N))
  dates = seq.Date(from = as.Date("2012/4/19"), to = as.Date("2012/4/19") + Ndays - 1, by = "day")
  
  colnames(my.N) = as.character(dates)
  my.N$MidSz = MidLen + 3   # names of size binds are different (represent the same bin), adjust here
  
  my.N.2 = melt(my.N, id.vars = c("MidSz"))
  names(my.N.2)[2:3] = c("Date", "N")
  
  my.Growth = as_tibble(as.data.frame(Growth))
  dates = seq.Date(from = as.Date("2012/4/19"), to = as.Date("2012/4/19") + Ndays - 1, by = "day")
  
  colnames(my.Growth) = as.character(dates)
  my.Growth$MidSz = MidLen + 3    # names of size binds are different (represent the same bin), adjust here
  
  my.G.2 = melt(my.Growth, id.vars = c("MidSz"))
  names(my.G.2)[2:3] = c("Date", "Growth")
  
  N.G = left_join(my.N.2, my.G.2, by = c("MidSz", "Date"))
  N.G$Date = as.Date(N.G$Date, format = "%Y-%m-%d")
  
  #-----------------------------------------------------------------------------#
  # Match up the base matabolism estimates with the N & Growth data for each size
  # bin and date
  
  all = left_join(N.G, base_mat, by = c("MidSz", "Date"))
  
  # since the input sizes don't match, cut down the data to only what matches
  # between the input from Yard and that from Korman (N & G)
  all2 = all[which(!is.na(all$DL)),] 
  
  dat.all = all2
  #-----------------------------------------------------------------------------#
  # Below this runs the function 'metabolizer' for all the input data
  # Set the argument with.plants to TRUE or FALSE
  
  index = seq(1:nrow(dat.all))
  
  # run the metabolism calculations across the input data - see source
  tmp = mapply(metabolizer,
               sec = dat.all[index,6],
               Temp = dat.all[index,7],
               M = dat.all[index,5],
               del_G = dat.all[index,4],
               N = dat.all[index,3],
               with.plants = with.plants,
               SIMPLIFY = F)
  
  # append results as additional columns
  dat.all[,8:19] = matrix(unlist(do.call(rbind, tmp)), ncol = 12)
  names(dat.all)[8:19] = names(tmp[[1]])
  
  # change name "Growth" to "DelG.g" 
  names(dat.all)[4] = "DelG.g.day"
  
  # flag in output for inclusion of plants in diet
  dat.all$with.plants = with.plants
  
  # write to file in working directory, with the date run
  today = format(Sys.time(), "%Y_%m_%d")
  write.csv(dat.all, file = paste0("RBT_BioE_Output_", today, ".csv"),
            row.names = FALSE)
  
  return(dat.all)
  
}

#-----------------------------------------------------------------------------#
