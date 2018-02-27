###############################################################################
#                                                                      Feb 2018
#
#   Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * This script takes the basal metabolism (calculated in
#    "1_Calc_Metabolism.R") and scales up to population level metrics using the
#    abundance and growth data provided by Korman
#  * Run "1_AGFD_Calc_Metabolism.R" first, then this script
#
#  To do:
#  * 
###############################################################################

#-----------------------------------------------------------------------------#
# Organize the base matabolism estimates (so this matches the col. order in
# '2_Pop_Expand.r')

all2 = cbind(base_mat[,c(1:2)],
             N = dat.in.all$N,
             Growth = base_mat$Growth,
             base_mat[,c(3:12)])

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

#---------------------
# Calculation converts Kg AFDM in Lees Ferry reach to g AFDM m-2 da-1 

# PopDaCTotInvgafdmm2 is the average amount of daily invertebrate biomass
# consumed per m^2 . Biomass units are g AFDM m-2 da-1 in Lees Ferry

# PopDaCTotInvgafdmm2 = PopDaCTotInvKgafdm / (GCLengthm * GCWidthm * Kgtog)    # bad

# all2$PopDaCTotInvgafdmm2 = (all2$PopDaCTotInvKgafdm / GCLengthm * GCWidthm) * 1000    # BAD !

all2$PopDaCTotInvgafdmm2 = (all2$PopDaCTotInvKgafdm * 1000) / (GCLengthm * GCWidthm)  # GOOD


#-----------------------------------------------------------------------------#

# write.table(all2, file = "RBT_BioE_Output_Values_11_14_17_No_Plants.csv", sep = ",", row.names = F)
# write.table(Bio, file = "RBT_BioE_Josh_Bio_11_15_17.csv", sep = ",", row.names = F)
# write.table(Growth, file = "RBT_BioE_Josh_Growth_11_15_17.csv", sep = ",", row.names = F)
# write.table(MidWgt, file = "RBT_BioE_Josh_Midwgt_11_15_17.csv", sep = ",", row.names = F)
# write.table(N, file = "RBT_BioE_Josh_N_11_15_17.csv", sep = ",", row.names = F)

#-----------------------------------------------------------------------------#