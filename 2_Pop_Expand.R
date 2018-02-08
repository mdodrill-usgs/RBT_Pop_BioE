###############################################################################
#                                                                      Aug 2017
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
#  * Get the physical data input until 6/10/2016 (Yard)
###############################################################################
load("C:/Users/mdodrill/Desktop/RBT_BioE/From_Korman/Interp.RData")

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

all2$PopDadelGMJ = all2$Growth * all2$N * .001 * KgwwtoMJ                             # is the convert to KG done above in KgwwtoMJ?

#---------------------
# PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from
# invertebrates consumed at a population level (N of size-bin). Energy units are
# MJ. (pDi is the proportion of the diet composed of invertebrates, and Ai is
# the assimilation efficiency of invertebrates)

# PopDadelGInvMJ = PopDadelGMJ * (pDi) / (Ai) 
# Ai = 0.783      # Assimilation efficiency of invertebrates

all2$PopDadelGInvMJ = all2$PopDadelGMJ * all2$pDi / 0.783                             # should the Ai be * instead of /

#---------------------
# PopDadelGPlaMJ is the amount of daily energy ascribed to growth derived from
# plant matter consumed at a population level (N of size-bin). Energy units are
# MJ. (pDp is the proportion of the diet composed of plant matter, and Ap is the
# assimilation efficiency of plant matter)

# PopDadelGPlaMJ = PopDadelGMJ * (pDp) / (Ap) 
# Ap = 0.11       # assimilation efficiency of plants 

all2$PopDadelGPlaMJ = all2$PopDadelGMJ * (1 - all2$pDi) / 0.11                       # same Q as above? 

#---------------------
# PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of invertebrates) from an individual level that is scaled up
# to a population level (N of size-bin) and converted from KJ to MJ. Units are
# MJ consumed in Lees Ferry da-1.

# PopDaCMinInvMJ = IndCMinInv * DaN * KJtoMJ

all2$PopDaCMinInvMJ = all2$Cmin.Inv * 0.001 * all2$N * 0.001                   # are the units correct?

#---------------------
# PopDaCMinPlaMJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of plant matter) from an individual level that is scaled up to
# a population level (N of size-bin) and converted from KJ to MJ. Units are MJ
# consumed in Lees Ferry da-1.

# PopDaCMinPlaMJ = IndivCMinPlant * DaN * KJtoMJ

all2$PopDaCMinPlaMJ = all2$Cmin.Pla * 0.001 * all2$N * 0.001                  # are the units correct?

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


#-----------------------------------------------------------------------------#

# write.table(all2, file = "RBT_BioE_Output_Values_11_14_17_No_Plants.csv", sep = ",", row.names = F)
# write.table(Bio, file = "RBT_BioE_Josh_Bio_11_15_17.csv", sep = ",", row.names = F)
# write.table(Growth, file = "RBT_BioE_Josh_Growth_11_15_17.csv", sep = ",", row.names = F)
# write.table(MidWgt, file = "RBT_BioE_Josh_Midwgt_11_15_17.csv", sep = ",", row.names = F)
# write.table(N, file = "RBT_BioE_Josh_N_11_15_17.csv", sep = ",", row.names = F)


