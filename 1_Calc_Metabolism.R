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
rm(list = ls(all = TRUE))

library(reshape2)
library(dplyr)

# load functions stored in R.script
# source("C:/Users/mdodrill/Desktop/RBT_BioE/Git/RBT_Pop_BioE/BioE_Functions_V1.R", chdir = F)
source("./BioE_Functions_V1.R", chdir = F)

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
             with.plants = FALSE,             
             SIMPLIFY = F)

base_mat[,6:12] = matrix(unlist(do.call(rbind, tmp)), ncol = 7)
names(base_mat)[6:12] = names(tmp[[1]])


keep = c("dat.in.all", "base_mat")
rm(list = setdiff(ls(), keep))
#-----------------------------------------------------------------------------#