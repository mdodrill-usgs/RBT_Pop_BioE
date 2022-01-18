###############################################################################
# Bioenergetics model code for manuscript:
# 
# You are what you eat: effects of ecosystem drivers on secondary production
# estimated from fish consumption and invertebrate drift.
#
# Notes: 
# * set working directory to access required data and functions
# * the function 'calc_metabolism' will write a .csv file and return an object
#   with the model results 
#
###############################################################################
require(reshape2)
require(dplyr)

# setwd("add working directory here")

source("calc_metabolism.R")
#-----------------------------------------------------------------------------#

# run model with plants as a portion of the diet
run_with_p = calc_metabolism(with.plants = TRUE)

# run model without plants as a portion of the diet
run_without_p = calc_metabolism(with.plants = FALSE)

#-----------------------------------------------------------------------------#
