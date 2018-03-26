###############################################################################
#                                                                      Mar 2018
#
#          Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * 
#
#  To do:
#  * Add in plotting functions
###############################################################################


source("C:/Users/mdodrill/Desktop/RBT_BioE/Git/RBT_Pop_BioE/BioE_Functions_V1.R", chdir = F)

#-----------------------------------------------------------------------------#
# NO project. Recent time period: 2012 - 2016 

run = calc_metabolism(project = "NO")
# run = calc_metabolism(project = "NO", avg.temp = TRUE)  # this will use a 10 yr avg. temperature regime

dat = pop_expand(base_mat = run$base_mat, dat.in.all = run$dat.in.all)

# will write an output file
dat = pop_expand(base_mat = run$base_mat, dat.in.all = run$dat.in.all, write.output = "Run_1")



#-----------------------------------------------------------------------------#
# Lees Ferry long term monitoring data: 199X - 20XX 

run = calc_metabolism(project = "monitoring", avg.temp = TRUE)

dat = pop_expand(base_mat = run$base_mat, dat.in.all = run$dat.in.all)


#-----------------------------------------------------------------------------#
# Run model for a range of temperatures
big.out = list()

temps = c(4, 2, 0, -2, -4)

for(i in seq_along(temps)){
  
  run = calc_metabolism(project = "NO", temp.adj = temps[i])
  
  big.out[[i]] = pop_expand(base_mat = run$base_mat, dat.in.all = run$dat.in.all)
}

# write.table(big.out[[1]], file = "RBT_BioE_Output_Values_03_22_18_Temp_plus_4.csv", sep = ",", row.names = F)
# write.table(big.out[[2]], file = "RBT_BioE_Output_Values_03_22_18_Temp_plus_2.csv", sep = ",", row.names = F)
# write.table(big.out[[3]], file = "RBT_BioE_Output_Values_03_22_18_Temp_plus_0.csv", sep = ",", row.names = F)
# write.table(big.out[[4]], file = "RBT_BioE_Output_Values_03_22_18_Temp_minus_2.csv", sep = ",", row.names = F)
# write.table(big.out[[5]], file = "RBT_BioE_Output_Values_03_22_18_Temp_minus_4.csv", sep = ",", row.names = F)

#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from
# invertebrates consumed at a population level (N of size-bin). Energy units are
# MJ.

all2 = big.out[[3]]


my.mids = seq(75, 325, 50) +3


dat.in = all2[which(all2$MidSz %in% my.mids),]

# pop.2.b = group_by(all2, Date, sz.group) %>%
#   summarize(tot.PopDadelGInvMJ = sum(PopDadelGInvMJ))
# 
# pop.2.c = group_by(all2, Date, sz.group) %>%
#   summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))


# dat = merge(pop.2.b, pop.2.c, by = c("Date", "sz.group"))
# 
# dat.2 = melt(dat, id.vars = c("Date", "sz.group"))


dat.in.2 = dat.in[,which(names(dat.in) %in% c("Date", "MidSz", "PopDadelGInvMJ", "PopDaCMinInvMJ", "PopDaCTotInvMJ"))]

dat.in.3 = melt(dat.in.2, id.vars = c("Date", "MidSz"))

library(ggplot2)
library(ggthemes)


p.13 = ggplot(dat.in.3, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_x_date(date_breaks = "6 month", date_labels = "%b \n %y") +
  labs(title = "", y = "") + 
  facet_wrap(~ MidSz) +
  theme_base() +
  theme(legend.position = "top")


p.13 



#-----------------------------------------------------------------------------#






