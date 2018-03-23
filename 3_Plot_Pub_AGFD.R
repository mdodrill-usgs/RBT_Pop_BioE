###############################################################################
#                                                                      Mar 2018
#
#   Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * This script makes plots used in the publication
#  * Run "1_Calc_Metabolism_AGFD.R" first, then run,
#    '2_Pop_Expand_AGFD.R', then this script
#  * See '3_Plot_Pub.R'
#
#  To do:
#
###############################################################################
library(ggplot2)
library(ggthemes)
# library(gridExtra)
# library(gtable)
# library(grid)


windows(xpos = 25, record = T, width = 9 * 2, height = 5 * 2)

#-----------------------------------------------------------------------------#
# add the size groups to the data

# sz.groups = seq(78, 398, 50)
sz.groups = seq(75, 325, 50)

sz.key = data.frame(sz = c(1:6),
                    name = c("75-124", "125-174", "175-224",
                             "225-274", "275-324", ">325"))

tmp = findInterval(all2$MidSz, sz.groups)

all2$sz.group = factor(sz.key[match(tmp, sz.key[,1]),2], ordered = T, levels = sz.key[,2])

#-----------------------------------------------------------------------------#
# Make a theme to use

yard_theme = theme(axis.title.x = element_text(size = 14, vjust = -.1),
                   axis.title.y = element_text(size = 14, vjust = 1),
                   axis.text.x = element_text(size = 12, colour = "black"),
                   axis.text.y = element_text(size = 12, colour = "black"),
                   panel.background = element_rect(fill = "white"),
                   panel.grid.minor = element_line(colour = "white"),
                   panel.grid.major = element_line(colour = "white"),
                   panel.border = element_rect(colour = "black", fill = NA),
                   legend.text = element_text(size = 12),
                   legend.title = element_text(size = 12),
                   legend.title.align = .5,
                   legend.key = element_rect(fill = "white"))

#-----------------------------------------------------------------------------#
# AGFD Figure
# PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed
# at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry
# da-1.

dat = all2

pop.7 = group_by(dat, Date) %>%
  summarize(tot.PopDaCTotInvKgafdm = sum(PopDaCTotInvKgafdm))

az.1 = ggplot(pop.7, aes(x = Date, y = tot.PopDaCTotInvKgafdm)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "year", date_labels = "%Y") + 
  labs(title = "", y = "Total Population Level Consumption of \nInvertebrate Biomass (kg AFDM)", x = "") + 
  yard_theme +
  theme(panel.grid.major = element_line(colour = "gray90"),
        axis.line = element_line(color = 'black'),
        panel.border = element_blank())
# plot.margin = unit(c(1,1,0,1), "lines"),

az.1 

#-----------------------------------------------------------------------------#