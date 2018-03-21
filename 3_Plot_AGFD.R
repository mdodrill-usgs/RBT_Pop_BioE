###############################################################################
#                                                                      Feb 2018
#
#   Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * This script plots a bunch of model variables
#  * Run "1_Calc_Metabolism.R" first, then run, '2_Pop_Expand.R', then this script
#
#  To do:
#  * Write a wrapper function for running the plots?
#  * Build a MarkDown doc?
#
###############################################################################
library(ggplot2)
library(ggthemes)

windows(xpos = 25, record = T, width = 9*2, height = 5*2)

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
# Daily abundance by size class

N.all = group_by(all2, Date, sz.group) %>%
  summarize(N.tot = sum(N))

p.1 = ggplot(N.all, aes(x = Date, y = N.tot)) +
  geom_bar(position = "stack", stat = "identity",
           aes(color = rev(sz.group), fill = rev(sz.group))) + 
  theme_base()
p.1

#-----------------------------------------------------------------------------#
# Mean daily growth by size class

G.town = group_by(all2, Date, sz.group) %>%
  summarize(G.mean = mean(Growth))

p.2 = ggplot(G.town, aes(x = Date, y = G.mean)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  theme_base()


p.2

#-----------------------------------------------------------------------------#
# Mean daily Cmin by size class

c.min = group_by(all2, Date, sz.group) %>%
  summarize(mean.c.min = mean(Cmin.kJ))

p.3 = ggplot(c.min, aes(x = Date, y = mean.c.min)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  theme_base()
p.3

#-----------------------------------------------------------------------------#
# Total daily Cmin (all fish)

c.min.tot = group_by(all2, Date) %>%
  summarize(tot.c.min = sum(Cmin.kJ))

p.4 = ggplot(c.min.tot, aes(x = Date, y = tot.c.min)) +
  geom_line(size = 1) +
  # scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_base()
p.4

#-----------------------------------------------------------------------------#
# Total daily Cmin - Inverts (all fish)

c.min.inv = group_by(all2, Date) %>%
  summarize(tot.c.min.inv = sum(Cmin.Inv))

p.5 = ggplot(c.min.inv, aes(x = Date, y = tot.c.min.inv)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  theme_base()
p.5

#-----------------------------------------------------------------------------#
# Total daily Cmin - Plants (all fish)

c.min.pla = group_by(all2, Date) %>%
  summarize(tot.c.min.pla = sum(Cmin.Pla))


p.6 = ggplot(c.min.pla, aes(x = Date, y = tot.c.min.pla)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  theme_base()
p.6

#-----------------------------------------------------------------------------#
# Toal daily Cmin - Both Inverts & Plants

c.min.pla$type = rep("Plants")
names(c.min.pla)[2] = "c.min" 

c.min.inv$type = rep("Inverts")
names(c.min.inv)[2] = "c.min" 
c.min.2 = rbind(c.min.pla, c.min.inv)

p.7 = ggplot(c.min.2, aes(x = Date, y = c.min)) +
  geom_line(aes(color = type), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  theme_base()
p.7

#-----------------------------------------------------------------------------#
# Fu.p

#-----------------------------------------------------------------------------#
# Fu.i

#-----------------------------------------------------------------------------#
# Fu.tot

#-----------------------------------------------------------------------------#
# PopDadelGMJ is the amount of daily growth converted to units of energy (MJ)
# consumed.

pop.1 = group_by(all2, Date) %>%
  summarize(tot.PopDadelGMJ = sum(PopDadelGMJ))

p.10 = ggplot(pop.1, aes(x = Date, y = tot.PopDadelGMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDadelGMJ is the amount of daily growth converted to units of energy (MJ) consumed",
       y = "PopDadelGMJ") + 
  theme_base()
p.10 


pop.1.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDadelGMJ = sum(PopDadelGMJ))

p.11 = ggplot(pop.1.b, aes(x = Date, y = tot.PopDadelGMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGMJ is the amount of daily growth converted to units of energy (MJ) consumed",
       y = "PopDadelGMJ") + 
  theme_base()
p.11 

#-----------------------------------------------------------------------------#
# PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from
# invertebrates consumed at a population level (N of size-bin). Energy units are
# MJ.


pop.2 = group_by(all2, Date) %>%
  summarize(tot.PopDadelGInvMJ = sum(PopDadelGInvMJ))

p.12 = ggplot(pop.2, aes(x = Date, y = tot.PopDadelGInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from invertebrates \n consumed at a population level (N of size-bin). Energy units are MJ",
       y = "PopDadelGInvMJ") + 
  theme_base()
p.12


pop.2.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDadelGInvMJ = sum(PopDadelGInvMJ))

p.13 = ggplot(pop.2.b, aes(x = Date, y = tot.PopDadelGInvMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from invertebrates \n consumed at a population level (N of size-bin). Energy units are MJ",
       y = "PopDadelGInvMJ") + 
  theme_base()
p.13 



p.13.b = ggplot(all2, aes(x = Date, y = PopDadelGInvMJ, group = as.factor(MidSz))) +
  geom_line(size = 1, aes(color = PopDadelGInvMJ)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "",
       y = "PopDadelGInvMJ") + 
  facet_wrap(~ sz.group) +
  theme_base()
p.13.b 

#-----------------------------------------------------------------------------#
# PopDadelGPlaMJ is the amount of daily energy ascribed to growth derived from
# plant matter consumed at a population level (N of size-bin). Energy units are
# MJ.

pop.3 = group_by(all2, Date) %>%
  summarize(tot.PopDadelGPlaMJ = sum(PopDadelGPlaMJ))

p.14 = ggplot(pop.3, aes(x = Date, y = tot.PopDadelGPlaMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGPlaMJ is the amount of daily energy ascribed to growth derived from plant matter \n consumed at a population level (N of size-bin). Energy units are MJ",
       y = "PopDadelGPlaMJ") + 
  theme_base()
p.14


pop.3.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDadelGPlaMJ = sum(PopDadelGPlaMJ))

p.15 = ggplot(pop.3.b, aes(x = Date, y = tot.PopDadelGPlaMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGPlaMJ is the amount of daily energy ascribed to growth derived from plant matter \n consumed at a population level (N of size-bin). Energy units are MJ",
       y = "PopDadelGPlaMJ") + 
  theme_base()
p.15

#-----------------------------------------------------------------------------#
# PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of invertebrates) from an individual level that is scaled up
# to a population level (N of size-bin) and converted from KJ to MJ. Units are
# MJ consumed in Lees Ferry da-1.

pop.4 = group_by(all2, Date) %>%
  summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))

p.16 = ggplot(pop.4, aes(x = Date, y = tot.PopDaCMinInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on the \n consumption of invertebrates) from an individual level that is scaled up
       to a population level (N of size-bin) and converted from KJ to MJ. Units are \n MJ consumed in Lees Ferry da-1.",
       y = "PopDaCMinInv_MJ") + 
  theme_base()
p.16 


pop.4.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))

p.17 = ggplot(pop.4.b, aes(x = Date, y = tot.PopDaCMinInvMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on the \n consumption of invertebrates) from an individual level that is scaled up
       to a population level (N of size-bin) and converted from KJ to MJ. Units are \n MJ consumed in Lees Ferry da-1.",
       y = "PopDaCMinInv_MJ") + 
  theme_base()
p.17 


# by individual mid size
all2$MidSz.2 = as.factor(all2$MidSz) 

p.17.b = ggplot(all2, aes(x = Date, y = PopDaCMinInvMJ)) +
  geom_line(aes(color = MidSz.2), size = 1) +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %Y") +
  facet_wrap(~ sz.group) + 
  theme_base() + theme(axis.text.x = element_text(angle = -45, vjust = .5))

p.17.b

#--------------------------------------
# playing around with ridgeline plots
# library(ggridges)
# library(gridExtra)
# 
# p.list = list()
# 
# for(i in 1:6){
#   sub = all2[which(as.character(all2$sz.group) == sz.key[i,2]),]
# 
#   sub$Date2 = as.numeric(sub$Date)
#   sub$MidSz.3 = as.factor(as.numeric(sub$MidSz.2))
# 
#   p.17.c = ggplot(sub, aes(x = Date2, y = MidSz.3, height = PopDaCMinInvMJ, group = MidSz.3)) +
#     geom_density_ridges(stat = "identity", scale = 2, aes(fill = MidSz.3), alpha = .5) +
#     # geom_density_ridges(stat = "identity", scale = 1, aes(fill = sz.group), alpha = 1) +
#     labs(y = "", x = "", title = "")# +
#   # facet_wrap(~ sz.group)
#   # p.17.c + theme_base() + theme(axis.text = element_blank())
# 
#   p.list[[i]] = p.17.c + theme_base() + theme(axis.text = element_blank())
# }
# 
# 
# grid.arrange(p.list[[1]], p.list[[2]], p.list[[3]], p.list[[4]], p.list[[5]], p.list[[6]], nrow = 2)
#-----------------------------------------------------------------------------#
# PopDaCMinPlaMJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of plant matter) from an individual level that is scaled up to
# a population level (N of size-bin) and converted from KJ to MJ. Units are MJ
# consumed in Lees Ferry da-1.

pop.5 = group_by(all2, Date) %>%
  summarize(tot.PopDaCMinPlaMJ = sum(PopDaCMinPlaMJ))

p.18 = ggplot(pop.5, aes(x = Date, y = tot.PopDaCMinPlaMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinPlaMJ expands Cmin (energy consumed for maintenance levels based on
       the consumption of plant matter) from an individual level that is scaled up to
       a population level (N of size-bin) and converted from KJ to MJ. Units are MJ
       consumed in Lees Ferry da-1.",
       y = "PopDaCMinPlaMJ") + 
  theme_base()
p.18


pop.5.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCMinPlaMJ = sum(PopDaCMinPlaMJ))

p.19 = ggplot(pop.5.b, aes(x = Date, y = tot.PopDaCMinPlaMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinPlaMJ expands Cmin (energy consumed for maintenance levels based on
       the consumption of plant matter) from an individual level that is scaled up to
       a population level (N of size-bin) and converted from KJ to MJ. Units are MJ
       consumed in Lees Ferry da-1.",
       y = "PopDaCMinPlaMJ") + 
  theme_base()
p.19 

#-----------------------------------------------------------------------------#
# PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
# level (N of size-bin) that is derived from solely from invertebrates. Units
# are MJ consumed in Lees Ferry da-1.

pop.6 = group_by(all2, Date) %>%
  summarize(tot.PopDaCTotInvMJ = sum(PopDaCTotInvMJ))

p.18 = ggplot(pop.6, aes(x = Date, y = tot.PopDaCTotInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
       level (N of size-bin) that is derived from solely from invertebrates. Units
       are MJ consumed in Lees Ferry da-1.",
       y = "PopDaCTotInvMJ") + 
  theme_base()
p.18


pop.6.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCTotInvMJ = sum(PopDaCTotInvMJ))

p.19 = ggplot(pop.6.b, aes(x = Date, y = tot.PopDaCTotInvMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
       level (N of size-bin) that is derived from solely from invertebrates. Units
       are MJ consumed in Lees Ferry da-1.",
       y = "PopDaCTotInvMJ") + 
  theme_base()
p.19 

#-----------------------------------------------------------------------------#
# PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed
# at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry
# da-1.

pop.7 = group_by(all2, Date) %>%
  summarize(tot.PopDaCTotInvKgafdm = sum(PopDaCTotInvKgafdm))

p.20 = ggplot(pop.7, aes(x = Date, y = tot.PopDaCTotInvKgafdm)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed \n at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry da-1.",
       y = "PopDaCTotInvKgafdm") + theme_base()

p.20 

pop.7.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCTotInvKgafdm = sum(PopDaCTotInvKgafdm))

p.21 = ggplot(pop.7.b, aes(x = Date, y = tot.PopDaCTotInvKgafdm)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed \n at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry da-1.",
       y = "PopDaCTotInvKgafdm") + theme_base()

p.21 

#--------------------------------------
# # playing around with ridgeline plots
# library(ggridges)
# 
# pop.7.b$Date2 = as.numeric(pop.7.b$Date)
# pop.7.b$gp = as.factor(as.numeric(pop.7.b$sz.group))
# 
# p.21.b = ggplot(pop.7.b, aes(x = Date2, y = gp, height = tot.PopDaCTotInvKgafdm, group = gp)) +
#   geom_density_ridges(stat = "identity", scale = 2, aes(fill = sz.group), alpha = .5) +
#   # geom_density_ridges(stat = "identity", scale = 1, aes(fill = sz.group), alpha = 1) +
#   labs(y = "", x = "", title = "")
# p.21.b + theme_base() + theme(axis.text = element_blank())


#-----------------------------------------------------------------------------#
# PopDaCTotInvgafdmm2 is the average amount of daily invertebrate biomass
# consumed per m^2 . Biomass units are g AFDM m-2 da-1 in Lees Ferry

pop.8 = group_by(all2, Date) %>%
  summarize(tot.PopDaCTotInvgafdmm2 = sum(PopDaCTotInvgafdmm2))

p.20 = ggplot(pop.8, aes(x = Date, y = tot.PopDaCTotInvgafdmm2)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDaCTotInvgafdmm2 is the average amount of daily invertebrate biomass
  consumed per m^2 . Biomass units are g AFDM m-2 da-1 in Lees Ferry",
       y = "PopDaCTotInvgafdmm2") + theme_base()

p.20 

pop.8.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCTotInvgafdmm2 = sum(PopDaCTotInvgafdmm2))

p.21 = ggplot(pop.8.b, aes(x = Date, y = tot.PopDaCTotInvgafdmm2)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDaCTotInvgafdmm2 is the average amount of daily invertebrate biomass
  consumed per m^2 . Biomass units are g AFDM m-2 da-1 in Lees Ferry",
       y = "PopDaCTotInvgafdmm2") + theme_base()

p.21 
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of invertebrates) from an individual level that is scaled up
# to a population level (N of size-bin) and converted from KJ to MJ. Units are
# MJ consumed in Lees Ferry da-1.

pop.4 = group_by(all2, Date) %>%
  summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))

p.16 = ggplot(pop.4, aes(x = Date, y = tot.PopDaCMinInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on the \n consumption of invertebrates) from an individual level that is scaled up
       to a population level (N of size-bin) and converted from KJ to MJ. Units are \n MJ consumed in Lees Ferry da-1.",
       y = "PopDaCMinInv_MJ") + 
  theme_base()
p.16 

#--------------------------------
# PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
# level (N of size-bin) that is derived from solely from invertebrates. Units
# are MJ consumed in Lees Ferry da-1.

pop.6 = group_by(all2, Date) %>%
  summarize(tot.PopDaCTotInvMJ = sum(PopDaCTotInvMJ))

p.18 = ggplot(pop.6, aes(x = Date, y = tot.PopDaCTotInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
       level (N of size-bin) that is derived from solely from invertebrates. Units
       are MJ consumed in Lees Ferry da-1.",
       y = "PopDaCTotInvMJ") + 
  theme_base()
p.18
#--------------------------------

dat.new.1 = merge(pop.4, pop.6, by = c("Date"))
dat.new.2 = merge(dat.new.1, pop.2, by = c("Date"))


dat.in = melt(dat.new.2, id.vars = c("Date"))


G1 = ggplot(dat.in, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(y = "Population Level Daily Consumption in MJ") + 
  theme_base()


G1 + theme(legend.position = c(.2,.9))




#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# plot comparing the Cmin, del_G, and total consumption of inverts in units of
# Kg afdm


pop.pop = group_by(all2, Date) %>%
  summarize(tot.PopDaCMinInvKgafdm = sum(PopDaCMinInvKgafdm),
            tot.PopDadelGInvKgafdm = sum(PopDadelGInvKgafdm),
            tot.PopDaCTotInvKgafdm = sum(PopDaCTotInvKgafdm))

dat.in = melt(pop.pop, id.vars = c("Date"))

dat.in$date.2 = substr(dat.in$Date, 3, 4)

G1 = ggplot(dat.in, aes(x = Date, y = value)) +
  geom_line(aes(color = variable), size = 1) +
  geom_text(aes(x = Date, y = value, label = date.2)) +  
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(y = "Population Level Daily Consumption in Kg AFDM of Inverts", 
       title = "DD NotoAGF") + 
  theme_base()


G1 + theme(legend.position = c(.2,.9))


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
