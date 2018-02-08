###############################################################################
#                                                                      Feb 2018
#
#   Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * This script plots a bunch of model variables
#
#  To do:
#  * Finish plots for all of the variables
#  * Write a wrapper function for running the plots?
###############################################################################
library(ggplot2)
library(ggthemes)

windows(xpos = 25, record = T)

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


p = ggplot(N.all, aes(x = Date, y = N.tot)) +
    geom_bar(position = "stack", stat = "identity",
             aes(color = rev(sz.group), fill = rev(sz.group)))
p

#-----------------------------------------------------------------------------#
# Mean daily growth by size class

G.town = group_by(all2, Date, sz.group) %>%
  summarize(G.mean = mean(Growth))


p = ggplot(G.town, aes(x = Date, y = G.mean)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p

# p2 = p + theme_solarized(light = F)
# p2


#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Mean daily growth by size class
# library(gganimate)
# library(magick)
# 
# 
# G.town = group_by(all2, Date, sz.group) %>%
#   summarize(G.mean = mean(Growth))
# 
# G.town$year = substr(G.town$Date,1,4)
# 
# 
# p = ggplot(G.town, aes(x = Date, y = G.mean, frame = year)) +
#   geom_line(aes(color = sz.group, cumulative = TRUE), size = 1) +
#   scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
# p
# 
# p2 = p + theme_solarized(light = F)
# p2
# gganimate(p2, interval = 2, filname = "output.gif", ani.width = 1000, ani.height = 600)
# 
# 

#-----------------------------------------------------------------------------#
# Mean daily Cmin by size class

c.min = group_by(all2, Date, sz.group) %>%
        summarize(mean.c.min = mean(Cmin.kJ))


p = ggplot(c.min, aes(x = Date, y = mean.c.min)) +
    geom_line(aes(color = sz.group), size = 1) +
    scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p

# p2 = p + theme_solarized_2(light = F)
# p2

#-----------------------------------------------------------------------------#
# Total daily Cmin (all fish)

c.min.tot = group_by(all2, Date) %>%
  summarize(tot.c.min = sum(Cmin.kJ))


p = ggplot(c.min.tot, aes(x = Date, y = tot.c.min)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p

#-----------------------------------------------------------------------------#
# Total daily Cmin - Inverts (all fish)

c.min.inv = group_by(all2, Date) %>%
  summarize(tot.c.min.inv = sum(Cmin.Inv))


p = ggplot(c.min.inv, aes(x = Date, y = tot.c.min.inv)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p

#-----------------------------------------------------------------------------#
# Total daily Cmin - Plants (all fish)

c.min.pla = group_by(all2, Date) %>%
  summarize(tot.c.min.pla = sum(Cmin.Pla))


p = ggplot(c.min.pla, aes(x = Date, y = tot.c.min.pla)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p

#-----------------------------------------------------------------------------#
# Toal daily Cmin - Both Inverts & Plants

# Note the units are different between the tot and pla/inv
# c.min.2 = plyr::join_all(list(c.min.tot, c.min.pla, c.min.inv), by = "Date", type = "full")

c.min.pla$type = rep("Plants")
names(c.min.pla)[2] = "c.min" 
  
c.min.inv$type = rep("Inverts")
names(c.min.inv)[2] = "c.min" 
c.min.2 = rbind(c.min.pla, c.min.inv)


p = ggplot(c.min.2, aes(x = Date, y = c.min)) +
  geom_line(aes(color = type), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p

#-----------------------------------------------------------------------------#
# F


#-----------------------------------------------------------------------------#
# PopDadelGMJ is the amount of daily growth converted to units of energy (MJ)
# consumed.

pop.1 = group_by(all2, Date) %>%
  summarize(tot.PopDadelGMJ = sum(PopDadelGMJ))

p = ggplot(pop.1, aes(x = Date, y = tot.PopDadelGMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDadelGMJ is the amount of daily growth converted to units of energy (MJ) consumed",
       y = "PopDadelGMJ")
p + theme_base()


pop.1.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDadelGMJ = sum(PopDadelGMJ))

p = ggplot(pop.1.b, aes(x = Date, y = tot.PopDadelGMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGMJ is the amount of daily growth converted to units of energy (MJ) consumed",
       y = "PopDadelGMJ")
p + theme_base()


#-----------------------------------------------------------------------------#
# PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from
# invertebrates consumed at a population level (N of size-bin). Energy units are
# MJ.


pop.2 = group_by(all2, Date) %>%
  summarize(tot.PopDadelGInvMJ = sum(PopDadelGInvMJ))

p = ggplot(pop.2, aes(x = Date, y = tot.PopDadelGInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from invertebrates \n consumed at a population level (N of size-bin). Energy units are MJ",
       y = "PopDadelGInvMJ")
p + theme_base()


pop.2.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDadelGInvMJ = sum(PopDadelGInvMJ))

p = ggplot(pop.2.b, aes(x = Date, y = tot.PopDadelGInvMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDadelGInvMJ is the amount of daily energy ascribed to growth derived from invertebrates \n consumed at a population level (N of size-bin). Energy units are MJ",
       y = "PopDadelGInvMJ")
p + theme_base()


#-----------------------------------------------------------------------------#
# PopDadelGPlaMJ is the amount of daily energy ascribed to growth derived from
# plant matter consumed at a population level (N of size-bin). Energy units are
# MJ.

pop.3 = group_by(all2, Date) %>%
  summarize(tot.PopDadelGPlaMJ = sum(PopDadelGPlaMJ))

p = ggplot(pop.3, aes(x = Date, y = tot.PopDadelGPlaMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p


pop.3.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDadelGPlaMJ = sum(PopDadelGPlaMJ))

p = ggplot(pop.3.b, aes(x = Date, y = tot.PopDadelGPlaMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y")
p


#-----------------------------------------------------------------------------#
# PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of invertebrates) from an individual level that is scaled up
# to a population level (N of size-bin) and converted from KJ to MJ. Units are
# MJ consumed in Lees Ferry da-1.

pop.4 = group_by(all2, Date) %>%
  summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))

p = ggplot(pop.4, aes(x = Date, y = tot.PopDaCMinInvMJ)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on the \n consumption of invertebrates) from an individual level that is scaled up
        to a population level (N of size-bin) and converted from KJ to MJ. Units are \n MJ consumed in Lees Ferry da-1.",
       y = "PopDaCMinInv_MJ")
p + theme_base()


pop.4.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))

p = ggplot(pop.4.b, aes(x = Date, y = tot.PopDaCMinInvMJ)) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  labs(title = "PopDaCMinInv_MJ expands Cmin (energy consumed for maintenance levels based on the \n consumption of invertebrates) from an individual level that is scaled up
        to a population level (N of size-bin) and converted from KJ to MJ. Units are \n MJ consumed in Lees Ferry da-1.",
       y = "PopDaCMinInv_MJ")
p + theme_base()


pop.4.c = group_by(all2, Date, MidSz) %>%
  summarize(tot.PopDaCMinInvMJ = sum(PopDaCMinInvMJ))
pop.4.c$MidSz = as.factor(pop.4.c$MidSz)


all2$MidSz.2 = as.factor(all2$MidSz)
p = ggplot(all2, aes(x = Date, y = PopDaCMinInvMJ)) +
  geom_line(aes(color = MidSz.2), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
  facet_wrap(~ sz.group)
p



#-----------------------------------------------------------------------------#
# PopDaCMinPlaMJ expands Cmin (energy consumed for maintenance levels based on
# the consumption of plant matter) from an individual level that is scaled up to
# a population level (N of size-bin) and converted from KJ to MJ. Units are MJ
# consumed in Lees Ferry da-1.

#-----------------------------------------------------------------------------#
# PopDaCTotInvMJ  is the total amount of daily energy consumed at a population
# level (N of size-bin) that is derived from solely from invertebrates. Units
# are MJ consumed in Lees Ferry da-1.

#-----------------------------------------------------------------------------#
# PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed
# at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry
# da-1.

pop.7 = group_by(all2, Date) %>%
  summarize(tot.PopDaCTotInvKgafdm = sum(PopDaCTotInvKgafdm))

p = ggplot(pop.7, aes(x = Date, y = tot.PopDaCTotInvKgafdm)) +
  geom_line(size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed \n at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry da-1.",
       y = "PopDaCTotInvKgafdm")


p + theme_base()


pop.7.b = group_by(all2, Date, sz.group) %>%
  summarize(tot.PopDaCTotInvKgafdm = sum(PopDaCTotInvKgafdm))

p = ggplot(pop.7.b, aes(x = Date, y = tot.PopDaCTotInvKgafdm)) +
  # geom_line(size = 1) +
  geom_line(aes(color = sz.group), size = 1) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") + 
  labs(title = "PopDaCTotInvKgafdm is the total amount of daily invertebrate biomass consumed \n at a population level (N of size-bin). Biomass units are Kg AFDM in Lees Ferry da-1.",
       y = "PopDaCTotInvKgafdm")


p + theme_base()




#-----------------------------------------------------------------------------#
# PopDaCTotInvgafdmm2 is the average amount of daily invertebrate biomass
# consumed per m^2 . Biomass units are g AFDM m-2 da-1 in Lees Ferry









