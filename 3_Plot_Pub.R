###############################################################################
#                                                                      Feb 2018
#
#   Population Level Bioenergetics Model of RBT @ Lees Ferry
#
#  Notes:
#  * This script makes plots used in the publication
#  * Run "1_Calc_Metabolism.R" first, then run, '2_Pop_Expand.R', then this script
#
#  To do:
#
###############################################################################
library(ggplot2)
library(ggthemes)
# library(gridExtra)
library(gtable)
library(grid)


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
# Fig 2 - Fig. 2. 2 -axis’s, Size stratified population estimate (50 mmm FL
# bins) and biomass estimate pooled across size strata. X axis time (2012-2016)
# (combination of bar histogram and line graph). (Color)



N.all = group_by(all2, Date, sz.group) %>%
  summarize(N.tot = sum(N))

all2$Pop.Mass.g = all2$N * all2$fish.mass


p.1 = ggplot(N.all, aes(x = Date, y = N.tot/100000)) +
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity",
           # aes(color = rev(sz.group), fill = rev(sz.group))) +
           aes(color = sz.group, fill = sz.group)) +
  labs(y = "Abundance (100,000's)", x = "", color = "Size Class", fill = "Size Class") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b \n %Y") + #,
               # limits = as.Date(c('2012-01-01','2017-01-01'))) +
  theme_base()
# p.1

p1 = p.1 + theme(legend.position = c(.8,.8))

p1

#--------------------------------------
mass.all = group_by(all2, Date) %>%
  summarise(mass.tot = sum(Pop.Mass.g))


# p.2 = ggplot(mass.all, aes(x = Date, y = mass.tot/1000/1000)) +
p.2 = ggplot(mass.all, aes(x = Date, y = mass.tot * (1e-6))) +
  geom_line(size = 1) + 
  labs(y = "Biomass (metric ton ww)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
               # limits = as.Date(c('2012-01-01','2017-01-01'))) +
  theme_base()


p2 = p.2 
# p2 = p2 + theme(axis.text.y = element_text(hjust = 1))

# p2 = p.2 + theme(axis.text.y = element_text(hjust = 1),
#                 axis.text.x = element_text(margin = margin(t = 30, r = 0, b = 0, l = 0)),
#                 # axis.ticks.x = element_line(colour = "white"))
#                 axis.ticks.x = element_line(colour = "red"))
                


p2
#--------------------------------------


# source function 'ggplot_dual_axis_2' from 'Working_Second_Axis.R'

ggplot_dual_axis_2(p1, p2)


# ggplot_dual_axis_3(p1, p2)




#--------------------------------------
mass.all = group_by(all2, Date) %>%
  summarise(mass.tot = sum(Pop.Mass.g))


p2 = ggplot(all2, aes(x = Date, y = Pop.Mass.g)) +
  geom_line(size = 1, aes(color = as.factor(MidSz))) + 
  labs(y = "Biomass") +
  facet_wrap(~sz.group, scales = "free") +
  theme_base()


p2 = p2 + theme(axis.text.y = element_text(hjust = -1))

p2
#--------------------------------------




#-----------------------------------------------------------------------------#
# Fig 3 - Fig. 3. Panel (A & B), Line graphs (individual-daily not expanded to a
# population level); Panel A: depicts size stratified daily growth (MJ/da),
# Panel B: depicts size stratified daily basal metabolism (Cmin, MJ). (Color)


# ask Yard --> does he want the mean for the size group? (what's below)


G.town = group_by(all2, Date, sz.group) %>%
  summarize(G.mean = mean(Growth),
            Cmin.mean = mean(Cmin.Inv))

p.3.a = ggplot(G.town, aes(x = Date, y = G.mean)) +
        geom_line(aes(color = sz.group), size = 1) +
        scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
        theme_base()

p.3.a


p.3.b = ggplot(G.town, aes(x = Date, y = Cmin.mean)) +
        geom_line(aes(color = sz.group), size = 1) +
        scale_x_date(date_breaks = "3 month", date_labels = "%b %Y") +
        theme_base()

p.3.b


grid.arrange(p.3.a, p.3.b)

#-----------------------------------------------------------------------------#











