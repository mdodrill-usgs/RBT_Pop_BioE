###############################################################################
#                                                                      Mar 2018
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
library(gridExtra)
library(gtable)
library(grid)
library(RColorBrewer)


windows(xpos = 25, record = T, width = 9 * 2, height = 5 * 2)


#-----------------------------------------------------------------------------#
# add the size groups to the data

# sz.groups = seq(78, 398, 50)
sz.groups = seq(75, 325, 50)

sz.key = data.frame(sz = c(1:6),
                    name = c("75-124", "125-174", "175-224",
                             "225-274", "275-324", ">325"))

tmp = findInterval(dat$MidSz, sz.groups)

dat$sz.group = factor(sz.key[match(tmp, sz.key[,1]),2], ordered = T, levels = sz.key[,2])

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
                   # strip.background = element_rect(fill = "white"),
                   # strip.background = element_blank(),
                   # strip.text = element_text(size = 14, vjust = 1),
                   legend.text = element_text(size = 12),
                   legend.title = element_text(size = 12),
                   legend.title.align = .5,
                   legend.key = element_rect(fill = "white"))

#-----------------------------------------------------------------------------#

# Keep working on the colors....



# my.cols <- brewer.pal(5, "Blues")
# my.cols
# 
# brewer.pal(4, "GnBu")  # the colors I used above....
# #  "#F0F9E8" "#BAE4BC" "#7BCCC4" "#2B8CBE"
# display.brewer.pal(4, "GnBu")
# 
# brewer.pal.info
# 
# display.brewer.all()
# 
# display.brewer.pal(11, "Spectral")
# 

# cols = brewer.pal(11, "RdYlBu")[c(2,4,5,6,9,10)]
cols.1 = rev(brewer.pal(9, "YlOrRd")[c(3, 4, 5)])

# cols.2 = brewer.pal(11, "RdYlBu")[c(2,4,5,6,9,10)]
cols.2 = brewer.pal(11, "RdYlBu")[c(2,9,10)]

cols = c(cols.2[1], cols.1[1:3], cols.2[2:3])

#-----------------------------------------------------------------------------#
# Fig 2 - Fig. 2. 2 -axis’s, Size stratified population estimate (50 mmm FL
# bins) and biomass estimate pooled across size strata. X axis time (2012-2016)
# (combination of bar histogram and line graph). (Color)

N.all = group_by(dat, Date, sz.group) %>%
  summarize(N.tot = sum(N))

dat$Pop.Mass.g = dat$N * dat$fish.mass


p.1 = ggplot(N.all, aes(x = Date, y = N.tot/100000)) +
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity",
           # aes(color = rev(sz.group), fill = rev(sz.group))) +
           aes(color = sz.group, fill = sz.group)) +
  scale_fill_manual(values = cols) +
  scale_color_manual(values = cols) +
  labs(y = "Abundance (100,000's)", x = "", color = "Size Class", fill = "Size Class") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b \n %Y") + #,
               # limits = as.Date(c('2012-01-01','2017-01-01'))) +
  theme_base()
# p.1

p1 = p.1 + theme(legend.position = c(.8,.8))

p1

#--------------------------------------
mass.all = group_by(dat, Date) %>%
  summarise(mass.tot = sum(Pop.Mass.g))


# !!! Make sure the biomass is correct & the second y-axis is correct !!!
# !!! Make sure the biomass is correct & the second y-axis is correct !!!
# !!! Make sure the biomass is correct & the second y-axis is correct !!!
# !!! Make sure the biomass is correct & the second y-axis is correct !!!


# p.2 = ggplot(mass.all, aes(x = Date, y = mass.tot/1000/1000)) +
p.2 = ggplot(mass.all, aes(x = Date, y = mass.tot * (1e-6))) +
  geom_line(size = 1) + 
  labs(y = "Biomass (metric tons ww)") +
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
mass.all = group_by(dat, Date) %>%
  summarise(mass.tot = sum(Pop.Mass.g))


p2 = ggplot(dat, aes(x = Date, y = Pop.Mass.g)) +
  geom_line(size = 1, aes(color = as.factor(MidSz))) + 
  labs(y = "Biomass") +
  facet_wrap(~sz.group, scales = "free") +
  theme_base()


p2 = p2 + theme(axis.text.y = element_text(hjust = -1))

p2

#-----------------------------------------------------------------------------#
# Fig 3 - Fig. 3. Panel (A & B), Line graphs (individual-daily not expanded to a
# population level); Panel A: depicts size stratified daily growth (MJ/da),
# Panel B: depicts size stratified daily basal metabolism (Cmin, MJ). (Color)

#--------------------------------------
# In energy units

# dat$DadelGMJ = dat$Growth  * .001 * KgwwtoMJ 
# 
# # DadelGInvMJ = DadelGMJ * dat$pDi[1] / 0.783     
# 
# 
dat$Cmin.MJ = dat$Cmin.kJ * .001  # convert kilo Joules to mega Joules
# 
# G.town = group_by(dat, Date, sz.group) %>%
#   summarize(G.mean = mean(DadelGMJ),
#             Cmin.mean = mean(Cmin.MJ))
# 
# p.3.a = ggplot(G.town, aes(x = Date, y = G.mean)) +
#         geom_line(aes(color = sz.group), size = 1) +
#         scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
#         labs(x = "", y = "Individual Mean Daily \n Growth Rates (MJ)",
#              color = "Size Class") +
#         yard_theme +
#         theme(legend.position = c(.5, 1),
#               axis.text.x = element_blank(),
#               plot.margin = unit(c(1,1,0,1), "lines"),
#               axis.line = element_line(color = 'black'),
#               panel.border = element_blank(),
#               panel.grid.major = element_line(colour = "gray90")) + 
#         guides(colour = guide_legend(nrow = 1, title.position = "left"))   
# # p.3.a
#--------------------------------------

G.town = group_by(dat, Date, sz.group) %>%
  summarize(G.mean = mean(Growth),
            Cmin.mean = mean(Cmin.MJ))

p.3.a = ggplot(G.town, aes(x = Date, y = G.mean)) +
        geom_line(aes(color = sz.group), size = 1) +
        scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
        labs(x = "", y = "Individual Mean Daily \n Growth Rates (g)",
             color = "Size Class") +
        scale_color_manual(values = cols) +
        yard_theme +
        theme(legend.position = c(.5, 1),
              axis.text.x = element_blank(),
              plot.margin = unit(c(1,1,0,1), "lines"),
              axis.line = element_line(color = 'black'),
              panel.border = element_blank(),
              panel.grid.major = element_line(colour = "gray90")) +
        guides(colour = guide_legend(nrow = 1, title.position = "left"))
# p.3.a


p.3.b = ggplot(G.town, aes(x = Date, y = Cmin.mean)) +
        geom_line(aes(color = sz.group), size = 1) +
        scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
        labs(x = "", y = "Individual Mean Daily \n Minimum Consumption (MJ)",
             color = "Size Class") +
       scale_color_manual(values = cols) +
        yard_theme +
        theme(legend.position = "none",
              plot.margin = unit(c(0,1,1,1), "lines"),
              axis.line = element_line(color = 'black'),
              panel.border = element_blank(),
              panel.grid.major = element_line(colour = "gray90")) #+ 
        # guides(colour = guide_legend(nrow = 1, title.position = "left"))

# p.3.b


G1 <- arrangeGrob(p.3.a,
                  top = textGrob("A", x = unit(.95, "npc"),
                                 y = unit(.95, "npc"),
                                 just = c("left", "top"),
                                 gp = gpar(col = "black", fontsize = 18)))

G2 <- arrangeGrob(p.3.b,
                  top = textGrob("B", x = unit(.95, "npc"),
                                 y = unit(.95, "npc"),
                                 just = c("left", "top"),
                                 gp = gpar(col = "black", fontsize = 18)))



# grid.arrange(p.3.a, p.3.b)
grid.arrange(G1, G2)

#-----------------------------------------------------------------------------#
# Fig. 4. Single axis, Total daily consumption (at a population level)
# stratified by size (50 mmm FL bins). (Color)

# Add temperature panel
# u.date = unique(dat$Date)
# temps = dat[which(!duplicated(dat$Date)), c("Date", "T")]

temps = data.frame(Date = dat$Date, obs.T = dat$T, avg.T = dat.2$T) %>%
      filter(!duplicated(Date))

# Color ramp of observed temps
# f.4.a = ggplot(temps, aes(x = Date, y = obs.T)) +
#         geom_line(size = 1, aes(color = obs.T)) +
#         scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
#         # scale_color_gradientn(colours = rev(brewer.pal(3, "Spectral"))) 
#         # scale_color_gradient(low = "blue", high = "red")
#         scale_colour_gradient2(low = "blue", mid = "yellow",
#                                high = "red", midpoint = 11, space = "Lab") +
#         labs(x = "", y = expression("Mean Daily Temperature "*~degree*C),
#              color = "Size Class") +
#         yard_theme +
#         theme(legend.position = "none",    
#               plot.margin = unit(c(0,1,1,1), "lines"),
#               axis.line = element_line(color = 'black'),
#               panel.border = element_blank(),
#               panel.grid.major = element_line(colour = "gray90")) 
# f.4.a

# observed and avg
temps.2 = melt(temps, id.vars = "Date")

f.4.a = ggplot(temps.2, aes(x = Date, y = value)) +
        geom_line(size = 1, aes(color = variable)) +
        scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
        scale_color_manual(values = c("black", "gray")) +
        labs(x = "", y = expression("Mean Daily Temperature "*~degree*C),
             color = "Temperature Regime") +
        yard_theme +
        theme(legend.position = c(.1,.9),
              plot.margin = unit(c(1,1,1,1), "lines"),
              axis.line = element_line(color = 'black'),
              panel.border = element_blank(),
              panel.grid.major = element_line(colour = "gray90"))
f.4.a


  
pop.6.b = group_by(dat, Date, sz.group) %>%
  # summarize(tot.PopDaCTotInvMJ = sum(PopDaCTotInvMJ))
  summarize(tot.PopDaCTotInvKg = sum(PopDaCTotInvKg))

# f.4.b = ggplot(pop.6.b, aes(x = Date, y = tot.PopDaCTotInvMJ)) +
f.4.b = ggplot(pop.6.b, aes(x = Date, y = tot.PopDaCTotInvKg)) +
        geom_line(aes(color = sz.group), size = 1) +
        scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
        # labs(x = "", y = "Population Total Daily Consumption (MJ)",
        labs(x = "", y = "Population Total Daily Consumption (Kg)",
             color = "Size Class") +
        scale_color_manual(values = cols) +
        yard_theme +
        theme(legend.position = c(.5, 1),   
              plot.margin = unit(c(1,1,1,1), "lines"),
              axis.line = element_line(color = 'black'),
              panel.border = element_blank(),
              panel.grid.major = element_line(colour = "gray90")) +
        guides(colour = guide_legend(nrow = 1, title.position = "left"))   
f.4.b


G1.4 <- arrangeGrob(f.4.a,
                  top = textGrob("A", x = unit(.95, "npc"),
                                 y = unit(.95, "npc"),
                                 just = c("left", "top"),
                                 gp = gpar(col = "black", fontsize = 18)))

G2.4 <- arrangeGrob(f.4.b,
                  top = textGrob("B", x = unit(.95, "npc"),
                                 y = unit(.95, "npc"),
                                 just = c("left", "top"),
                                 gp = gpar(col = "black", fontsize = 18)))



# grid.arrange(p.3.a, p.3.b)
grid.arrange(G1.4, G2.4)


#-----------------------------------------------------------------------------#
#Fig. 5. 2 -axis’s, Total drift estimates (3 taxa) (trip dates) and annual diet
#composition showing bar histogram for gammarids, chironomids, and simuliids
#(combination of bar histogram and line graph). This figure will likely be
#tricky (Black/white)



#-----------------------------------------------------------------------------#
# run the model with the observed and avg. temps (dat.1 & dat.2)

s.1 = dat.1[,c("MidSz", "Date", "PopDaCTotInvKg")]
s.1$Temp = "Observed"


s.2 = dat.2[,c("MidSz", "Date", "PopDaCTotInvKg")]
s.2$Temp = "Average"


s.3 = rbind(s.1, s.2)


pop.10 = group_by(s.3, Date, Temp) %>%
         summarize(tot.PopDaCTotInvKg = sum(PopDaCTotInvKg))


# pop.10 = group_by(dat, Date) %>%
#   summarize(tot.PopDadelGKg = sum(PopDadelGKg),
#             tot.PopDaCMinInvKg = sum(PopDaCMinInvKg),
#             tot.PopDaCTotInvKg = sum(PopDaCTotInvKg))
# 
# pop.in = melt(pop.10, id.vars = "Date")


f.4.b = ggplot(pop.10, aes(x = Date, y = tot.PopDaCTotInvKg)) +
  geom_line(size = 1, aes(color = Temp)) +
  scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
  labs(x = "", y = "Population Total C (Kg)",
       color = "Temperature Regime") +
  # scale_color_manual(values = cols) +
  yard_theme #+
  # theme(legend.position = c(.5, 1),   
  #       plot.margin = unit(c(0,1,1,1), "lines"),
  #       axis.line = element_line(color = 'black'),
  #       panel.border = element_blank(),
  #       panel.grid.major = element_line(colour = "gray90")) +
  # guides(colour = guide_legend(nrow = 1, title.position = "left"))   
f.4.b



#-----------------------------------------------------------------------------#
# run the model with the observed and avg. temps (dat.1 & dat.2)
# see 'Working_Shade_Plot'

avg = group_by(dat.2, Date) %>%
  summarize(tot.PopDaCTotInvKg = sum(PopDaCTotInvKg),
            tot.PopDaCMinInvKg = sum(PopDaCMinInvKg))

pop.1 = group_by(dat.1, Date) %>%
  summarize(tot.PopDaCTotInvKg = sum(PopDaCTotInvKg),
            tot.PopDaCMinInvKg = sum(PopDaCMinInvKg))

#--------------------------------------
pop.1$min = ifelse(pop.1$tot.PopDaCTotInvKg <= pop.1$tot.PopDaCMinInvKg, pop.1$tot.PopDaCTotInvKg, pop.1$tot.PopDaCMinInvKg)
pop.1$max = ifelse(pop.1$tot.PopDaCTotInvKg > pop.1$tot.PopDaCMinInvKg, pop.1$tot.PopDaCTotInvKg, pop.1$tot.PopDaCMinInvKg)

pop.1$shade = ifelse(pop.1$tot.PopDaCTotInvKg < pop.1$tot.PopDaCMinInvKg, "up", "down")

pop.1.b = pop.1
pop.1.c = pop.1


pop.1.b$min = ifelse(pop.1.b$shade == "up", NA, pop.1.b$min)
pop.1.b$max = ifelse(pop.1.b$shade == "up", NA, pop.1.b$max)
pop.1.b$shade = ifelse(pop.1.b$shade == "up", NA, pop.1.b$shade)
pop.1.b$tot.PopDaCTotInvKg = ifelse(pop.1.b$shade == "up", NA, pop.1.b$tot.PopDaCTotInvKg)

pop.1.c$min = ifelse(pop.1.c$shade == "down", NA, pop.1.c$min)
pop.1.c$max = ifelse(pop.1.c$shade == "down", NA, pop.1.c$max)
pop.1.c$shade = ifelse(pop.1.c$shade == "down", NA, pop.1.c$shade)
pop.1.c$tot.PopDaCTotInvKg = ifelse(pop.1.c$shade == "down", NA, pop.1.c$tot.PopDaCTotInvKg)

#--------------------------------------
p = ggplot(pop.1.b, aes(x = Date)) +
  geom_ribbon(aes(ymin = min, ymax = max), alpha = .5, fill = "gray90") +
  geom_ribbon(data = pop.1.c, aes(ymin = min, ymax = max), alpha = .5, fill = "gray25") +
  geom_line(data = pop.1.b, aes(y = tot.PopDaCTotInvKg), color = "gray50", alpha = .5, size = 1) +
  geom_line(data = pop.1.c, aes(y = tot.PopDaCTotInvKg), color = "gray50", alpha = .5, size = 1) +
  geom_line(data = pop.1, aes(y = tot.PopDaCMinInvKg), size = 1, color = "black") +
  # geom_line(data = pop.1, aes(y = tot.PopDaCMinInvKg), size = 1.5, color = "red") +
  geom_line(data = avg, aes(y = tot.PopDaCMinInvKg), color = "blue", size = 1.5) +
  # geom_line(data = avg, aes(y = tot.PopDaCMinInvKg), size = 1) +
  geom_line(data = avg, aes(y = tot.PopDaCTotInvKg), color = "orange") +
  scale_x_date(date_breaks = "3 month", date_labels = "%b \n %Y") +
  labs(x = "", y = "add label here") + # / area of lees ferry 25000 * 123
  yard_theme +
  theme(legend.position = c(.8,.9))
p

# G3.4 <- arrangeGrob(p,
#                     top = textGrob("C", x = unit(.95, "npc"),
#                                    y = unit(.95, "npc"),
#                                    just = c("left", "top"),
#                                    gp = gpar(col = "black", fontsize = 18)))
# 
# grid.arrange(G1.4, G2.4, G3.4)

#-----------------------------------------------------------------------------#
# RBT diet proportions by taxa
# See: Yard_Diet_Mass_V3.R in C:\Users\mdodrill\Desktop\FB_DOWN\Analysis\YARD

# this does not include the 'aggregate', change?!change?!change?!change?!change?!

diet = read.table(file = "C:/Users/mdodrill/Desktop/RBT_BioE/Git/RBT_Pop_BioE/Data_In/FB_RBT_Diet_Mass_by_Taxa_Lees_V2.csv", header = T, sep = ",")

diet$season = ifelse(diet$season == "spring", "Spring",
                     ifelse(diet$season == "summer", "Summer",
                            ifelse(diet$season == "fall", "Fall", "Winter")))

diet$season.2 = factor(diet$season, levels = c('Spring', 'Summer', 'Fall', 'Winter'),
                        ordered = TRUE)



p = ggplot(diet, aes(y = prop, x = year)) +
  geom_bar(aes(color = taxa, fill = taxa), stat = "identity", position = 'dodge') +
  labs(y = "Proportion of Biomass", x = "", color = "", fill = "") +
  facet_wrap(~ season.2, nrow = 1) +
  yard_theme +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14, vjust = 1),
        legend.position = c(.1,.9))

p

#-----------------------------------------------------------------------------#







