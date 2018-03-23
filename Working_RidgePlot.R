###############################################################################
#                                                                      Mar 2018
#
#                 playing around with ridgeline plots
#
#  Notes:
#
#  To do:
#
###############################################################################
library(ggridges)
library(gridExtra)


windows(xpos = 25, record = T, width = 9 * 2, height = 5 * 2)

#-----------------------------------------------------------------------------#

all2$MidSz.2 = as.factor(all2$MidSz) 

all2$Pop.Mass.g = all2$N * all2$fish.mass


sz.groups = seq(75, 390, 10)

# sz.key = data.frame(sz = c(1:6),
#                     name = c("75-124", "125-174", "175-224",
#                              "225-274", "275-324", ">325"))
sz.key = data.frame(sz = c(1:length(sz.groups)),
                    name = as.character(sz.groups))

tmp = findInterval(all2$MidSz, sz.groups)

all2$sz.group = factor(sz.key[match(tmp, sz.key[,1]),2], ordered = T, levels = sz.key[,2])


# all2$MidSz.3 = as.factor(as.numeric(all2$MidSz.2))

dat.in.1 = group_by(all2, Date, sz.group) %>%
  summarise(mass.tot = sum(Pop.Mass.g))


p.a = ggplot(dat.in.1, aes(x = as.numeric(Date), y = sz.group, height = mass.tot, group = sz.group)) +
  geom_density_ridges(stat = "identity", scale = 2, aes(fill = sz.group), alpha = .5) +
  labs(y = "", x = "", title = "Biomass") +
  theme_base() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")

dat.in.2 = group_by(all2, Date, sz.group) %>%
  summarise(N.tot = sum(N))


p.b = ggplot(dat.in.2, aes(x = as.numeric(Date), y = sz.group, height = N.tot, group = sz.group)) +
  geom_density_ridges(stat = "identity", scale = 2, aes(fill = sz.group), alpha = .5) +
  labs(y = "", x = "", title = "Abundance") +
  theme_base() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")

grid.arrange(p.a, p.b, nrow = 1)


#-----------------------------------------------------------------------------#
# For the AGFD time series


all2$MidSz.2 = as.factor(all2$MidSz) 

all2$Pop.Mass.g = all2$N * all2$fish.mass


sz.groups = seq(0, 570, 10)

sz.key = data.frame(sz = c(1:length(sz.groups)),
                    name = as.character(sz.groups))

tmp = findInterval(all2$MidSz, sz.groups)

all2$sz.group = factor(sz.key[match(tmp, sz.key[,1]),2], ordered = T, levels = sz.key[,2])

u.date = all2[!duplicated(all2$Date),]$Date

date.key = data.frame(date = u.date, num = as.numeric(u.date), year = substr(u.date, 3, 4))

#--------------------------------------

dat.in.1 = group_by(all2, Date, sz.group) %>%
  summarise(mass.tot = sum(Pop.Mass.g)) %>%
  filter(sz.group >= 30 & sz.group <= 500)

dat.in.2 = group_by(all2, Date, sz.group) %>%
  summarise(N.tot = sum(N)) %>%
  filter(sz.group >= 30 & sz.group <= 500)


p.a = ggplot(dat.in.1, aes(x = as.numeric(Date), y = sz.group, height = mass.tot, group = sz.group)) +
  geom_density_ridges(stat = "identity", scale = 2, aes(fill = sz.group), alpha = .5) +
  labs(y = "Size Bin", x = "", title = "Biomass") +
  scale_x_continuous(labels = date.key$year, breaks = date.key$num) +
  theme_base() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")

p.b = ggplot(dat.in.2, aes(x = as.numeric(Date), y = sz.group, height = N.tot, group = sz.group)) +
  geom_density_ridges(stat = "identity", scale = 2, aes(fill = sz.group), alpha = .5) +
  labs(y = "", x = "", title = "Abundance") +
  scale_x_continuous(labels = date.key$year, breaks = date.key$num) +
  theme_base() +
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")

grid.arrange(p.a, p.b, nrow = 1)

#-----------------------------------------------------------------------------#