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
library(gridExtra)

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
# Fig 2 - 

N.all = group_by(all2, Date, sz.group) %>%
  summarize(N.tot = sum(N))

all2$Pop.Mass.g = all2$N * all2$fish.mass




p.1 = ggplot(N.all, aes(x = Date, y = N.tot)) +
  # geom_bar(position = "stack",
  geom_bar(position = position_stack(reverse = TRUE),
           stat = "identity",
           # aes(color = rev(sz.group), fill = rev(sz.group))) +
           aes(color = sz.group, fill = sz.group)) +
  # geom_line(data = all2, aes(x = Date, y = Pop.Mass.g)) +
  theme_base()
p.1


mass.all = group_by(all2, Date, sz.group) %>%
  summarise(mass.tot = sum(Pop.Mass.g))


p.2 = ggplot(mass.all, aes(x = Date, y = mass.tot)) +
      geom_bar(position = position_stack(reverse = TRUE),
               stat = "identity",
               aes(color = sz.group, fill = sz.group)) +
  theme_base()
p.2


grid.arrange(p.1, p.2)


sub = N.all[which(N.all$Date == N.all$Date[1]),]
sub.2 = mass.all[which(mass.all$Date == mass.all$Date[1]),]


p.1 = ggplot(sub, aes(x = Date, y = N.tot)) +
  geom_bar(position = "stack", stat = "identity",
  # geom_bar(position = "dodge", stat = "identity",
           aes(color = sz.group, fill = sz.group)) + 
  theme_base()
p.1


