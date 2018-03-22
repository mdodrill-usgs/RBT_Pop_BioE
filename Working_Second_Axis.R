# library(gridExtra)
# library(ggplot2)
# library(grid)
# 
# x <- data.frame(
#   date = seq(as.Date("2012-01-01"),as.Date("2012-12-31"), by="week"), 
#   rain = sample(0:20,53,replace=T),
#   flow = sample(50:200,53,replace=T))
# 
# 
# 
# 
# x$rain2 <- x$rain * (100-50)/max(x$rain) + (50-min(x$rain,na.rm=T))  
# summary(x$rain2)
# 
# 
# 
# ## What we want the axis to show
# ylimits <- seq(0,20, by = 5)
# 
# ## What we need to specify in our graph
# ylimits2 <- ylimits * 50/max(x$rain) + (50-min(x$rain,na.rm=T))
# 
# g.bottom <- ggplot(x, aes(x = date, y = flow)) +
#   geom_line() +  #plot flow
#   geom_line(aes(y = rain2), colour = "blue", size =1.5) +  # plot rain2
#   ## specify our yaxis limits and remove any axis expansion
#   scale_y_continuous(expand = c(0,0), limits = c(50,200)) +  
#   labs(x = "Date", y = "River flow (m/s)") +
#   theme_classic() +
#   theme(plot.background = element_rect(fill = "transparent"),
#         plot.margin = unit(c(2,0,1,1),units="lines"))
# g.bottom
# 
# 
# 
# g.y <- ggplot(x, aes(x = date, y = rain2)) +
#   theme_classic() + 
#   geom_line(colour = "transparent") +
#   scale_y_continuous(breaks = ylimits2, labels = ylimits, expand = c(0,0),
#                      limits = c(50,200)) +
#   scale_x_date(limits = c(min(x$date),min(x$date))) +
#   labs(y = "Rain (mm)") +
#   ## Adjust the placement of the y axis title
#   theme(axis.title.y = element_text(vjust = 4.5, hjust = 0.88,angle = 270),  
#         ## Adjust the justification of the y axis labels
#         axis.text.y = element_text(hjust=0),  
#         ## Reverse the ticks to go on the other side
#         axis.ticks.length = unit(-0.15,"cm"),
#         ## Reverse spacing between ticks and text to move text to the right
#         axis.ticks.margin = unit(-0.5, "cm"),
#         axis.title.x = element_blank(), ## Remove all x-axis attributes
#         axis.text.x = element_blank(),
#         axis.ticks.x = element_blank(),
#         axis.line.x = element_blank(),
#         plot.background = element_rect(fill = "transparent"),
#         plot.margin = unit(c(2,0,3.85,-1.5),units="lines"))
# 
# 
# vp1 <- viewport(width = 0.9, height = 1, x = 0, y = 0.5, just = c(0,0.5))
# vp2 <- viewport(width = 0.1, height = 1, x = 0.9, y = 0.5,just = c(0,0.5))
# 
# 
# 
# print(g.bottom, vp=vp1)
# print(g.y, vp=vp2)
# 
# 
# 
# #-----------------------------------------------------------------------------#
# # http://rpubs.com/kohske/dual_axis_in_ggplot2
# 
# library(ggplot2)
# library(gtable)
# library(grid)
# 
# grid.newpage()
# 
# # two plots
# p1 <- ggplot(mtcars, aes(mpg, disp)) + geom_line(colour = "blue") + theme_bw()
# p2 <- ggplot(mtcars, aes(mpg, drat)) + geom_line(colour = "red") + theme_bw() %+replace% 
#   theme(panel.background = element_rect(fill = NA))
# 
# # extract gtable
# g1 <- ggplot_gtable(ggplot_build(p1))
# g2 <- ggplot_gtable(ggplot_build(p2))
# 
# # overlap the panel of 2nd plot on that of 1st plot
# pp <- c(subset(g1$layout, name == "panel", se = t:r))
# g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
#                      pp$l, pp$b, pp$l)
# 
# # axis tweaks
# ia <- which(g2$layout$name == "axis-l")
# ga <- g2$grobs[[ia]]
# ax <- ga$children[[2]]
# ax$widths <- rev(ax$widths)
# ax$grobs <- rev(ax$grobs)
# ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
# # draw it
# grid.draw(g)
# 
# 
# 
# 
# 
# 
#-----------------------------------------------------------------------------#
# https://gist.github.com/jslefche/e4c0e9f57f0af49fca87#file-ggplot_dual_axis-r


# ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
# 
#   # Update plot with transparent panel
#   plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
# 
#   grid.newpage()
# 
#   # Increase right margin if which.axis == "y"
#   if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
# 
#   # Extract gtable
#   g1 = ggplot_gtable(ggplot_build(plot1))
# 
#   g2 = ggplot_gtable(ggplot_build(plot2))
# 
#   # Overlap the panel of the second plot on that of the first
#   pp = c(subset(g1$layout, name == "panel", se = t:r))
# 
#   g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
# 
#   # Steal axis from second plot and modify
#   axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
# 
#   ia = which(g2$layout$name == axis.lab)
# 
#   ga = g2$grobs[[ia]]
# 
#   ax = ga$children[[2]]
# 
#   # Switch position of ticks and labels
#   if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
# 
#   ax$grobs = rev(ax$grobs)
# 
#   if(which.axis == "x") {
# 
#     ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
# 
#   } else {
# 
#     ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
# 
#   }
# 
#   # Modify existing row to be tall enough for axis
#   if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
# 
#   # Add new row or column for axis label
#   if(which.axis == "x") {
# 
#     g = gtable_add_grob(g, ax, 2, 4, 2, 4)
# 
#     g = gtable_add_rows(g, g2$heights[1], 1)
# 
#     g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
# 
#   } else {
# 
#     g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# 
#     g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
#     # g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
# 
#   }
# 
#   # Draw it
#   grid.draw(g)
# 
# }
# 
# 
# ggplot_dual_axis(p1,p2,which.axis = "y")

#-----------------------------------------------------------------------------#
# using element_blank in any theme will blow this up :(

ggplot_dual_axis_2 = function(plot1, plot2) {
  
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = "axis-l"
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  # ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  # ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.3, "cm")
  ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.3, "cm")
  
  # ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Add new row or column for axis label
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[13]], pp$t, length(g$widths), pp$b - 1)  # axis label 2
    
  # Draw it
  grid.draw(g)
  
}




# ggplot_dual_axis_2(p1, p2)

# see also...

# https://stackoverflow.com/questions/44616530/axis-labels-on-two-lines-with-nested-x-variables-year-below-months/44616739#44616739



#-----------------------------------------------------------------------------#
# using element_blank in any theme will blow this up :(
ggplot_dual_axis_3 = function(plot1, plot2) {

# plot1 = p1
# plot2 = p2
  
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = "axis-l"
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  # ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.3, "cm")
  
  # Add new row or column for axis label
  
  g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  
  g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
  
  g = gtable_add_grob(g, g2$grob[[13]], pp$t, length(g$widths), pp$b - 1)  # axis label 2
  
  #-------------------------------------------------
  # Steal axis from second plot and modify
    axis.lab.2 = "axis-b"

    ia.2 = which(g2$layout$name == axis.lab.2)

    ga.2 = g2$grobs[[ia.2]]

    ax.2 = ga.2$children[[2]]

    # Switch position of ticks and labels
    # if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
    # 
    # ax$grobs = rev(ax$grobs)
    # 
    # if(which.axis == "x") {
    # 
    #   ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm")
    # 
    # } else {
    # 
    #   ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    # 
    # }

    # Modify existing row to be tall enough for axis
    # if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]

    # Add new row or column for axis label
    # if(which.axis == "x") {

      g = gtable_add_grob(g, ax.2, -4, 8, -4, 4)

      # g = gtable_add_rows(g, g2$heights[1], 1)

      # g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
      
      
      # g = gtable_add_grob(g, ax.2, 2, 4, 2, 4)

      # g = gtable_add_rows(g, g2$heights[1], 1)
      # 
      # g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
      # 
# 
#     } else {
# 
#       g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
# 
#       g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
# 
#       # g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
  grid.draw(g)
  
}



  
  
  
  

  
  
  
  
  
  
  
  
  