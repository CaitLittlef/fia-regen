https://stackoverflow.com/questions/49472390/overlaying-boxplot-with-histogram-in-ggplot2
https://stackoverflow.com/questions/4551582/combination-boxplot-and-histogram-using-ggplot2

# install.packages("gridExtra")
# install.packages("ggExtra")

library(gridExtra)
library(ggExtra) # from R-forge


# Create partial curve (orig data) and add in new ribbon data
plot <- ggplot() +
  geom_smooth(data = pred.df.long, aes(x = x, y = mean.y), span = 0.25, se = FALSE, col = "black") + 
  geom_ribbon(data = df.up.low, aes(x = x, ymin = ymin, ymax = ymax),
              alpha = 0.5, fill = "light grey") +
  geom_vline(xintercept = quant[1], lty = 2, lwd = 1, col = "red") +
  geom_vline(xintercept = quant[2], lty = 2, lwd = 1, col = "red") +
  
  # geom_rug(data = data.brt, aes(x = explan.vars[i]), sides = "b") + 
  
  scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
                              max(data.brt[,explan.vars[i]]))) +
  scale_y_continuous(expand=c(0,0), limits=c(0.15,0.30)) +
  expand_limits(x = 0) + 
  # labs(x = paste0(explan.vars.names[i]),
  labs(x = NULL,
       y = "Probability of juvenile presence") +
  coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
                         max(data.brt[,explan.vars[i]]))) +
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.margin=unit(c(1,1,-0.5,1), "cm"))

hist <- ggplot(data = data.brt, aes(x = data.brt[,explan.vars[i]])) +
  geom_histogram(binwidth = 0.1) +
  geom_vline(xintercept = quant[1], lty = 2, lwd = 1, col = "red") +
  geom_vline(xintercept = quant[2], lty = 2, lwd = 1, col = "red") +
  # scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
  #                             max(data.brt[,explan.vars[i]]))) +
  coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
                         max(data.brt[,explan.vars[i]]))) +
  labs(x = paste0(explan.vars.names[i]),
       # y = "Frequency") +
       # y = NULL) +
       y = "   ") + # else not aligned
  theme_bw(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=unit(c(-0.5,1,1,1), "cm"))
  
## Specifying width
# Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange

plots <- list(plot, hist)
grobs <- list()
widths <- list()


# Collect the widths for each grob of each plot
for (l in 1:length(plots)){
  grobs[[l]] <- ggplotGrob(plots[[l]])
  widths[[l]] <- grobs[[l]]$widths[2:5]
}

# Use do.call to get the max width
maxwidth <- do.call(grid::unit.pmax, widths)

# Assign the max width to each grob
for (l in 1:length(grobs)){
  grobs[[l]]$widths[2:5] <- as.list(maxwidth)
  
}

# Plot
tiff(paste0(plot.dir, "/", explan.vars[[i]], "_alt2.tiff"))
# do.call("grid.arrange", c(grobs, ncol = 1))
grid.arrange(grobs = grobs, ncol = 1, heights = c(2,1))
dev.off()


