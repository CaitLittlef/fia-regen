x <- seq(0, 20, 0.1)
y <- 2*sin(x) + 0.2*x + 2
z <- -2*sin(x) + 0.2*x + 2

# plot(x,y+0.1*x,type = "l")
# plot(x,z+0.1*x,type = "l")

df <- data.frame(x,y,z)
# Are neither, one, or both (N1B) regions experiencing suitable conditions?
# Setting 4.3 as threshold above which unsuitable conditions exist.
df$N1B <- as.factor(ifelse(df$y <= 4.3 & df$z <= 4.3, "B", ifelse(df$y >= 4.3 & df$z >= 4.3, "N", "one")))
df$N1B <- factor(df$N1B, levels = c("B", "one", "N"))
df <- tidyr::gather(df, key = "yz", value = "value", -c("x", "N1B"))

display.brewer.pal(8, "Dark2")

g <- ggplot(data = df) +
  # geom_col(aes(x = x, y = -0.1*max(df$value), fill = N1B),
  geom_col(aes(x = x, y = 4.3, fill = N1B),         
           width = 0.1,
           alpha = 0.5) + # *max to make all same height
  geom_hline(yintercept = 4.3, col = palette[8], linetype="dashed", size = 1.25) +
  geom_line(aes(x = x, y = value, color = yz), size = 1.5) +
  labs(x = "Time",
       y = "Climate variable") +
  scale_x_discrete(expand=c(0,0), limits=c(1,20)) + # instead of continuous to align cols w/ lines
  # scale_y_continuous(expand=c(0,0), limits=c(-0.1*max(df$value),8)) +
  scale_y_continuous(expand=c(0,0), limits=c(0,8)) +
  scale_color_manual(values = palette[c(2,3)],
                     labels=c("Region A", "Region B")) +
  scale_fill_manual(values = c(palette[c(1, 5, 8)]),
                     labels=c("Recruitment in both regions", "Recruitment in one region", "Recruitment in neither region")) +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        legend.justification="left", 
        legend.position = c(0, 0.93),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.box = "horizontal",
        legend.background = element_rect(color = "transparent", fill = "transparent"),
        plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) +
  annotate("text", x = 0.25, y = 4.55, label = "Threshold", color = palette[8], size = 6, hjust = 0)
g


png(paste0(out.dir, "concept_diag_", currentDate,".png"),
    width = 550, height = 500, units = "px", pointsize = 12)
pdf(paste0(out.dir, "concept_diag_", currentDate,".pdf"),
    width = 8, height = 7)
g; dev.off() ; dev.off()

