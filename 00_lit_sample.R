
# Load database, setting blank strings to NA (n.b., some blanks still persist...)
# data <- read.csv("smpl_yrs.csv", header = TRUE) # now retired
# data <- read.csv("smpl_yrs_181124.csv", header = TRUE) # has tighter filter re: mgmt
# data <- na.omit(data)

# data <- read.csv("rubric_simple.csv", header = TRUE) # no kemp
# data <- read.csv("rubric_simple_190406.csv", header = TRUE)
# w/ kemp 2016, on avg; duplicated for kemp 2019 -- same fires
# w/ haffey 2018
# w/ davis assuming sampled 2016, from tbl in SI (first pub 190311)
# w/ young et al. 2019
data <- read.csv("rubric_simple_190619.csv", header = TRUE)
# Includes all in mendeley folder called 190305 (plus Kim's paper that came out a week later on March 11th)
# data <- na.omit(data)



# single color
g <- ggplot(data, aes(x=yrs.btwn, fill = palette[4]))
p <- g + geom_vline(xintercept = median(data$yrs.btwn),
                    lty = 2, lwd = 0.5, col = "red") +
  geom_histogram(binwidth = 1, color = "white") +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        text = element_text(color = "black", size = 14), # sets all text elements
        legend.position = ("none"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()) +
  # scale_x_continuous(expand = c(0,0)) +
  scale_x_continuous(breaks = (seq(0, 45, by = 5)), expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), lim = c(0,50)) +
  coord_cartesian(ylim = c(0,50)) +
  scale_fill_manual(values = palette[4]) +
  labs(x = "Years between fire and sampling",
       y = "Number of sampling efforts", color = "black", size = 12) # +
# annotate("text", x = 12.5, y = 28, label = "median", size = 6)
p

p
dev.off()
tiff(paste0(out.dir,"SampYrHist1col_400dpi_",currentDate,"_.tiff"), res = 400, width = 5, height = 3, units = "in") 
p
dev.off()
# ggsave("SampYrHist1col_400dpi_190406.tiff", width = 5, height = 3, dpi = 400)

# 
# 
# # two colors
# my.col <- c("#009933", "#330066")
# 
# g <- ggplot(data, aes(x=yrs.btwn, fill=record.successive.samp))
# g + geom_histogram(breaks = seq(0,46, by = 1), color = "white") +
#   # coord_flip() +
#   theme_classic() +
#   theme(text = element_text(color = "black", size = 12), # sets all text elements
#         legend.position = ("none"),
#         axis.ticks.x = element_blank(),
#         axis.ticks.y = element_blank()) +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0), lim = c(0,25)) +
#   scale_fill_manual(values = rev(my.col)) +
#   labs(x = "Years between fire and sampling",
#        y = "Number of sampling efforts", color = "black", size = 16)
# 
# ggsave("SampYrHist2col_400dpi_181110.tiff", width = 5, height = 3, dpi = 400)
