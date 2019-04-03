#################################################3
## Is there an asymptote? Check with GAM
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
gam.test <- gam(regen_pipo ~ s(YEAR.DIFF, k=5) , data=data.pipo, family = "binomial")
# gam.test <- gam(regen_pipo ~ s(YEAR.DIFF, k=3) + s(BALive_pipo, k = 3), data=data.pipo, family = "binomial")
summary(gam.test)  

# Is smoothing parameter k ok? Or am I fitting b/c it's too low?
gam.check(gam.test) # Suggests k=3 may be too low.
# Check with residuals: can remove any pattern by increasing k.
hist(residuals(gam.test)) # more resids < 0 mean more fits are > obs.
# Here, bs is a smoother to help avoid overfitting
gam(residuals(gam.test) ~ s(YEAR.DIFF, k = 20, bs = "cs"), data = data.pipo)
# GCV (gen cross-val) score, with smaller better.
# k = 3 in main model, GCV = 0.9409415 is generally smallest.
# k = 5 in main model, GCV = 0.9414019
# k = 10 n main model, GCV = 0.9412997 

plot.gam(gam.test, pages = 1, resid = T) # number plots = number smooths; plots component smooth functions.
# hist(resid(gam.test)) # b/c more neg resids --> more fitted > actual


# Predicting gives component smooth functions, not on scale of response var.
# If >1 covar, use type = terms to get col for each covar
new.yr <- seq(1, 30, 0.5) # use new data to predict
# new.df <- data.frame(YEAR.DIFF = 1:30, BALive_pipo = median(data.pipo$BALive_pipo))
predRegen<-predict(gam.test, list(YEAR.DIFF = new.yr), type = "response", se = TRUE)
up <- (predRegen$fit + 1.96 * predRegen$se.fit) 
down <- (predRegen$fit - 1.96 * predRegen$se.fit) 
pred.df <- data.frame(cbind(new.yr, pred = predRegen$fit, seup, sedwn))
# predRegen<-predict(gam.test, newdata = newd, type = "terms")

# plot(data.pipo$YEAR.DIFF, gam.test$fitted.values)
# points(new.yr, predRegen$fit, add = T, col = "blue")
# lines(new.yr,predRegen,col="blue",lwd=2)

pipo.plot.all <- ggplot() + 
  geom_point(data = data.pipo, aes(x=YEAR.DIFF, y=regen_pipo),
             shape = 16, size=1.5, alpha = 0.5,
             position = position_jitter(width = 1, height = 0)) +
  geom_line(data = pred.df, aes(x=new.yr, y = pred), lty = 1, size = 1) +
  geom_ribbon(data = pred.df, aes(x=new.yr, ymin = down, ymax = up),
              alpha = 0.25, fill = "grey") +
  scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
  scale_y_continuous(expand=c(0,0), limits=c(-0.15,1.25)) +
  labs(x = "Years between fire and sampling",
       y = "Probability of juvenile presence") +
  coord_cartesian(xlim=c(0,30), ylim=c(-0.05,1.05)) +
  theme_bw(base_size = 14) 
pipo.plot.all





#### Are there diff asymptotes for each node?
data.pipo$regen_pipo <- as.factor(data.pipo$regen_pipo)
glm.tree.pipo <- glmtree(regen_pipo ~ YEAR.DIFF
                         | BALive_pipo + FIRE.SEV,
                         data = data.pipo,
                         family = "binomial",
                         minsplit = 50,
                         ordinal = "L2")
plot(glm.tree.pipo)


## Get obs for each node (orig as df) and save row.names (match orig dataset)
# Create new col in df to assign node number
data.pipo$node <- NA 
data.pipo$node %>% factor()

# How many and what ID? Keep only those for assigning. length() gives all, width() gives terminal
# BUT ORDER OF NODES WON'T ALWAYS WORK! USE NODEID()
# diff <- (length(glm.tree.pipo) - width(glm.tree.pipo)) # Gives nodes that SHOULDN'T get assigned
# loop.ready <- ((1+diff):(width(glm.tree.pipo)+diff)) # gives node value that should get assigned
(loop.ready <- nodeids(glm.tree.pipo, terminal = TRUE))

# Loop through all terminal nodes
for (i in loop.ready){ 
  node <- paste0("node",(i))
  obs <- glm.tree.pipo[[i]]$data %>% row.names()
  data.pipo$node[rownames(data.pipo) %in% obs] <- paste0(node)
}

# What's fire sev like for higher BA node?
data.pipo %>%
  filter(node == "node5") %>%
  count(., FIRE.SEV) # 24 plots med or high

## Run gams on obs from each node
new.yr <- seq(0,32,0.5) # new YEAR.DIFF data for prediction
node <- NULL 
pred <- NULL 
up <- NULL 
down <- NULL 
pred.list <- list()
for (i in loop.ready){
  # subset by node
  data.temp <- data.pipo %>% filter(node == paste0("node",i))
  # run gam on that subset
  gam.temp <- gam(regen_pipo ~ s(YEAR.DIFF, k = 5), data = data.temp, family = "binomial")
  # predict and keep se
  temp <- predict(gam.temp, list(YEAR.DIFF = new.yr), type = "response", se = T)
  pred <- temp$fit
  up <- (temp$fit + 1.96 * temp$se.fit)
  down <- (temp$fit - 1.96 * temp$se.fit)
  node <- paste0("node",(i))
  pred.list[[i]] <- data.frame(cbind(new.yr, node, pred, up, down))
}
pred.df <- bind_rows(pred.list)
pred.df[,c(1,3,4,5)] <- apply(pred.df[,c(1,3,4,5)], 2, function(x) as.numeric(as.character(x)))
pred.df$node <- as.factor(pred.df$node)

## Plots for each node.
# For viz, need continuous response
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))

plot.list <- list()
for (i in loop.ready){
  data.temp <- data.pipo %>% filter(node == paste0("node",i))
  pred.temp <- pred.df %>% filter(node == paste0("node",i))
  plot <- ggplot() + 
    geom_point(data = data.temp, aes(x=YEAR.DIFF, y=regen_pipo),
               shape = 16, size=1.5, alpha = 0.5,
               position = position_jitter(width = 1, height = 0),
               col = pal[i]) +
    geom_line(data = pred.temp, aes(x=new.yr, y = pred), lty = 1, size = 1,
              col = pal[i]) +
    geom_ribbon(data = pred.temp, aes(x = new.yr, ymin = down, ymax = up), 
                alpha = 0.25, fill = pal[i]) +
    scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
    scale_y_continuous(expand=c(0,0), limits=c(-0.15,1.25)) +
    labs(x = "Years between fire and sampling",
         y = "Probability of juvenile presence",
         title = paste0("n = ",nrow(data.temp))) + 
    # scale_x/y_cont removes pts beyond limits; coord_cart overrides even if not plotted 
    coord_cartesian(xlim=c(0,30), ylim=c(-0.05,1.05)) +
    theme_bw(base_size = 14) 
    plot.list[[i]] <- plot

}
plot.list[[3]]
plot.list[[4]]
plot.list[[5]]

# tiff(paste0(out.dir,"pipo_tree_node3_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[3]]
# dev.off()
# tiff(paste0(out.dir,"pipo_tree_node4_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[4]]
# dev.off()
# tiff(paste0(out.dir,"pipo_tree_node5_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[5]]
# dev.off()


## Create plots for each node; can't align with expression. Fake it w/ phantom()
labels = c(expression("BA < 35" ~ ft^{2}~"/acre,"~ "no-low severity"),
           expression(~phantom(0)~"BA < 35" ~ ft^{2}~"/acre,"~ "med-hi severity"),
           expression("BA > 35" ~ ft^{2}~"/acre"~phantom(1000000000000))) # to align

p <- ggplot() +
  # state outlines
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = data.pipo, aes(x = LON_FS, y = LAT_FS, col = node),
             size = 4, alpha = 0.7) +
  scale_color_manual(values = pal[loop.ready],
                     labels = labels) + 
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side oflegend .position coords refer to 
        legend.position=c(0,0))
p
# tiff(paste0(out.dir,"pipo_tree_map_",currentDate,".tiff"),
#    width = 450, height = 600, units = "px")
# p
# dev.off()



################################################# PSME
## Is there an asymptote? Check with GAM
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))
## Get obs for each node (orig as df) and save row.names (match orig dataset)
# Create new col in df to assign node number
data.psme$node <- NA 
data.psme$node %>% factor()

# How many and what ID? Keep only those for assigning. 
(loop.ready <- nodeids(glm.tree.psme, terminal = TRUE))

# Loop through all terminal nodes
for (i in loop.ready){ 
  node <- paste0("node",(i))
  obs <- glm.tree.psme[[i]]$data %>% row.names()
  data.psme$node[rownames(data.psme) %in% obs] <- paste0(node)
}


## Run gams on obs from each node
new.yr <- seq(0,32,0.5) # new YEAR.DIFF data for prediction
node <- NULL 
pred <- NULL 
up <- NULL 
down <- NULL 
pred.list <- list()
for (i in loop.ready){
  # subset by node
  data.temp <- data.psme %>% filter(node == paste0("node",i))
  # run gam on that subset
  gam.temp <- gam(regen_psme ~ s(YEAR.DIFF, k = 5), data = data.temp, family = "binomial")
  # predict and keep se
  temp <- predict(gam.temp, list(YEAR.DIFF = new.yr), type = "response", se = T)
  pred <- temp$fit
  up <- (temp$fit + 1.96 * temp$se.fit)
  down <- (temp$fit - 1.96 * temp$se.fit)
  node <- paste0("node",(i))
  pred.list[[i]] <- data.frame(cbind(new.yr, node, pred, up, down))
}
pred.df <- bind_rows(pred.list)
pred.df[,c(1,3,4,5)] <- apply(pred.df[,c(1,3,4,5)], 2, function(x) as.numeric(as.character(x)))
pred.df$node <- as.factor(pred.df$node)

## Plots for each node.
# For viz, need continuous response
data.psme$regen_psme <- as.numeric(as.character(data.psme$regen_psme))

plot.list <- list()
for (i in loop.ready){
  data.temp <- data.psme %>% filter(node == paste0("node",i))
  pred.temp <- pred.df %>% filter(node == paste0("node",i))
  plot <- ggplot() + 
    geom_point(data = data.temp, aes(x=YEAR.DIFF, y=regen_psme),
               shape = 16, size=1.5, alpha = 0.5,
               position = position_jitter(width = 1, height = 0),
               col = pal[i]) +
    geom_line(data = pred.temp, aes(x=new.yr, y = pred), lty = 1, size = 1,
              col = pal[i]) +
    geom_ribbon(data = pred.temp, aes(x = new.yr, ymin = down, ymax = up), 
                alpha = 0.25, fill = pal[i]) +
    scale_x_continuous(expand=c(0,0), limits=c(0,30)) +
    scale_y_continuous(expand=c(0,0), limits=c(-0.15,1.25)) +
    labs(x = "Years between fire and sampling",
         y = "Probability of juvenile presence",
         title = paste0("n = ",nrow(data.temp))) + 
    # scale_x/y_cont removes pts beyond limits; coord_cart overrides even if not plotted 
    coord_cartesian(xlim=c(0,30), ylim=c(-0.05,1.05)) + 
    theme_bw(base_size = 14)
  plot.list[[i]] <- plot
  
}
plot.list[[3]]
plot.list[[4]]
plot.list[[6]]
plot.list[[7]]

# lowest BA & higher CMD has best probability.
hist(data.psme$def.tc[data.psme$node == "node3"])
hist(data.psme$def.tc[data.psme$node == "node4"], 30)
hist(data.psme$def.tc[data.psme$node == "node6"])
hist(data.psme$def.tc[data.psme$node == "node7"], 30)

data.psme %>%
  filter(node == "node4") %>%
  count(def.tc > 500)
data.psme %>%
  filter(node == "node4") %>%
  count(LAT_FS > 42) # S. border of Idaho
87/(87 + 141) # 0.38
data.psme %>%
  filter(node == "node4") %>%
  count(LAT_FS > 37) # S. border of UT & CO
127/(101 + 127) # 0.56
data.psme %>%
  filter(node == "node4") %>%
  filter(YEAR.DIFF > 20) %>%
  count(LAT_FS > 42, regen_psme)

#  `LAT_FS > 42`    regen_psme     n
# <lgl>              <dbl> <int>
#   1 TRUE                   0     1 # ID
#   2 TRUE                   1     5 # ID & MT


# tiff(paste0(out.dir,"psme_tree_node3_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[3]]
# dev.off()
# tiff(paste0(out.dir,"psme_tree_node4_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[4]]
# dev.off()
# tiff(paste0(out.dir,"psme_tree_node6_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[6]]
# dev.off()
# tiff(paste0(out.dir,"psme_tree_node7_",currentDate,".tiff"),
#       width = 400, height = 400, units = "px")
# plot.list[[7]]
# dev.off()

plot(glm.tree.psme)

## Create plots for each node; can't align with expression. Fake it w/ phantom()
labels = c(expression("BA < 15" ~ ft^{2}~"/acre,"~ "deficit < 422 mm"),
           expression("BA < 15" ~ ft^{2}~"/acre,"~ "deficit > 422 mm"),
           expression("BA > 15" ~ ft^{2}~"/acre,"~ "deficit < 506 mm"),
           expression("BA > 15" ~ ft^{2}~"/acre,"~ "deficit > 506 mm"))

p <- ggplot() +
  # state outlines
  geom_sf(data = NAmer, color = "#808B96", fill = "white") +
  geom_sf(data = IntWsts, color = "#808B96", fill = "#EAECEE") +
  coord_sf(xlim = c(-121, -100), ylim = c(30, 50), expand = FALSE) +
  geom_point(data = data.psme, aes(x = LON_FS, y = LAT_FS, col = node),
             size = 4, alpha = 0.7) +
  scale_color_manual(values = pal[loop.ready],
                     labels = labels) + 
  theme_bw(base_size = 12) +
  theme(panel.grid.major = element_line(color = "#808B96"), # blend lat/long into background
        panel.background = element_rect(fill = "#808B96"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.background = element_rect(color = "#808B96"),
        legend.justification=c(0,0), # defines which side of legend .position coords refer to 
        legend.position=c(0,0))
p
# tiff(paste0(out.dir,"psme_tree_map_",currentDate,".tiff"),
#    width = 450, height = 600, units = "px")
# p
# dev.off()
