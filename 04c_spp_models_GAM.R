#################################################3
## Is there an asymptote? Check with GAM
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
gam.test <- gam(regen_pipo ~ s(YEAR.DIFF, k=3) , data=data.pipo, family = "binomial")
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
seup <- (predRegen$fit + 1.96 * predRegen$se.fit) 
sedwn <- (predRegen$fit - 1.96 * predRegen$se.fit) 
pred.df <- data.frame(cbind(new.yr, pred = predRegen$fit, seup, sedwn))
# predRegen<-predict(gam.test, newdata = newd, type = "terms")

# plot(data.pipo$YEAR.DIFF, gam.test$fitted.values)
# points(new.yr, predRegen$fit, add = T, col = "blue")
# lines(new.yr,predRegen,col="blue",lwd=2)

pipo.plot <- ggplot() + 
  geom_point(data = data.pipo, aes(x=YEAR.DIFF, y=regen_pipo),
             shape = 16, size=1,
             position = position_jitter(width = 1, height = 0)) +
  geom_line(data = pred.df, aes(x=new.yr, y = pred), lty = 1) +
  geom_line(data = pred.df, aes(x=new.yr, y = seup), lty = 2) +
  geom_line(data = pred.df, aes(x=new.yr, y = sedwn), lty = 2) + 
  xlim(0,30) +
  ylim(0,1) + 
  theme_bw()
pipo.plot



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
diff <- (length(glm.tree.pipo) - width(glm.tree.pipo)) # Gives nodes that SHOULDN'T get assigned
loop.ready <- ((1+diff):(width(glm.tree.pipo)+diff)) # gives node value that should get assigned

# Loop through all terminal nodes
for (i in loop.ready){ # length(summary) to only get terminal nodes
  node <- paste0("node",(i))
  obs <- glm.tree.pipo[[i]]$data %>% row.names()
  data.pipo$node[rownames(data.pipo) %in% obs] <- paste0(node)
}



## Run gams on obs from each node
new.yr <- seq(0,30,0.5) # new data for prediction
node <- NULL 
pred <- NULL 
up <- NULL 
down <- NULL 
pred.list <- list()
for (i in loop.ready){
  # subset by node
  data.temp <- data.pipo %>% filter(node == paste0("node",i))
  # run gam on that subset
  gam.temp <- gam(regen_pipo ~ s(YEAR.DIFF, k = 3), data = data.temp, family = "binomial")
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
               shape = 16, size=1, alpha = 0.5,
               position = position_jitter(width = 1, height = 0)) +
    geom_line(data = pred.temp, aes(x=new.yr, y = pred), lty = 1, size = 1) +
    geom_ribbon(data = pred.temp, aes(x = new.yr, ymin = down, ymax = up), alpha = 0.25) +
    xlim(0,30) +
    ylim(0,1) + 
    theme_minimal(base_size = 12)
  plot.list[[i]] <- plot
}
plot.list[[3]]


## Predictions for each node
node3 <- data.pipo[data.pipo$NODE == "node3",]
gam.node3 <- gam(regen_pipo ~ s(YEAR.DIFF, k=3) , data=node3, family = "binomial")
# node 3 preds
pred3<-predict(gam.node3, list(YEAR.DIFF = new.yr), type = "response", se = TRUE)
seup3 <- (pred3$fit + 1.96 * pred3$se.fit) 
sedwn3 <- (pred3$fit - 1.96 * pred3$se.fit) 
pred3.df <- data.frame(cbind(new.yr, pred3 = pred3$fit, seup3, sedwn3))

# node 3 plot
node3.plot <- ggplot() + 
  geom_point(data = data.pipo, aes(x=YEAR.DIFF, y=regen_pipo),
             shape = 16, size=1, alpha = 0.5,
             position = position_jitter(width = 1, height = 0)) +
  geom_line(data = pred3.df, aes(x=new.yr, y = pred3), lty = 1, size = 1) +
  geom_ribbon(data = pred3.df, aes(x = new.yr, ymin = sedwn3, ymax = seup3), alpha = 0.25) +
  xlim(0,30) +
  ylim(0,1) + 
  theme_minimal(base_size = 12)
node3.plot



# node 4 plot
node4.plot <- ggplot() + 
  geom_point(data = data.pipo, aes(x=YEAR.DIFF, y=regen_pipo),
             shape = 16, size=1,
             position = position_jitter(width = 1, height = 0)) +
  geom_line(data = pred4.df, aes(x=new.yr, y = pred4), lty = 1) +
  geom_line(data = pred4.df, aes(x=new.yr, y = seup4), lty = 2) +
  geom_line(data = pred4.df, aes(x=new.yr, y = sedwn4), lty = 2) + 
  xlim(0,30) +
  ylim(0,1) + 
  theme_minimal(base_size = 12)
node4.plot

# node 5 plot
node5.plot <- ggplot() + 
  geom_point(data = data.pipo, aes(x=YEAR.DIFF, y=regen_pipo),
             shape = 16, size=1,
             position = position_jitter(width = 1, height = 0)) +
  geom_line(data = pred5.df, aes(x=new.yr, y = pred5), lty = 1) +
  geom_line(data = pred5.df, aes(x=new.yr, y = seup5), lty = 2) +
  geom_line(data = pred5.df, aes(x=new.yr, y = sedwn5), lty = 2) + 
  xlim(0,30) +
  ylim(0,1) + 
  theme_minimal(base_size = 12)
node5.plot








node_dynamite <- function(obj, factor = 1,
                          col = "black",
                          fill = "lightgray",
                          bg = "white",
                          width = 0.5,
                          yscale = NULL,
                          ylines = 3,
                          cex = 0.5,
                          id = TRUE,
                          mainlab = NULL, 
                          gp = gpar())
{
  ## observed data/weights and tree fit
  y <- obj$fitted[["(response)"]]
  stopifnot(is.numeric(y))
  g <- obj$fitted[["(fitted)"]]
  w <- obj$fitted[["(weights)"]]
  if(is.null(w)) w <- rep(1, length(y))
  
  ## (weighted) means and standard deviations by node
  n <- tapply(w, g, sum)
  m <- tapply(y * w, g, sum)/n
  s <- sqrt(tapply((y - m[factor(g)])^2 * w, g, sum)/(n - 1))
  
  if (is.null(yscale)) 
    yscale <- c(min(c(0, (m - factor * s) * 1.1)), max(c(0, (m + factor * s) * 1.1)))
  
  ### panel function for boxplots in nodes
  rval <- function(node) {
    
    ## extract data
    nid <- id_node(node)
    mid <- m[as.character(nid)]
    sid <- s[as.character(nid)]
    wid <- n[as.character(nid)]
    
    top_vp <- viewport(layout = grid.layout(nrow = 2, ncol = 3,
                                            widths = unit(c(ylines, 1, 1), 
                                                          c("lines", "null", "lines")),  
                                            heights = unit(c(1, 1), c("lines", "null"))),
                       width = unit(1, "npc"), 
                       height = unit(1, "npc") - unit(2, "lines"),
                       name = paste("node_dynamite", nid, sep = ""),
                       gp = gp)
    
    pushViewport(top_vp)
    grid.rect(gp = gpar(fill = bg, col = 0))
    
    ## main title
    top <- viewport(layout.pos.col=2, layout.pos.row=1)
    pushViewport(top)
    if (is.null(mainlab)) { 
      mainlab <- if(id) {
        function(id, nobs) sprintf("Node %s (n = %s)", id, nobs)
      } else {
        function(id, nobs) sprintf("n = %s", nobs)
      }
    }
    if (is.function(mainlab)) {
      mainlab <- mainlab(names(obj)[nid], wid)
    }
    grid.text(mainlab)
    popViewport()
    
    plot <- viewport(layout.pos.col = 2, layout.pos.row = 2,
                     xscale = c(0, 1), yscale = yscale,
                     name = paste0("node_dynamite", nid, "plot"),
                     clip = FALSE)
    
    pushViewport(plot)
    
    grid.yaxis()
    grid.rect(gp = gpar(fill = "transparent"))
    grid.clip()
    
    xl <- 0.5 - width/8
    xr <- 0.5 + width/8
    
    ## box & whiskers
    grid.rect(unit(0.5, "npc"), unit(0, "native"), 
              width = unit(width, "npc"), height = unit(mid, "native"),
              just = c("center", "bottom"), 
              gp = gpar(col = col, fill = fill))
    grid.lines(unit(0.5, "npc"), 
               unit(mid + c(-1, 1) * factor * sid, "native"), gp = gpar(col = col))
    grid.lines(unit(c(xl, xr), "npc"), unit(mid - factor * sid, "native"), 
               gp = gpar(col = col))
    grid.lines(unit(c(xl, xr), "npc"), unit(mid + factor * sid, "native"), 
               gp = gpar(col = col))
    
    upViewport(2)
  }
  
  return(rval)
}
class(node_dynamite) <- "grapcon_generator"







# IMPORTANT CAVEAT: CUMU LIKELIHOOD OF REGEN NOT HITTING 1 ISN'T A BAD THING -- ESP IF WE SEE THERE'S STILL IVE BA