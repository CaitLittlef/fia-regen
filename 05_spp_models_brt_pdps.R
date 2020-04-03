## To use this, must have either run 04_spp_models_brt.R.
# OR load data and models at start of that script then proceed here.

#############################################
##PDPS FOR EACH VARIABLE#####################
#############################################
## CHANGES FOR EACH SPECIES:
# Lines 97-100: change plotting limits


## pdps account for average effect of other vars
# ref: https://stats.stackexchange.com/questions/122721/r-partial-dependency-plots-from-gbm-package-values-and-y-axis/122802

par(mfrow=c(1,1))

predictors<-list(rep(NA,length(models))) ## space for data: however many models are run
responses<-list(rep(NA,length(models)))

## Create folder for plots
currentDate <- Sys.Date()
dir.create(paste0(out.dir, sp,"_brt_plots_", currentDate))
plot.dir <- paste0(out.dir, sp,"_brt_plots_", currentDate,"/")


## Loop through vars. Orig code overlaid loess fit and hist iteratively. 
## If factor, have to run subsequent loop to create boxplot.

str(data.brt[,explan.vars]) # exclude FIRE.SEV, the 5th one

for (i in c(1:4)){ # thru 4 for pipo, to exclude FIRE.SEV
# for (i in 1:(length(explan.vars))){   # works if none are factors
  
  # Loop through the models and populate  lists of predictors & (marginal) responses.
  # With plot.gbm, other vars  "integrated out" -- not true pdp with mean effect of other vars.
  # With type = "response", automagically on scale of repsonse var (don't need to subtract mean)
  # Return.grid skips plot and gives grid of predictor vals and response vals
  
  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    r1 <- gbm::plot.gbm(gbm.mod, i.var = i, type = "response", return.grid = TRUE)
    predictors[[j]]<-r1[,1]
    responses[[j]]<-r1[,2]
  }
  
  
  # Get limits for plotting
  ymin=min(unlist(responses))
  ymax=max(unlist(responses))
  xmin=min(unlist(predictors))
  xmax=max(unlist(predictors))
  
  ## Tidy data into single dataframe so can add multiple curves onto same plot)
  predictor.cols <- do.call(cbind, predictors) # each col reps a model -- VALS ALL IDENTICAL
  response.cols <- do.call(cbind, responses) # each col reps a model
  pred.df <- as.data.frame(cbind(predictor.cols[,1], response.cols))
  colnames(pred.df) <- c("x", paste0("y", 1:length(models)))
  
  ## Gather into long-form with diff rows for each predictor and each model
  pred.df.long.all <- tidyr::gather(pred.df, key = "model", value = "y", -"x")
  
  ## Compute mean and upper/lower bounds, summarizing all models
  pred.df.long <- pred.df.long.all %>%
    group_by(x) %>%
    summarise(mean.y = mean(y),
              # median.y = quantile(y, probs = 0.50),
              lower.y = quantile(y, probs = 0.025),
              upper.y= quantile(y, probs = 0.975))
              # lower.y = quantile(y, probs = 0.05),
              # upper.y= quantile(y, probs = 0.95))
  
  
  ## Create plotting object for 95% bounds with smooth, else ribbin is choppy)
  ## Match span here (for smoothing) with geom_smooth() span in plot
  g1 <- ggplot(pred.df.long) + 
    stat_smooth(aes(x = x, y = lower.y), method = "loess", span = 0.25, se = FALSE) +
    stat_smooth(aes(x = x, y = upper.y), method = "loess", span = 0.25, se = FALSE)
  
  ## Build plot object for rendering 
  gg1 <- ggplot_build(g1)
  
  ## Extract data for the loess lines from the 'data' slot
  df.up.low <- data.frame(x = gg1$data[[1]]$x,
                          ymin = gg1$data[[1]]$y,
                          ymax = gg1$data[[2]]$y)
  
  ## Add 5th & 95th percentile values so we can know which range of predictor var to trust
  (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.1, 0.9)))
  # (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
  
  ## Create partial curve (orig data) and add in new ribbon data
  # plot <- ggplot() +
  #   geom_vline(xintercept = quant[1], lty = 2, lwd = 0.5, col = "red") +
  #   geom_vline(xintercept = quant[2], lty = 2, lwd = 0.5, col = "red") +
  #   geom_smooth(data = pred.df.long,
  #               aes(x = x, y = mean.y),
  #               span = 0.25,
  #               se = FALSE,
  #               col = "black") + 
  #   geom_ribbon(data = df.up.low,
  #               aes(x = x, ymin = ymin, ymax = ymax),
  #               alpha = 0.25, fill = "light grey") +
  #   scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
  #                               max(data.brt[,explan.vars[i]]))) +
  #   scale_y_continuous(expand=c(0,0),
  #   limits=c(0.15,0.31)) + #else ribbon for yr diff cut-off pipo
  #   # scale_y_continuous(expand=c(0,0),
  #   #                    limits=c(0.15,0.46)) + #else ribbon for yr diff cut-off psme
  #   expand_limits(x = 0) + 
  #   # labs(x = paste0(explan.vars.names[i]),
  #   labs(x = NULL,
  #        y = "Probability of juvenile presence") +
  #   coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
  #                          max(data.brt[,explan.vars[i]]))) +
  #   theme_bw(base_size = 18) +
  #   theme(panel.grid.minor = element_blank(),
  #         panel.grid.major = element_blank(),
  #         axis.title.x=element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.ticks.x=element_blank(),
  #         plot.margin=unit(c(5.5, 5.5, -10, 5.5), "pt")) # shrink margins for adjacency
  
  
  ## Alt: include all model runs as individ lines, overlay average
  plot <- ggplot() +
    geom_vline(xintercept = quant[1], lty = 2, lwd = 0.5, col = "red") +
    geom_vline(xintercept = quant[2], lty = 2, lwd = 0.5, col = "red") +
    geom_smooth(data = pred.df.long.all,
                aes(x = x, y = y, group = model),
                span = 0.25,
                se = FALSE,
                col = "light grey",
                lwd = 0.25) + 
    geom_smooth(data = pred.df.long,
                aes(x = x, mean.y),
                span = 0.25,
                se = FALSE,
                col = "black") + 
    # geom_ribbon(data = df.up.low,
    #             aes(x = x, ymin = ymin, ymax = ymax),
    #             alpha = 0.25, fill = "light grey") +
    scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
                                max(data.brt[,explan.vars[i]]))) +
    scale_y_continuous(expand=c(0,0),
                       # limits=c(0.15,0.31)) + #else ribbon for yr diff cut-off pipo
    # scale_y_continuous(expand=c(0,0),
                       limits=c(0.15,0.46)) + #else ribbon for yr diff cut-off psme
    expand_limits(x = 0) + 
    # labs(x = paste0(explan.vars.names[i]),
    labs(x = NULL,
         y = "Probability of juvenile presence") +
    coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
                           max(data.brt[,explan.vars[i]]))) +
    theme_bw(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin=unit(c(5.5, 5.5, -10, 5.5), "pt")) # shrink margins for adjacency
  
  
  
  ## Create hist to put underneath partial curve. Use orig data.
  hist <- ggplot(data = data.brt, aes(x = data.brt[,explan.vars[i]])) +
    geom_vline(xintercept = quant[1], lty = 2, lwd = 0.5, col = "red") +
    geom_vline(xintercept = quant[2], lty = 2, lwd = 0.5, col = "red") +
    geom_histogram(bins = 30) +
    # scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
    #                             max(data.brt[,explan.vars[i]]))) +
    coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
                           max(data.brt[,explan.vars[i]]))) +
    labs(x = explan.vars.names[i]) +
    theme_bw(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=unit(c(-10, 5.5, 5.5, 5.5), "pt")) # shrink margins for adjacency
  
  
  ## To stack plots with aligned width, extract max width from each object.
  # Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange
  
  plots <- list(plot, hist)
  grobs <- list()
  widths <- list()
  
  ## Collect the widths for each grob of each plot
  for (l in 1:length(plots)){
    grobs[[l]] <- ggplotGrob(plots[[l]])
    widths[[l]] <- grobs[[l]]$widths[2:5]
  }
  
  ## Use do.call to get the max width
  maxwidth <- do.call(grid::unit.pmax, widths)
  
  ## Assign the max width to each grob
  for (l in 1:length(grobs)){
    grobs[[l]]$widths[2:5] <- as.list(maxwidth)
  }
  
  ## Plot
  png(paste0(plot.dir, explan.vars[[i]], ".png"), width = 450, height = 450, units = "px")
  # tiff(paste0(plot.dir, explan.vars[[i]], ".tiff"))
  # do.call("grid.arrange", c(grobs, ncol = 1))
  grid.arrange(grobs = grobs, ncol = 1, heights = c(3,1))
  dev.off()

}  

dev.off()


##############################

#### PDPS FOR FACTOR VARIABLES (FIRE.SEV) ####

str(data.brt[,explan.vars]) # exclude FIRE.SEV, the 5th one
for (i in c(5)){

  # Loop through the models and populate  lists of predictors & (marginal) responses.
  # With plot.gbm, other vars  "integrated out" -- not true pdp with mean effect of other vars.
  # With type = "response", automagically on scale of repsonse var (don't need to subtract mean)
  # Return.grid skips plot and gives grid of predictor vals and response vals
  
  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    r1 <- gbm::plot.gbm(gbm.mod, i.var = i, type = "response", return.grid = TRUE)
    predictors[[j]]<-r1[,1]
    responses[[j]]<-r1[,2]
  }
  
  # currentDate <- Sys.Date()
  tiff(paste0(plot.dir, explan.vars[[i]], ".tif"))
  
  # Get limits for plotting
  ymin=min(unlist(responses))
  ymax=max(unlist(responses))
  # xmin=min(unlist(predictors)) # fire sev scale 1-4
  # xmax=max(unlist(predictors)) # fire sev scale 1-4
  
  ## Tidy data into single dataframe so can add multiple curves onto same plot)
  predictor.cols <- do.call(cbind, predictors) # each col reps a model -- VALS ALL IDENTICAL
  response.cols <- do.call(cbind, responses) # each col reps a model
  pred.df <- as.data.frame(cbind(predictor.cols[,1], response.cols))
  colnames(pred.df) <- c("x", paste0("y", 1:length(models)))
  
  ## Gather into long-form with diff rows for each predictor and each model
  pred.df.long <- tidyr::gather(pred.df, key = "model", value = "y", -"x")
  pred.df.long$x <- as.factor(pred.df.long$x) # for grp-ing in plot
  
  ## Compute mean and upper/lower bounds
  # pred.df.long <- pred.df.long %>%
  #   group_by(x) %>%
  #   summarise(mean.y = mean(y),
  #             # median.y = quantile(y, probs = 0.50),
  #             lower.y = quantile(y, probs = 0.025),
  #             upper.y= quantile(y, probs = 0.975))
  
  
  ## Create "partial" boxes (orig data) with predictions
  
  data.summ <- function(x) { # create summary for plotting as geom (crossbar)
    m <- mean(x)
    ymin <- unname(quantile(x, probs = 0.025)) # else get named number
    ymax <- unname(quantile(x, probs = 0.975)) # else get named number
    return(c(y=m,ymin=ymin,ymax=ymax))
  }
  
  # Here, just add in homegrown summary. Alt: geom_box, geom_violin
  plot <- ggplot(data = pred.df.long, aes(x = x, y = y)) + 
    stat_summary(fun.data=data.summ, 
                      geom="crossbar", width = 0.95,
                      fill = "light grey", alpha = 0.25)+
    scale_y_continuous(expand=c(0,0),
                       limits=c(0.15,0.31)) + # based on pipo ribbons for other vars
    # limits=c(0.15,0.46)) + #else ribbon for yr diff cut-off psme
    # expand_limits(x = 0) + # no: for factor, DON'T force to zero.
    # labs(x = paste0(explan.vars.names[i]),
    labs(x = NULL,
         y = "Probability of juvenile presence") +
    # coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]), # don't need with factor
    #                        max(data.brt[,explan.vars[i]]))) +
    theme_bw(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          plot.margin=unit(c(5.5, 5.5, -10, 5.5), "pt")) # shrink margins for adjacency
  # plot
  
  ## Create hist to put underneath partial curve. Use orig data.
  hist <- ggplot(data = data.brt, aes(x = as.factor(data.brt[,explan.vars[i]]))) +
    # geom_vline(xintercept = quant[1], lty = 2, lwd = 0.5, col = "red") +
    # geom_vline(xintercept = quant[2], lty = 2, lwd = 0.5, col = "red") +
    geom_histogram(stat = "count") +
    # scale_x_continuous(limits=c(min(data.brt[,explan.vars[i]]),
    #                             max(data.brt[,explan.vars[i]]))) +
    # coord_cartesian(xlim=c(min(data.brt[,explan.vars[i]]),
    #                        max(data.brt[,explan.vars[i]]))) +
    labs(x = explan.vars.names[i]) +
    theme_bw(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin=unit(c(-10, 5.5, 5.5, 5.5), "pt")) # shrink margins for adjacency
  
  
  ## To stack plots with aligned width, extract max width from each object.
  # Ref: https://stackoverflow.com/questions/36198451/specify-widths-and-heights-of-plots-with-grid-arrange
  
  plots <- list(plot, hist)
  grobs <- list()
  widths <- list()
  
  
  ## Collect the widths for each grob of each plot
  for (l in 1:length(plots)){
    grobs[[l]] <- ggplotGrob(plots[[l]])
    widths[[l]] <- grobs[[l]]$widths[2:5]
  }
  
  ## Use do.call to get the max width 
  maxwidth <- do.call(grid::unit.pmax, widths)

  ## Assign the max width to each grob
  for (l in 1:length(grobs)){
    grobs[[l]]$widths[2:5] <- as.list(maxwidth)
    grobs[[l]]$widths[2:5] <- as.list(maxwidth)
  }
  
  
  ## Plot
  png(paste0(plot.dir, explan.vars[[i]], ".png"), width = 450, height = 450, units = "px")
  # tiff(paste0(plot.dir, explan.vars[[i]], ".tiff"))
  # do.call("grid.arrange", c(grobs, ncol = 1))
  grid.arrange(grobs = grobs, ncol = 1, heights = c(3,1))
  dev.off()
  
} 
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()
dev.off()






#############################################

##PDPS BY QUANTILE###########################
#############################################

## CHANGES FOR EACH SPECIES:
# Lines 352-355: change plotting limits
## CHANGES FOR EACH VARIABLE:
# Lines 376-384: change variable used
# Lines 408-416: turn that variable off
# Lines 433-441: turn that variable on
# Lines 434 or 598: turn that variable label on
## SKIP FOR FIRE SEVERITY


par(mfrow=c(1,1))
## Which var am I varying? # Change in newdata mutate (pre-loop) & newdata transform (in loop) below
# Only need to do this for final variables retained
explan.vars
# var <- "BALive_brt_m"
# var <- "def.tc"
var <- "tmax.tc"
# var <- "ppt.tc"
# var <- "def59_z_max13"
# var <- "DUFF_DEPTH_cm"
# var <- "LITTER_DEPTH_cm"
# var <- "FIRE.SEV"
# var <- "REBURN"


# Only looking at YEAR.DIFF here
i <- 1 # explan.vars[i] still exists in loop below & 1 = YEAR.DIFF

## Create list for dumping predictors (here, all YEAR.DIFF) & responses x 10 models.
# Also create lists of each quant to contain each lists of model responses.
# 3 corresponds to the 3 quantiles (10%, 50%, 90%) I'll use
predictors<-list(rep(NA,length(models))) 
responses<-list(rep(NA,length(models)))
all.quants.predictors <- list(rep(NA,length(models)*3))
all.quants.responses <- list(rep(NA,length(models)*3))
quantile <- list(rep(NA,length(models)*3))


## For predictions, create 100 new YEAR.DIFF values.
year.new <- seq(from = min(data.brt$YEAR.DIFF), to = max(data.brt$YEAR.DIFF), by = 0.25)

## Set values of all other vars (exept that of interest) to mean; add on year.new.
print(var)
newdata <- data.brt %>% # Create new data with all but YEAR.DIFF & BALive_brt
  mutate(
    BALive_brt_m = mean(BALive_brt_m), # turn on/off var that's selected above
    def.tc = mean(def.tc),
    # tmax.tc = mean(tmax.tc),
    ppt.tc = mean(ppt.tc),
    def59_z_max13 = mean(def59_z_max13),
    DUFF_DEPTH_cm = mean(DUFF_DEPTH_cm),
    LITTER_DEPTH_cm = mean(LITTER_DEPTH_cm),
    FIRE.SEV = Mode(FIRE.SEV), # homegrown function
    REBURN = Mode(REBURN)
  )
newdata <- newdata[1:length(year.new),] # keep just enough (of all the same vals) for new years.
newdata$YEAR.DIFF <- year.new
levels(newdata$REBURN)
NY <- levels(newdata$REBURN) # To call REBURN levels without redefining var, create YN look-up

## Create new data to predict with (*n mods) for each quantile (q);
# predict with q*newdata
# Tried (0, 25, 50, 75, 100) but 0 and 100 too extreme. Stick with 10%, 50%, 90%
for (q in 1:3){ # Pick quantiles BUT MUST SPECIFY IN PROBS (10, 50 90)
  probs <- c(0.1, 0.5, 0.9)
  # for (q in 1:4){ # For 4 levels of FIRE.SEV; convenient: factor num = sev; chng tiff name below, too.
  # for (q in 1:2){ # For 2 levels of REBURN; chng tiff name below, too.
  # Loop through each of the j models
  for(j in 1:length(models)){
    gbm.mod<-models[[j]]
    r1 <- predict(gbm.mod,
                  # newdata = transform(newdata, BALive_brt_m = quantile(BALive_brt_m, probs = probs)[q]),
                  # newdata = transform(newdata, def.tc = quantile(def.tc, probs = probs)[q]),
                  newdata = transform(newdata, tmax.tc = quantile(tmax.tc, probs = probs)[q]),
                  # newdata = transform(newdata, ppt.tc = quantile(ppt.tc, probs = probs)[q]),
                  # newdata = transform(newdata, def59_z_max13 = quantile(def59_z_max13, probs = probs)[q]),
                  # newdata = transform(newdata, DUFF_DEPTH_cm = quantile(DUFF_DEPTH_cm, probs = probs)[q]),
                  # newdata = transform(newdata, LITTER_DEPTH_cm = quantile(LITTER_DEPTH_cm, probs = probs)[q]),
                  # newdata = transform(newdata, FIRE.SEV = as.factor(q)), 
                  # newdata = transform(newdata, REBURN = as.factor(NY[q])),
                  n.trees = gbm.mod$n.trees,
                  type = "response",
                  se.fit = TRUE)
    predictors[[j]] <- newdata$YEAR.DIFF 
    responses[[j]] <- r1
  }
  all.quants.predictors[[q]] <- predictors
  all.quants.responses[[q]] <- responses
  quantile[[q]] <- rep(q, length(models)) # Capture quantile num for ALL models
  
  # ymin=min(unlist(responses))
  # ymax=max(unlist(responses))
  xmin=min(unlist(predictors))
  xmax=max(unlist(predictors))
  ymin = 0.0
  ymax = 0.5
  
}  

## Prep for ggplot

# all.quants.preds/resp are lists of responses from n models within q lists
length(all.quants.responses) # 3
# all.quants.responses[[1]] # has 10 lists full of response values for quant 1

# Combine into one list, with each element containing responses
pred.temp1 <- do.call(cbind, all.quants.predictors) 
resp.temp1 <- do.call(cbind, all.quants.responses)
quant.temp <- do.call(cbind, quantile)
quant.temp <-c(quant.temp[,1:3]) # Gives quants repeated n models times

# length(resp.temp1) # 300
# length(quant.temp) # 300
# resp.temp1[[1]] # gives response values for q1, model 1
# resp.temp1[[101]] # gives response values for q2, model 1
# resp.temp1[[201]] # gives response values for q3, model 1
# resp.temp1[[301]] # doesn't exist


# Combine further into matrix
pred.temp2 <- do.call(cbind, pred.temp1) # each col reps a model (of 100) for each quant (3) - VALS IDENTICAL B/C PREDICTOR VALUES ALL THE SAME
resp.temp2 <- do.call(cbind, resp.temp1) # each col reps a model (of 100) for each quant (3) - VALS DIFF ACROSS MODELS AND QUANTS
# nrow(pred.temp2) # 117 for each predicted response
# ncol(pred.temp2) # 300 for each model (3 quants * 100 models each)

# Pull into dataframe (repeat all predictor values which are identical)
pred.df <- as.data.frame(cbind(pred.temp2[,1], resp.temp2))
# Name with quants (1-3) and each model (1-10 or 1-100)
colnames(pred.df) <- c("x", paste0("y", quant.temp, "mod",1:length(models)))

# Gather into long-form with diff rows for each predictor and each model
pred.df.long.all <- tidyr::gather(pred.df, key = "model", value = "y", -"x")

# Add col to specify quant (pulling from name is maybe not best way but whatevs)
pred.df.long.all$q <- paste0("q", mid(pred.df.long.all$model,2,1))

# Compute mean and upper/lower bounds
pred.df.long <- pred.df.long.all %>%
  group_by(x, q) %>%
  summarise(mean.y = mean(y),
            # median.y = quantile(y, probs = 0.50),
            lower.y = quantile(y, probs = 0.025),
            upper.y= quantile(y, probs = 0.975))

# Create plotting object for 95% bounds (else adding ribbon is choppy)
g1 <- ggplot(pred.df.long, aes(group=q)) + 
  stat_smooth(aes(x = x, y = lower.y), method = "loess", span = 0.25, se = FALSE) +
  stat_smooth(aes(x = x, y = upper.y), method = "loess", span = 0.25, se = FALSE)

# plot(g1)

# Build plot object for rendering 
gg1 <- ggplot_build(g1)

# Extract data for the loess lines from the 'data' slot
df.up.low <- data.frame(x = gg1$data[[1]]$x,
                        ymin = gg1$data[[1]]$y,
                        ymax = gg1$data[[2]]$y,
                        q = gg1$data[[1]]$group)

## Add 5th & 95th percentile values so we can no which year.diff range to trust (here i = YEAR.DIFF)
# (quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.05, 0.95)))
(quant <- quantile(data.brt[,explan.vars[i]], probs = c(0.1, 0.9)))


################# W/ RIBBONS - USE LINES IN CHUNK BELOW ####
## Create plot -- CHANGE LAGEND LABEL NAME!! AND CHANGE LIMITS PER SPECIES
# Create partial curve (orig data) and add in new ribbon data
# plot <- ggplot() +
#   geom_smooth(data = pred.df.long,
#               aes(x = x, y = mean.y, color = q),
#               span = 0.25,
#               se = FALSE) +
#   scale_color_manual(values = palette[3:5],
                       # name = expression(paste("Live BA (m"^"2","ha"^"-1",")")),
#                      # name = "Max deficit anomaly",
#                      # name = "Duff depth (cm)",
#                      name = expression(paste("Max temp (",degree*C,")")),
#                      labels = c(expression(paste("10"^"th"," percentile")),
#                                 expression(paste("50"^"th"," percentile")),
#                                 expression(paste("90"^"th"," percentile")))) +
#   geom_ribbon(data = df.up.low,
#               aes(x = x, ymin = ymin, ymax = ymax,
#                   group = q, fill = factor(df.up.low$q)),
#               alpha = 0.15,
#               show.legend = FALSE) +
#   scale_fill_manual(values = palette[3:5]) +
#   geom_vline(xintercept = quant[1], lty = 2, lwd = 0.5, col = "red") +
#   geom_vline(xintercept = quant[2], lty = 2, lwd = 0.5, col = "red") + 
#   # scale_x_continuous(expand=c(0,0), limits=c(0,30)) + # pipo, max year.diff is 30
#   scale_x_continuous(expand=c(0,0), limits=c(0,31)) + # psme, max year.diff is 31
#   # scale_y_continuous(expand=c(0,0), limits=c(0.14,0.36)) + # pipo
#   scale_y_continuous(expand=c(0,0), limits=c(0.09,0.61)) + # psme
#   expand_limits(x = 0) + 
#   labs(x = paste0(explan.vars.names[i]),
#        y = "Probability of juvenile presence") +
#   theme_bw(base_size = 18) +
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         legend.background = element_blank(),
#         legend.justification=c(1,0), # defines which side of legend .position coords refer to
#         legend.position=c(1,0),
#         legend.text=element_text(size=12),
#         legend.title=element_text(size=14),
#         legend.title.align=1)
#################


display.brewer.pal(8, "Dark2")

## Alt: Plot all model runs, not just min/max and mean for each quantile.
# Cannot specify multiple scale_color_manuals, so to get light saturation, add multiple geoms.
plot <- ggplot() +
  geom_smooth(data = pred.df.long.all[pred.df.long.all$q == "q1",],
              aes(x = x, y = y, group = model),
              span = 0.25,
              se = FALSE,
              # col = palette[3], alpha = 0.15, # alpha doesn't work
              col = "#D4D2EA", # 20 pts towards grey, lightened by 30% at hexcolortool.com/
              lwd = 0.25) +
  geom_smooth(data = pred.df.long.all[pred.df.long.all$q == "q2",],
              aes(x = x, y = y, group = model),
              span = 0.25,
              se = FALSE,
              # col = palette[5], alpha = 0.15, # alpha doesn't work
              col = "#B1DD7E", # this is palette[4] lightened by 30% at hexcolortool.com/
              lwd = 0.25) +
  geom_smooth(data = pred.df.long.all[pred.df.long.all$q == "q3",],
              aes(x = x, y = y, group = model),
              span = 0.25,
              se = FALSE,
              # col = palette[7], alpha = 0.15, # alpha doesn't work
              col = "#F7D596", # this is palette[5] lightened by 40% at hexcolortool.com/
              lwd = 0.25) +
  geom_smooth(data = pred.df.long,
              aes(x = x, y = mean.y, color = q),
              span = 0.25,
              se = FALSE) +
  scale_color_manual(values = palette[c(3,5,7)],
                     # name = expression(paste("Live BA (m"^"2","ha"^"-1",")")),
                     # name = "Max deficit anomaly",
                     # name = "Duff depth (cm)",
                     name = expression(paste("Max temp (",degree*C,")")),
                     labels = c(expression(paste("10"^"th"," percentile")),
                                expression(paste("50"^"th"," percentile")),
                                expression(paste("90"^"th"," percentile")))) +
  # geom_ribbon(data = df.up.low,
  #             aes(x = x, ymin = ymin, ymax = ymax,
  #                 group = q, fill = factor(df.up.low$q)),
  #             alpha = 0.15,
  #             show.legend = FALSE) +
  scale_fill_manual(values = palette[3:5]) +
  geom_vline(xintercept = quant[1], lty = 2, lwd = 0.5, col = "red") +
  geom_vline(xintercept = quant[2], lty = 2, lwd = 0.5, col = "red") + 
  # scale_x_continuous(expand=c(0,0), limits=c(0,30)) + # pipo, max year.diff is 30
  scale_x_continuous(expand=c(0,0), limits=c(0,31)) + # psme, max year.diff is 31
  # scale_y_continuous(expand=c(0,0), limits=c(0.14,0.36)) + # pipo
  scale_y_continuous(expand=c(0,0), limits=c(0.09,0.61)) + # psme
  expand_limits(x = 0) + 
  labs(x = paste0(explan.vars.names[i]),
       y = "Probability of juvenile presence") +
  theme_bw(base_size = 18) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_blank(),
        legend.justification=c(1,0), # defines which side of legend .position coords refer to
        legend.position=c(1,0),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14),
        legend.title.align=1)



plot

# Save
png(paste0(plot.dir, "yr.diff_pdp_by_",var,"_all_qs.png"), width = 450, height = 450, units = "px")
# tiff(paste0(plot.dir, "yr.diff_pdp_by_",var,"_all_qs.tif"))
# tiff(paste0(plot.dir, explan.vars[[i]], "_alt.tiff"))
print(plot) # When using ggplot in for loop, need to print.
dev.off() ; dev.off()

