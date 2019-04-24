######################################################
### SETUP
######################################################

currentDate <- Sys.Date()

## Load data; remove extraneous column if necessary
data.pipo <- read.csv("data.pipo.csv") ; data.pipo$X <- NULL

## Exclude sites w/o fire OR w/ fire.sev 5 & 6 (here NA)
data.pipo <- data.pipo[! is.na(data.pipo$FIRE.SEV) ,]

## Set variable classes
# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo,
#                                levels=unique(data.pipo$regen_pipo),
#                                ordered = TRUE)
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
data.pipo$FIRE.SEV <- factor(data.pipo$FIRE.SEV, ordered = TRUE)
data.pipo$REBURN <- factor(data.pipo$REBURN, ordered = TRUE)

# Create CMD relative chng (from observed z-score slopes)
data.pipo$CMD_CHNG <- data.pipo$def59.z.slope

# Create proportion BA
data.pipo$BAProp_pipo <- data.pipo$BALive_pipo/data.pipo$BALiveTot


######################################################
### GBM
######################################################

# install.packages("gbm")
library(gbm)

# data.pipo$regen_pipo <- factor(data.pipo$regen_pipo,
#                                levels=unique(data.pipo$regen_pipo),
#                                ordered = TRUE)

# gbm won't run if there are unused variables (columns) in the dataset
data <- data.pipo %>% dplyr::select(regen_pipo, YEAR.DIFF, BALive_pipo, BALiveTot,
                                    def.tc, tmax.tc, ppt.tc, CMD_CHNG, def59_z_13,
                                    ELEV, REBURN, FIRE.SEV)

f <- formula(regen_pipo ~ YEAR.DIFF + BALive_pipo + BALiveTot
             + def.tc + tmax.tc + ppt.tc + CMD_CHNG + def59_z_13
             + ELEV + REBURN + FIRE.SEV)
           
prelim <- gbm(f, data = data, 
              distribution = "bernoulli",
              interaction.depth = 1,
              bag.fraction = 0.5,
              train.fraction = 0.5,
              n.minobsinnode = 5,
              cv.folds = 5,
              keep.data = TRUE)

par(mfrow=c(1,1))
# Check performance using the out-of-bag (OOB) error; the OOB error typically
# underestimates the optimal number of iterations
best.iter <- gbm.perf(prelim, method = "OOB")
print(best.iter)
# Check performance using the 50% heldout test set
best.iter <- gbm.perf(prelim, method = "test")
print(best.iter)
# Check performance using 5-fold cross-validation
best.iter <- gbm.perf(prelim, method = "cv")
print(best.iter)
# Plot relative influence of each variable
par(mfrow = c(1, 2))
summary(prelim, n.trees = 1) # using first tree
summary(prelim, n.trees = best.iter) # using estimated best number of trees
# Compactly print the first and last trees for curiosity
print(pretty.gbm.tree(prelim, i.tree = 1))
print(pretty.gbm.tree(prelim, i.tree = prelim$n.trees))

# least squares error
print(sum((data2$Y - Yhat)^2))
# Construct univariate partial dependence plots
p1 <- plot(prelim, i.var = 1, n.trees = best.iter)
p2 <- plot(prelim, i.var = 2, n.trees = best.iter)
p3 <- plot(prelim, i.var = "FIRE.SEV", n.trees = best.iter) # can use index or name
grid.arrange(p1, p2, p3, ncol = 3)
# Construct bivariate partial dependence plots
plot(prelim, i.var = 1:2, n.trees = best.iter)
plot(prelim, i.var = c("BALive_pipo", "YEAR.DIFF"), n.trees = best.iter)
plot(prelim)
# Construct trivariate partial dependence plots
plot(prelim, i.var = c(1, 2, 6), n.trees = best.iter,
     continuous.resolution = 20)
plot(prelim, i.var = 1:3, n.trees = best.iter)
plot(prelim, i.var = 2:4, n.trees = best.iter)
plot(prelim, i.var = 3:5, n.trees = best.iter)



# may abort unless numeric is used not factor
# error with duplicate levels may also have to do with having two few factor levels represented in a train/test set
# https://stackoverflow.com/questions/38931194/warning-when-defining-factor-duplicated-levels-in-factors-are-deprecated