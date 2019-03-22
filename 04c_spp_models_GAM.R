#################################################3
## Is there an asymptote? Check with GAM
data.pipo$regen_pipo <- as.numeric(as.character(data.pipo$regen_pipo))
gam.test <- gam(regen_pipo ~ s(YEAR.DIFF, k=10) , data=data.pipo, family = "binomial")
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
seup <- (predRegen$fit + 1.96 * predRegen$se.fit) # to project to ha
sedwn <- (predRegen$fit - 1.96 * predRegen$se.fit) # to project to ha
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
