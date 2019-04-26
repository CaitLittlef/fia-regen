## Running LOESS on LOESS curves of each bootstrap for each variable for each drought type.
# Would have rather averaged all bootstraps for each variable, but gbm.plot uses lotsa different intervals.
# So, I've combined all loess predicted values into a single dataframe (each var, each drought type)...
# ... and and running another loess on all those values (20000 values for 20 bootstraps x 100 gbm.plot predictions x 10 var)


# Load up loess (smoothed) predictors for each drought type; remove auto-assigned unique ID (X)
pipo.mod <- read.csv(paste0(out.dir,"pipo_brt_lo.pred_z1_noELEV_noREBURN_noFIRESEV_2019-04-25.csv")) %>% dplyr::select(-X)
# for.sev <- read.csv(paste0(outdir,"/S_sev_forest_n10000_LR0.01_TC5_LO.PRED_noEVInoC_2018-10-28.csv")) %>% dplyr::select(-X)
# stp.mod <- read.csv(paste0(outdir,"/S_mod_steppe_n10000_LR0.01_TC5_LO.PRED_noEVInoC_2018-10-28.csv")) %>% dplyr::select(-X)
# stp.sev <- read.csv(paste0(outdir,"/S_sev_steppe_n10000_LR0.01_TC5_LO.PRED_noEVInoC_2018-10-28.csv")) %>% dplyr::select(-X)

# Dump all these dataframes into a single list to reference in loop.
list <- list(pipo.mod) #, for.sev, stp.mod, stp.sev)

## Create folder for plots
currentDate <- Sys.Date()
dir.create(paste0(out.dir, "pipo_brt_lo.pred_z1_noELEV_noREBURN_noFIRESEV_", currentDate))
plot.dir <- paste0(out.dir, "pipo_brt_lo.pred_z1_noELEV_noREBURN_noFIRESEV_", currentDate)
dev.off()

## Loop through each variable and, within each, all four drought types -- that's loess on loess of all 20 bootstraps.
# N.b., keeping drought separate plotting separately so I can change colors/lines.
# N.b., need to arrange variables each time or else smoothing is out of order/scribbled.
for(i in 1:(length(explan.vars) - 2)){ # b/c fire sev & reburn (factors) aren't in here
  
  ## For plot axis, need min & max across all 4 drought types and all variables.
  xmins <- list()
  xmaxs <- list()
  ymins <- list()
  ymaxs <- list()
  
  ## Capture min/max axis values for variable i across ALL 4 drought types. Slice selects rows by position.
    # for(j in c(1:4)){
    for(j in c(1:1)){  
    xmins[[j]] <- list[[j]] %>% filter(var == explan.vars[i]) %>% slice(which.min(x))
    xmaxs[[j]] <- list[[j]] %>% filter(var == explan.vars[i]) %>% slice(which.max(x)) 
    ymins[[j]] <- list[[j]] %>% filter(var == explan.vars[i]) %>% slice(which.min(y)) 
    ymaxs[[j]] <- list[[j]] %>% filter(var == explan.vars[i]) %>% slice(which.max(y))
    }
  ## Pull out the min and max values across all models for variable i (this is messy but whatevs)
  xmin <- min(xmins[[1]]$x)#, xmins[[2]]$x, xmins[[3]]$x, xmins[[4]]$x)
  xmax <- max(xmaxs[[1]]$x)#, xmaxs[[2]]$x, xmaxs[[3]]$x, xmaxs[[4]]$x)
  ymin <- min(ymins[[1]]$y)#, ymins[[2]]$y, ymins[[3]]$y, ymins[[4]]$y)
  ymax <- max(ymaxs[[1]]$y)#, ymaxs[[2]]$y, ymaxs[[3]]$y, ymaxs[[4]]$y)
  
  ## Establish empty pdf to successively add drought type functions to
  # pdf(paste0(plotdir, "/", explan.vars[[i]], ".pdf"))
  # png(paste0(plotdir, "/", explan.vars[[i]], ".png"))
  tiff(paste0(plot.dir, "/", explan.vars[[i]], ".tif"))
  par(mar=c(5.5,5.1,4.1,2.1))
  
  ## Drought type 1, var i
  # temp.lo <- list[[1]]
  # temp.lo <- temp.lo %>% filter(var == explan.vars[i]) %>% arrange(x) 
  # temp.lo.lo <- loess(temp.lo$y ~ temp.lo$x, span = 0.5) 
  # plot(temp.lo.lo$x, temp.lo.lo$fitted, col = "green", lty=2, type='l',
  #      lwd=2, xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))

  ## Drought type 2, var i
  # par(new=TRUE)
  # temp.lo <- list[[2]]
  # temp.lo <- temp.lo %>% filter(var == explan.vars[i]) %>% arrange(x) 
  # temp.lo.lo <- loess(temp.lo$y ~ temp.lo$x, span = 0.5) 
  # plot(temp.lo.lo$x, temp.lo.lo$fitted, col = "green", lty=1, type='l',
  #      lwd=2, xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  
  ## Drought type 3, var i
  # par(new=TRUE)
  # temp.lo <- list[[3]]
  # temp.lo <- temp.lo %>% filter(var == explan.vars[i]) %>% arrange(x) 
  # temp.lo.lo <- loess(temp.lo$y ~ temp.lo$x, span = 0.5) 
  # plot(temp.lo.lo$x, temp.lo.lo$fitted, col = "brown", lty=2, type='l',
  #      lwd=2, xlab="",ylab="",xaxt='n',yaxt='n',ylim=c(ymin,ymax),xlim=c(xmin,xmax))
  
  ## Drought type 4, var i; add labels on this last one
  # par(new=TRUE)
  temp.lo <- list[[1]]
  temp.lo <- temp.lo %>% filter(var == explan.vars[i]) %>% arrange(x) 
  temp.lo.lo <- loess(temp.lo$y ~ temp.lo$x, span = 0.5) 
  plot(temp.lo.lo$x, temp.lo.lo$fitted, col = "green", lty=1, type='l',
       lwd=2, ylab="Smoothed fitted functions", xlab=explan.vars[i], ylim=c(ymin,ymax),xlim=c(xmin,xmax),
       main="", font.lab=1, font.axis=1, cex.lab=1.8, cex.axis=1.5)
  
  abline(a=0,b=0,lty=1)
  dev.off()
}


# Create the legend
# png(paste0(plotdir,"/legend.png"))
pdf(paste0(plotdir,"/legend.pdf"))
par(mar=c(1,1,1,1))
plot(1, type="n", axes=FALSE, xlab="", ylab="")
legend(x="topleft", bty="n", c("forest sensitivity to moderate drought", "forest sensitivity to severe drought",  "steppe sensitivity to moderate drought", "steppe sensitivity to severe drought"), 
       lty=c(2,1,2,1), lwd=2, ncol=1, cex=1,
       col=c("green", "green", "brown", "brown"))  
dev.off()



















  