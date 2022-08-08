# Climotology ------------------------------------------------------------------
# Loading packages and all prewriten functions
setwd("C:\\Users\\james\\Desktop\\jamieslife\\analysis")
source("../R/mypackage.R")
rasterOptions(maxmemory = 100e+10)

# Loading raw .nc files from daily nc. files
# Loading raw .nc files from daily nc. files
chl1 <- loadnc(sdir = "C:\\Users\\james\\Desktop\\jamieslife\\data\\outfiles\\",
               patt = "CHLr_day.nc",
               var1 = "time", 
               var2 = "value",
               var3 = "lats", 
               var4 = "lons",
               alot = FALSE,
               t    = FALSE,
               origin = 1970)

stl1 <- loadnc(sdir = "C:\\Users\\james\\Desktop\\jamieslife\\data\\outfiles\\",
               patt = "CHLf_day.nc",
               var1 = "time", 
               var2 = "value",
               var3 = "lats", 
               var4 = "lons",
               alot = FALSE,
               t    = FALSE,
               origin = 1970)

# crop a 3X4 pixel (4X3?) square arounf station aloha
aloha <- extent(-159, -157, 22, 23)
north <- extent(-163, -132, 27, 34.5)

chl <- raster::crop(chl1, aloha)
#stl <- raster::crop(stl1, aloha)
stl <- raster::crop(chl1, north)


c_t <- getZ(chl)
s_t <- getZ(stl)
c_u <- cellStats(chl, stat = mean, na.rm = TRUE)
s_u <- cellStats(stl, stat = mean, na.rm = TRUE)

box_df <- data.frame(c_t, s_t, c_u, s_u)
colnames(box_df) <- c("time_c", "time_s", "mean_c", "mean_s")
box_df$months <- month(box_df$time_c)

north <- data.frame(box_df$mean_c, box_df$months)
north$ID <- "north"
colnames(north) <- c("chl", "months", "ID")
aloha <- data.frame(box_df$mean_s, box_df$months)
aloha$ID <- "aloha"
colnames(aloha) <- c("chl", "months", "ID")
head(aloha)
box_df <- rbind(aloha, north)
box_df <- box_df[order(box_df$months),]
box_df$ID <- as.factor(box_df$ID)

#thresh <- 0
#raster::values(stl)[raster::values(stl) < thresh] <- NA
# Pre-written function to find the satellite climatology
chl_clim <- bloomfreq(chl, func = "mean")
chl_sd   <- bloomfreq(chl, func = "sd")
chl_clim$sat_err <- chl_sd$chl
# doing it for stl filtered chl
stl_clim <- bloomfreq(stl, func = "mean")
stl_sd   <- bloomfreq(stl, func = "sd")
stl_clim$sat_err <- stl_sd$chl

# Load the HPLC Data text file from HOT_DOGS webserver
hplc5m <- data.frame(fread("..//data//infiles//hotdogs//HPLC_HOT.txt", 
                           colClasses = c(rep("character", 6)), 
                           header=TRUE),
                     stringsAsFactors = FALSE)
# convert the odd datetype to Postix
date   <- as.POSIXct(hplc5m$date, format="%m%d%y")
hplc5m <- data.frame(apply(hplc5m[,c("press", "hplc")], 
                           MARGIN=2, 
                           FUN=as.numeric))
hplc5m      <- cbind(hplc5m, date) # create a dataframe
hplc5m$hplc <- hplc5m$hplc/1000 # convert ng to mg
colnames(hplc5m) <- c("press", "chl", "time")
hplc5m$time <- as.Date(hplc5m$time) # that Postix is now a date class
hplc        <- hplc5m[which(hplc5m$press < 7), ] # We only want the upper 5m
# Remove old data dates
ind   <- which(hplc$time > as.Date("2002-01-01"))
hplc  <- hplc[ind,] 
#
hplc$month <- months(hplc$time)
hplc_clim  <- aggregate(chl ~ month, data = hplc, FUN = mean, na.rm= TRUE)
hplc_sd    <- aggregate(chl ~ month, data = hplc, FUN = sd, na.rm= TRUE)
hplc_clim$hp_err <- hplc_sd$chl

# Load the HPLC Data text file from HOT_DOGS webserver
poc5m <- data.frame(fread("..//data//infiles//hotdogs//poc_hot.txt", 
                          colClasses = c(rep("character", 6)), 
                          header=TRUE),
                    stringsAsFactors = FALSE)
# convert the odd datetype to Postix
date   <- as.POSIXct(poc5m$date, format="%m%d%y")
poc5m <- data.frame(apply(poc5m[,c("press", "pc")], 
                          MARGIN=2, 
                          FUN=as.numeric))
poc5m      <- cbind(poc5m, date) # create a dataframe
colnames(poc5m) <- c("press", "poc", "time")
poc5m$time <- as.Date(poc5m$time) # that Postix is now a date class
poc5m        <- poc5m[which(poc5m$press < 7), ] # We only want the upper 5m
# Remove old data dates
ind   <- which(poc5m$time > as.Date("2002-01-01"))
poc5m  <- poc5m[ind,] 
#
poc5m$month <- months(poc5m$time)
poc_bot_clim  <- aggregate(poc ~ month, data = poc5m, FUN = mean, na.rm= TRUE)
poc_bot_sd    <- aggregate(poc ~ month, data = poc5m, FUN = sd, na.rm= TRUE)
poc_bot_clim$poc_bot_err <- poc_bot_sd$poc

aloha_clim <- merge(chl_clim, hplc_clim, by = "month", sort = FALSE)

aloha_clim <- merge(aloha_clim, stl_clim, by = "month", sort = FALSE)

aloha_clim <- merge(aloha_clim, poc_bot_clim, by = "month", sort = FALSE)

colnames(aloha_clim) <- c("months", "sat_chl", "sat_err", "hp_chl", "hp_err", 
                          "stl_sat", "stl_err", "poc_bot", "poc_err")
m <- as.Date("1900-01-01") + months(1:12-1)
dt <- aloha_clim

#this may be one of the worst scripts I've writen...

# Set work directory and load packages/funtions
setwd("C:\\Users\\james\\Desktop\\jamieslife\\analysis")
source("../R/mypackage.R")
rasterOptions(maxmemory = 120e+10, memfrac = 0.9)
library(car)
library(rjags)
library(R2jags)

# Path of the raw data
ifile <- "../data/infiles/"
ofile <- "../data/outfiles/"
# Loading raw .nc files from daily nc. files
chl <- loadnc(sdir = paste(ifile, 
                           "chl\\ftp.hermes.acri.fr\\941251190\\", sep=""),
              patt = "00.nc",
              var1 = "time", 
              var2 = "CHL1_mean",
              var3 = "lat", 
              var4 = "lon",
              alot = TRUE,
              t    = TRUE)

hplc <- hotdata(file = "HPLC_CHLa_20m_Bottle_HOT_20220111.txt", 
                id = c("press", "hplc"),
                cutdate = min(getZ(chl)),
                depth = 30)
hplc$hplc <- hplc$hplc / 1000

se <- function(x) sqrt(var(x, na.rm=TRUE)/length(x))

# Finding the average of the upper 25m
value    <- aggregate(hplc$hplc, by = list(hplc$time), FUN=mean) 
err_hplc <- aggregate(hplc$hplc, by = list(hplc$time), FUN=se)
hplc     <- aggregate(hplc$press, by = list(hplc$time), FUN=mean)

hplc$hplc      <- value$x 
hplc$hplc_err  <- err_hplc$x
colnames(hplc) <- c("time", "press", "hplc", "err_hplc")
head(hplc)

# crop a 3X4 pixel (4X3?) square arounf station aloha
aloha <- extent(-158, -157.8, 22.65, 22.8)
aloha <- raster::crop(chl, aloha)
time  <- getZ(aloha)

u <- vector()
err <- vector()
for (i in 1:nrow(hplc)) {
  ind  <- which.min(abs(hplc$time[i] - time))
  temp <- subset(aloha, (ind-1):(ind+1))
  #temp <- subset(aloha, ind) # for day of comparison
  u[i] <- mean(raster::values(temp), na.rm = TRUE)
  err[i] <- se(as.vector((raster::values(temp))))
}

hplc$chl_sat <- u
hplc$err_sat <- err
head(hplc)

calib <- hplc
colnames(calib) <- c("time", "press", "chl_hplc", "err_hplc", "chl_sat", 
                     "err_sat")
calib <- subset(calib, !is.na(chl_sat))
head(calib)

# ------------------------------------------------------------------------------
dprof <- hotdata(file = "HPLC_CHLa_20m_Bottle_HOT_20220111.txt", 
                id = c("press", "hplc"),
                #cutdate = min(getZ(chl)),
                depth = 500)
dprof$hplc <- dprof$hplc/1000

temp <- unique(dprof$time)
idx <- which(dprof$time == sample(temp, 1))
idx <-  c(1087, 1088, 1089, 1090, 1091, 1092, 1093, 1094, 1095, 1096, 1097, 
          1098)
dprof <- dprof[idx, ]
# saved some I like
#idx <- c(610, 611, 612, 613, 614, 615, 616, 617, 618, 619, 620, 621)
#idx <-  979 980 981 982 983 984 985 986 987 988 989 990
# 
# par(mar = c(3, 3, 5, 5))
# plot(dprof$hplc, dprof$pres, 
#      ylim = c(200, 0), 
#      axes = FALSE, 
#      ylab = "", 
#      xlab = "",
#      pch = 19)
# abline(h = 25, col = colvect("firebrick", alpha = 0.5), lwd = 2)
# grid(nx = 4, # X-axis divided in two sections
#      ny = 6, # Y-axis divided in three sections
#      lty = 2, col = colvect(c("gray69"), alpha = 1), lwd = 1)
# axis(side = 3, las = 1, lwd = 2, mgp = c(1, 0.75, 0), cex.axis = 1.5)
# axis(side = 4, las = 2, lwd = 2, mgp = c(1, 0.75, 0), cex.axis = 1.5)
# box(which = "plot", lty = "solid", lwd = 2, col = "grey25")
# # title(ylab = "Pressure [db]", 
# #       cex.lab = 1.5,
# #       line = 1.5)
# # title(xlab = "CHL [mg/m^3]", 
# #       cex.lab = 1.5,
# #       line = 1.5)
# mtext("CHL [mg/m^3]", side=3, line=2.5, cex = 1.5)
# corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
# par(xpd = TRUE) #Draw outside plot area
# text(x = corners[2]+0.06, y = mean(corners[3:4]), "Pressure [db]", srt = 270, cex = 1.5)

# GLM ussing Gamma Log transform x ---------------------------------------------

library(MASS)
library(ggeffects)
x <- calib$chl_hplc
y <- calib$chl_sat
data <- data.frame(y, x)  #dataset
data <- within(data, {cx <- as.numeric(scale(x, scale = FALSE))})

#glmGamma <- glm(y ~ log(x + 1), data = data, family = Gamma(link = "identity"))
glmGamma <- glm(y ~ x, data = data, family = Gamma(link = "identity"))
glm_0 <- glm(y ~ 1, data = data, family = Gamma(link = "identity"))
#glmGamma <- glm(y ~ x, data = data, family = Gamma(link = "identity"))

model1 <- summary(glmGamma)

anova(glmGamma, glm_0, test="F")

m  <- round(model1$coefficients[2], 3)
b  <- round(model1$coefficients[1], 3)
r2 <- round(1 - model1$deviance/model1$null.deviance, 3)

myshape  <- gamma.shape(glmGamma)
gampred  <- predict(glmGamma , type = "response", se = TRUE, 
                    dispersion = 1/myshape$alpha) 

data$fit <- unname(gampred$fit)

# Box plots of ALOHA and 30N and Climate OFFICIAL ------------------------------

layout( matrix(c(1,
                 2),
               ncol = 1,
               nrow = 2, 
               byrow = TRUE),
        heights = c(1.85, 1))

plotit(x = x, y = y, abline = c(m, b, r2),
       xlab = "HPLC Bottle Samples [mg/L]",
       ylab = "GlobColor Daily GSM CHL1 L3 [mg/m^3]",
       # sub = "Comparison of HPLC and GlobColour GSM CHL",
       sub = "a)",
       ylim = c(0, 0.18),
       xlim = c(0.005, 0.22),
       line = -1.1,
       adj = 0.05,
       mar = c(5, 5, 4, 3))
title("HPLC to CHLsat Comparison and CHL Seasonality", line = 1, adj = 0, cex.main = 1.5)

boxit(x2 = box_df$months,
      x1 = box_df$ID,
      col = colvect(c("ivory4", "grey22"), alpha = c(0.5,0.9)),
      add = FALSE,
      y = box_df$chl,
      ylim = c(0.035, 0.2),
      legend = c("30N", "ALOHA"),
      data = box_df,
      labels = as.character(1:12),
      at = seq(from = 1, to = 24, by = 2) + 0.5,
      #sub = "30N and St. ALOHA Climatology",
      sub = "b)",
      xlab = "Month of year",
      ylab = "Mean CHL [mg/m^3]",
      line = -1.1,
      adj = 0.05,
      mar = c(5, 5, 0, 3))
# ------------------------------------------------------------------------------
layout( matrix(c(1, 3,
                 2, 3),
               ncol = 2,
               nrow = 2, 
               byrow = TRUE),
        heights = c(1.85, 1),
        widths = c(1, 0.75))

plotit(x = x, y = y, abline = c(m, b, r2),
       xlab = "HPLC Bottle Samples [mg/L]",
       ylab = expression(GlobColor~Daily~GSM~CHL1~L3~"["~mg/m^3~"]"),
       # sub = "Comparison of HPLC and GlobColour GSM CHL",
       sub = "a)",
       ylim = c(0, 0.18),
       xlim = c(0.005, 0.22),
       line = -1.1,
       adj = 0.05,
       mar = c(5, 5.5, 4, 1))
#title("HPLC to CHLsat Comparison and CHL Seasonality", line = 1, adj = 0, cex.main = 1.5)

boxit(x2 = box_df$months,
      x1 = box_df$ID,
      col = colvect(c("ivory4", "grey22"), alpha = c(0.5,0.9)),
      add = FALSE,
      y = box_df$chl,
      ylim = c(0.035, 0.2),
      legend = c("30N", "ALOHA"),
      data = box_df,
      labels = as.character(1:12),
      at = seq(from = 1, to = 24, by = 2) + 0.5,
      #sub = "30N and St. ALOHA Climatology",
      sub = "b)",
      xlab = "Month of year",
      ylab = expression(Mean~CHL~"["~mg/m^3~"]"),
      line = -1.1,
      adj = 0.05,
      mar = c(5, 5.5, 0, 1))

par(mar = c(5, 1, 4, 5))
plot(dprof$hplc, dprof$pres, 
     ylim = c(200, 0), 
     axes = FALSE, 
     ylab = "", 
     xlab = "",
     pch = 19)
lines(dprof$hplc, dprof$pres, col = colvect(c("gray25", alpha = 0.5)), lwd = 2)
abline(h = 25, col = colvect("firebrick", alpha = 0.5), lwd = 2)
grid(nx = 4, # X-axis divided in two sections
     ny = 6, # Y-axis divided in three sections
     lty = 2, col = colvect(c("gray69"), alpha = 1), lwd = 1)
axis(side = 1, las = 1, lwd = 2, mgp = c(1, 0.75, 0), cex.axis = 1.5)
axis(side = 4, las = 2, lwd = 2, mgp = c(1, 0.75, 0), cex.axis = 1.5)
box(which = "plot", lty = "solid", lwd = 3, col = "grey25")
# title(ylab = "Pressure [db]", 
#       cex.lab = 1.5,
#       line = 1.5)
title(xlab = expression(CHL~"["~mg/m^3~"]"),
      cex.lab = 1.5,
      line = 2.5)
#mtext("CHL [mg/m^3]", side=3, line=2.5, cex = 1.5)
corners = par("usr") #Gets the four corners of plot area (x1, x2, y1, y2)
par(xpd = TRUE) #Draw outside plot area
text(x = corners[2]+0.05, 
     y = mean(corners[3:4]), "Pressure [decibar]", 
     srt = 270, 
     cex = 1.5)
title(main = list("c)", cex = 1.5),
      line= -1.1,
      adj = 0.05)



































