## Read main data sets

malaria <- read.csv('dat_izzy_May15.csv')
annual_data <- read.csv('mdg_ocha4_ver5.csv')
id_match <- read.csv('fktIDs.csv')

p_monthly <- read.csv('p_tall.csv')
tmax_monthly <- read.csv('tMax_tall.csv')
tmin_monthly <- read.csv('tMin_tall.csv')
coordinates <- read.csv('coordinates.csv')

alt <- read.csv('altitude.bf.csv')
bednets <- read.csv('bednets.bs.bf.csv')
yfl <- read.csv('yfl.rec.csv')
tc <- read.csv('tc.bf.csv')
fcl <- read.csv('fcl.bf.csv')

malaria <- malaria %>% arrange(month)
malaria <- malaria %>% arrange(year)
coordinates$ID <- 0:194

# Add Fokontany name to malaria dataset

for (i in 0:194){
  
  malaria$fokontany[malaria$ID == i] <- id_match$Fokontany[id_match$ID == i]
  
}

test <- grep("Antaralava", malaria$fokontany,ignore.case=TRUE)

# Select Ifanadiana district

ifa_data <- annual_data[annual_data$ADM2_EN == 'Ifanadiana',]

# Remove useless rows
ifa_data$ADM0_PCODE <- NULL
ifa_data$ADM0_EN <- NULL
ifa_data$ADM1_PCODE <- NULL
ifa_data$ADM1_EN <- NULL
ifa_data$ADM1_TYPE <- NULL
ifa_data$OLD_PROVIN <- NULL
ifa_data$PROV_TYPE <- NULL
ifa_data$NOTES <- NULL
ifa_data$SOURCE <- NULL

# Identify duplicates 
n_occur <- data.frame(table(ifa_data$ADM4_EN))
fok_duplicate <- n_occur[n_occur$Freq > 1,]

malaria$altitude <- 0
malaria$nets.high <- 0
malaria$nets.low <- 0
malaria$yfl <- 0
malaria$tc <- 0
malaria$fcl <- 0

malaria$maj.veg <- 0
malaria$min.veg <- 0
malaria$med.veg <- 0
malaria$mean.ann.temp <- 0
malaria$min.ann.temp <- 0
malaria$max.ann.temp <- 0
malaria$mean.ann.p <- 0
malaria$wet.p <- 0
malaria$dry.p <- 0

malaria$monthly.p <- 0
malaria$monthly.max.t <- 0
malaria$monthly.min.t <- 0

malaria$monthly.p.lag <- 0
malaria$monthly.max.t.lag <- 0
malaria$monthly.min.t.lag <- 0

malaria$lat <- 0
malaria$long <- 0
malaria$x_32738 <- 0
malaria$y_32738 <- 0


# Create lagged monthly data (one month lag)

p.lagged <- c(p_monthly$Precipitation[1], p_monthly$Precipitation[-nrow(p_monthly)])
p_monthly$p.lag <- p.lagged

tmax.lagged <- c(tmax_monthly$maxT[1], tmax_monthly$maxT[-nrow(tmax_monthly)])
tmax_monthly$maxT.lag <- tmax.lagged

tmin.lagged <- c(tmin_monthly$minT[1], tmin_monthly$minT[-nrow(tmin_monthly)])
tmin_monthly$minT.lag <- tmin.lagged


## Fill malaria dataset

for (i in 0:194){
   
  malaria$altitude[malaria$ID == i] <- alt$alt_bf[alt$ID == i]
  malaria$nets.high[malaria$ID == i] <- bednets$highseason[bednets$ID == i]
  malaria$nets.low[malaria$ID == i] <- bednets$lowseason[bednets$ID == i]
  malaria$yfl[malaria$ID == i] <- yfl$yfl.rec[yfl$ID == i]
  malaria$tc[malaria$ID == i] <- tc$tc_bf[tc$ID == i]
  malaria$fcl[malaria$ID == i] <- fcl$fcl.bf[fcl$ID == i]
  
  # Monthly temperature and precipitation
  malaria$monthly.p[malaria$ID == i] <- p_monthly$Precipitation[p_monthly$OBJEC == i]
  malaria$monthly.max.t[malaria$ID == i] <- tmax_monthly$maxT[tmax_monthly$OBJEC == i]
  malaria$monthly.min.t[malaria$ID == i] <- tmin_monthly$minT[tmin_monthly$OBJEC == i]
  
  # Lagged monthly temperature and precipitation
  malaria$monthly.p.lag[malaria$ID == i] <- p_monthly$p.lag[p_monthly$OBJEC == i]
  malaria$monthly.max.t.lag[malaria$ID == i] <- tmax_monthly$maxT.lag[tmax_monthly$OBJEC == i]
  malaria$monthly.min.t.lag[malaria$ID == i] <- tmin_monthly$minT.lag[tmin_monthly$OBJEC == i]
  
  # Coordinates
  malaria$lat[malaria$ID == i] <- coordinates$lat[coordinates$ID == i]
  malaria$long[malaria$ID == i] <- coordinates$long[coordinates$ID == i]
  malaria$x_32738[malaria$ID == i] <- coordinates$x_32738[coordinates$ID == i]
  malaria$y_32738[malaria$ID == i] <- coordinates$y_32738[coordinates$ID == i]
  
}


for (j in unique(malaria$fokontany)) {
  
  f <- grep(j, ifa_data$ADM4_EN,ignore.case=TRUE)
  
  malaria$maj.veg[malaria$fokontany == j] <- ifa_data$MAJORITYVEG[f][1]
  malaria$min.veg[malaria$fokontany == j] <- ifa_data$MINORITYVEG[f][1]
  malaria$med.veg[malaria$fokontany == j] <- ifa_data$MEDIAN_VEG_NAME[f][1]
  malaria$mean.ann.temp[malaria$fokontany == j] <- mean(ifa_data$ANN_MEAN_T[f])
  malaria$min.ann.temp[malaria$fokontany == j] <- mean(ifa_data$MEAN_MIN_T[f])
  malaria$max.ann.temp[malaria$fokontany == j] <- mean(ifa_data$MEAN_MAX_T[f])
  malaria$mean.ann.p[malaria$fokontany == j] <- mean(ifa_data$ANN_MEAN_P[f])
  malaria$wet.p[malaria$fokontany == j] <- mean(ifa_data$WET_MON_P[f])
  malaria$dry.p[malaria$fokontany == j] <- mean(ifa_data$DRY_MON_P[f])
  
}


write.csv(malaria, 'malaria_predictors.csv')

