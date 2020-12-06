
# Putting covariates together

#### HOUSEHOLD WEALTH - COHORT ####

    # bring in data put together by Andres:
    setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/")
    list.files()
    
    load("Transformed_wealth_scores.rdata") 
    head(my.wscore.2014)
    head(my.wscore.2016)
    head(my.wscore.2018)
    
    # these files use IDHH, which is not in any of my other databases. i need to add it.
    
    # THE NEXT TIME YOU GO THROUGH THIS, FLIP THE SIGN!
    
    my.wscore.2014$my.wscore <- -my.wscore.2014$my.wscore
    my.wscore.2016$my.wscore <- -my.wscore.2016$my.wscore
    my.wscore.2018$my.wscore <- -my.wscore.2018$my.wscore
    
        setwd("/Users/Elizabeth/Documents/PIVOT_R/2014")
        pvshh012 <- read.dta('pvshh012.dta')  # 2014 household data 
        pvshh012$IDHH <- pvshh012$qhgrappe*100 + pvshh012$qhmen ; names(pvshh012)
        hh.14 <- dplyr::select(pvshh012, qhgrappe, qhmen, qhmois, qhannee, IDHH) ; names(hh.14)
        hh.14.w <- merge(my.wscore.2014, hh.14, by = c("IDHH")) ; head(hh.14.w) # merge wealth data with IDHH
        
        setwd("/Users/Elizabeth/Documents/PIVOT_R/2016")
        pvshh022 <- read.dta('pvshh022.dta')  # 2016 household data 
        pvshh022$IDHH <- pvshh022$qhgrappe*100 + pvshh022$qhmen ; names(pvshh022)
        hh.16 <- dplyr::select(pvshh022, qhgrappe, qhmen, qhmois, qhannee, IDHH) ; names(hh.16)
        hh.16.w <- merge(my.wscore.2016, hh.16, by = c("IDHH")) ; head(hh.16.w) # merge wealth data with IDHH
        
        # bring in child data & calculate IDHH
        setwd("/Users/Elizabeth/Documents/PIVOT_R/2018")
        pvshh032 <- read.dta('MENAGE.dta')    # 2018 household data 
        colnames(pvshh032) <- tolower(colnames(pvshh032)) # put column names in lowercase
        pvshh032$IDHH <- pvshh032$qhgrappe*100 + pvshh032$qhmen ; names(pvshh032)
        hh.18 <- dplyr::select(pvshh032, qhgrappe, qhmen, qhmois, qhannee, IDHH) ; names(hh.18)
        hh.18.w <- merge(my.wscore.2018, hh.18, by = c("IDHH")) ; head(hh.18.w) # merge wealth data with IDHH
    
    # let's start by finding the average wscore by cluster
       
        hh.14.w$qhgrappe <- as.factor(hh.14.w$qhgrappe)
        hh.14.w.bc <- tapply(hh.14.w$my.wscore, hh.14.w$qhgrappe, FUN = mean) 
        hh.14.w.bc <- as.data.frame(hh.14.w.bc) ; names(hh.14.w.bc) <- c("wscore.bc")
        hh.14.w.bc$cluster <- c(1:80) ; head(hh.14.w.bc)
        
        hh.16.w$qhgrappe <- as.factor(hh.16.w$qhgrappe)
        hh.16.w.bc <- tapply(hh.16.w$my.wscore, hh.16.w$qhgrappe, FUN = mean) 
        hh.16.w.bc <- as.data.frame(hh.16.w.bc) ; names(hh.16.w.bc) <- c("wscore.bc")
        hh.16.w.bc$cluster <- c(1:80) ; head(hh.16.w.bc)
        
        hh.18.w$qhgrappe <- as.factor(hh.18.w$qhgrappe)
        hh.18.w.bc <- tapply(hh.18.w$my.wscore, hh.18.w$qhgrappe, FUN = mean) 
        hh.18.w.bc <- as.data.frame(hh.18.w.bc) ; names(hh.18.w.bc) <- c("wscore.bc")
        hh.18.w.bc$cluster <- c(1:80) ; head(hh.18.w.bc)
        
        # plot wealth by cluster
            
            dat <- hh.14.w.bc
            dat <- hh.16.w.bc
            dat <- hh.18.w.bc
            
            my.title <- paste("Average wscore by cluster: hh 2018")
      
            # load data
            fokontany = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Administrative_boundaries/Limite_FKT_Distr_Ifanadiana.shp")
            healthcare = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Health_system/Centre-de-sante_Ifanadiana.shp")
            villages = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Villages/Villages_baseline.shp")
            roads = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Roads_proj/Ifanadiana_roads_projected.shp")
            
            # make cluster_access_loc_sp using dat:
            temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
            groups = group_by(temp.df, cluster)
            cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
            cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
            cluster.centroid = data.frame(cluster.long,cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
            cluster_access_loc = merge(cluster.centroid, dat, by = "cluster")
            xy = cluster.centroid[,c(2,3)]
            cluster_access_loc_sp <- SpatialPointsDataFrame(coords = xy, data = cluster_access_loc)
            class(cluster_access_loc_sp) # spatial points data frame
            head(cluster_access_loc_sp@data)
            
            my.grid = F.grid(fokontany)
            my.grid.coords <- my.grid$grid.coords 
            my.grid.sp <- my.grid$grid.sp 
            par(mar=c(0,0,0,0))
            # plot(my.grid.sp) 
            
            m.gstat <- gstat(formula = wscore.bc ~ 1, locations = ~Longitude  + Latitude,	data = cluster_access_loc_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance power
            
            # Plotting GSTAT
            par(mfrow=c(1,1))
            z=data.frame(predict(m.gstat, my.grid.coords))
            coordinates(z) <- c("Longitude", "Latitude") 
            gridded(z) <- TRUE
            brks = seq(-2,5,.001) ; range(z@data$var1.pred) # to help set the scale
            my.plot = plot(z, col = rev(terrain.colors(length(brks)-1)) , breaks = brks) # if you want to set the scale
            #my.plot=image(z, col=rev(terrain.colors(15))) # if you don't care about the scale
            plot(fokontany, add = TRUE)
            #add.elements(my.plot)
            title(my.title, line = -1, adj = 0.1)
            
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/PCA") # MAKE SURE THIS IS RIGHT
            dev.copy(pdf,  paste0(my.title, ".pdf"))
            dev.off()
      
      # 1D interpolate 3 points of normalized wealth by cluster: children
            
            # to simplify, let's just use the names I made before, even though they're not great.
            
            ch14.se.bc <- hh.14.w.bc
            ch16.se.bc <- hh.16.w.bc
            ch18.se.bc <- hh.18.w.bc
            
            ch14.se.bc$date <- as.Date("2014-04-01")
            ch16.se.bc$date <- as.Date("2016-08-01")
            ch18.se.bc$date <- as.Date("2018-05-01")
            
            ch.se.all <- rbind(ch14.se.bc, ch16.se.bc, ch18.se.bc) # ; View(ch.se.all)
            
            # curvilinear regression for each cluster & make a dataframe called cBE_crv:
            ch.se <- NULL
            ch.se <- data.frame(seq(as.Date("2014-04-1"), as.Date("2018-5-01"), by = "month"))  # make empty data frame for loop output
            for (i in 1:80){
              #i = 12
              a <- subset(ch.se.all, ch.se.all$cluster == i)
              a.int <- approx(x = a$date, y = a$wscore.bc, method = "linear", n = 50, rule = 1, f = 0) ; a.int # 50 months total counted --> roughly every 30 days, but not on the months.
              
              model <- lm(a.int$y ~ poly(a.int$x,2)) # curvilinear regression line
              pred.intervals <- as.data.frame(predict(model, data.frame(x = a.int$x), interval='confidence',level=0.99))
              
              ch.se <- cbind(ch.se, pred.intervals$fit)
            }
            colnames(ch.se) <- c("dates", paste0("cluster", 1:80))
            
            reset_par()
            # par(mar = c(5, 4, 4, 2), pty = "s", xaxs = "i", xaxt = 's', yaxs = "i", yaxt = 's')
            plot(x = ch14.se.bc$date, y = ch14.se.bc$wscore.bc, xlim = c(as.Date("2013-04-1"), as.Date("2019-5-01")), ylim = c(-1,3.5), col = "coral", xlab = "Date", ylab = "Wealth index")
            points(x = ch16.se.bc$date, y = ch16.se.bc$wscore.bc, col = "yellow green")
            points(x = ch18.se.bc$date, y = ch18.se.bc$wscore.bc, col = "skyblue")
            title("Curvilinear interp. of hh's normalized wealth score", line = -3)
            color = "black"
            points(x = ch.se$dates, y = ch.se$cluster56, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster12, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster3, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster4, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster5, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster6, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster8, cex = 0.4, pch = 16, col = color)
            points(x = ch.se$dates, y = ch.se$cluster9, cex = 0.4, pch = 16, col = color)
            legend("top", inset = 0.15, legend=c("2014 data", "2016 data", "2018 data", "interpolated data"), col= c('coral','yellow green','skyblue','black'), pch = c(1, 1, 1, 16), cex = 0.75)
            
            # reshaping data from wide to long:
            colnames <- paste0("cluster", 1:80)
            ch.se_long <- reshape(ch.se, varying = colnames, timevar= "cluster", v.names = "wscore.bc", direction = "long")
          
                  head(ch.se) # HOUSEHOLD WEALTH INDEX BY CLUSTER BY MONTH
                  head(ch.se_long) # HOUSEHOLD WEALTH INDEX BY CLUSTER BY MONTH - LONG FORM
                  
      # rasterize wealth index
                  
                  ch.se_long$year <- as.integer(format(ch.se_long$date,"%Y")) # extract year from date column of BE access data 
                  ch.se_long$month <- as.integer(format(ch.se_long$date,"%m")) # extract month from date column of BE access data 
                  
                  cov_fkt <- NULL ; cov_fkt_names <- NULL
                  for (j in 2014:2018){       # year
                    for (i in 1:12){          # month
                      
                      # MAKING EACH DATE INTO A RASTER: 
                      #i = 4 ; j = 2014 # for choosing specific dates
                      ii = which(ch.se_long$month == i & ch.se_long$year == j) # pick the correct rows
                      if (length(ii) == 0) next
                      fkt.temp = ch.se_long[ii,]
                      
                      # turn fkt.temp into a shape file:
                      temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
                      groups = group_by(temp.df, cluster)
                      cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
                      cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
                      cluster.centroid = data.frame(cluster.long, cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
                      cluster_access_loc = merge(cluster.centroid, fkt.temp, by = "cluster")
                      xy = cluster.centroid[,c(2,3)]
                      cluster_access_loc_sp <- SpatialPointsDataFrame(coords = xy, data = cluster_access_loc)
                      head(cluster_access_loc_sp@data)
                      m.gstat <- gstat(formula = wscore.bc ~ 1, locations = ~Longitude  + Latitude,	data = cluster_access_loc_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance power
                      a = data.frame(predict(m.gstat, my.grid.coords))
                      coordinates(a) <- c("Longitude", "Latitude")
                      gridded(a) <- TRUE 
                      
                      head(a) # CONTAINS ACCESS VALUES BY *PIXEL* FOR THE SPECIFIC DATE
                      
                      # for plotting rasters of specific dates:
                      reset_par() ; range(a@data$var1.pred) ; brks = seq(-9, 2,0.01)  # set the scale
                      my.plot = plot(a, col = rev(terrain.colors(length(brks)-1)) , breaks = brks) # if you want to set the scale
                      # my.plot = image(a, col = rev(terrain.colors(15))) # if you don't care about the scale
                      #add.elements(my.plot)
                      title(paste0("Average wealth index: children ", j, "-", i))
                      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/PCA/Interp_hh") # MAKE SURE THIS IS RIGHT
                      dev.copy(pdf,  paste0("Average wealth index by cluster ",j,"-",i,".pdf"))
                      dev.off()
                      
                      # DISAGGREGATING THE RASTER INTO MEAN VALUE BY FOKONTANY:
                      a.r <- raster(a) # turn spatial pixels layer into raster
                      a.r_bf <- raster::extract(a.r, fokontany, method = 'simple') # extract raster values within each polygon to a list object
                      a.r_mean <- as.data.frame(lapply(a.r_bf, FUN = mean)) # find mean raster value for each polygon
                      a.r_mean <- as.data.frame(reshape(a.r_mean, varying = colnames(a.r_mean), v.names = "a_bf", direction = "long"))
                      a.r_mean <- a.r_mean[,-1] # remove fkt #
                      a.r_mean <- a.r_mean[,-2] # remove ID (no value)
                      
                      head(a.r_mean) # CONTAINS THE AVERAGE ACCESS VALUES BY *FOKONTANY* FOR THE SPECIFIC DATE
                      
                      # plotting average wealth index by fokontany for a specific date:
                      reset_par()
                      newpoly <- fokontany
                      newpoly@data$a.r_mean <- a.r_mean
                      manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
                      #manual.col = colorRampPalette(c("white","blue"))
                      color.match = manual.col(length(unique(newpoly@data$a.r_mean))) # find a shade for each fokontany
                      lookupTable = sort(unique(newpoly@data$a.r_mean)) # sort the access values
                      newpoly@data$color = color.match[match(newpoly@data$a.r_mean, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
                      plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
                      title(paste0("Average wealth by fokontany ", j, "-", i))
                      #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
                      legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), fill = c("#f7f6fd","#4635d0"))
                      #plot(x = newpoly@data$a.r_mean, y = newpoly@data$a.r_mean, col = newpoly@data$color, pch = 16) # confirm that the lowest values of access are lightest and highest values are darkest
                      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/PCA/Interp_hh") # MAKE SURE THIS IS RIGHT
                      dev.copy(pdf,  paste0("Average wealth index by fkt ",j,"-",i,".pdf"))
                      dev.off()
                      
                      # to save the "a.r_mean" dataframe column for the specific date in a bigger dataframe:
                      cov_fkt <- cbind(cov_fkt, a.r_mean) ; dim(cov_fkt)
                      nextname_fkt <- paste("dat", j, i, sep = "_") ; cov_fkt_names <- rbind(cov_fkt_names, nextname_fkt)
                    }
                  }
                  colnames(cov_fkt) <- cov_fkt_names
                  
                  head(cov_fkt) # AVERAGE WEALTH INDEX BY FKT FOR KIDS
                  
                  # turn into long form:
                  cov_fkt <- as.data.frame(cov_fkt)
                  cov_fkt$ID <- c(0:194)
                  cov_fkt_long <- tidyr::gather(cov_fkt, date, wscore, "dat_2014_4" : "dat_2018_5", factor_key = TRUE)
                  cov_fkt_long$year <- as.numeric( str_extract(cov_fkt_long$date, '(?<=_)\\d+')) # pull out year
                  cov_fkt_long$month <- stri_sub(cov_fkt_long$date,-2, -1)
                  cov_fkt_long$month <- str_replace(cov_fkt_long$month, "_", "")
                  cov_fkt_long$year <- as.numeric(cov_fkt_long$year)
                  cov_fkt_long$month <- as.numeric(cov_fkt_long$month)
                  
                  # normalize to 0-1:
                  max.ch.wscore <- max(cov_fkt_long$wscore) ; max.ch.wscore 
                  min.ch.wscore <- min(cov_fkt_long$wscore) ; min.ch.wscore 
                  cov_fkt_long$wscore.n <- ( cov_fkt_long$wscore - min.ch.wscore) / (max.ch.wscore - min.ch.wscore)
                  
                  hist(cov_fkt_long$wscore.n)
                  w.hh.bf <- dplyr::select(cov_fkt_long, ID, year, month, wscore.n)
                
                                head(w.hh.bf) # NORMALIZED AVERAGE WEALTH SCORE BY FKT BY MONTH
                                
                                # save it so you don't have to redo all of this the next time R crashes
                                setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/PCA/")
                                write.csv(w.hh.bf,'Household wealth by fkt.csv', row.names = F)
                                
                                # reopen if needed lol
                                setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/PCA/")
                                w.hh.bf <- read.csv('Household wealth by fkt.csv', header = T)
                  
                        
                        # plot normalized wealth by fkt:
                        # plotting average wealth index by fokontany for a specific date:
                                
                                wealth.av <- with(w.hh.bf, aggregate(wscore.n ~ ID, FUN = "mean"))
                                
                        i = 5 ; j = 2014 # for choosing specific dates
                        ii = which(cov_fkt_long$month == i & cov_fkt_long$year == j) # pick the correct rows
                        if (length(ii) == 0) next
                        fkt.temp = cov_fkt_long[ii,]
                        reset_par()
                        
                        newpoly <- fokontany
                        #newpoly@data$wscore.n <- fkt.temp$wscore.n
                        newpoly@data <- merge(newpoly@data, wealth.av, by = c("ID"))
                        manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
                        #manual.col = colorRampPalette(c("white","blue"))
                        color.match = manual.col(length(unique(newpoly@data$wscore.n))) # find a shade for each fokontany
                        lookupTable = sort(unique(newpoly@data$wscore.n)) # sort the access values
                        newpoly@data$color = color.match[match(newpoly@data$wscore.n, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
                        plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
                        #title(paste0("Average wealth by fokontany ", j, "-", i))
                        title("Average bednet use by fokontany")
                        #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
                        legend("right", legend = c(round(min(newpoly@data$wscore.n),2),round(max(newpoly@data$wscore.n),2)), fill = c("#f7f6fd","#4635d0"))
                        #plot(x = newpoly@data$a.r_mean, y = newpoly@data$a.r_mean, col = newpoly@data$color, pch = 16) # confirm that the lowest values of access are lightest and highest values are darkest
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Images/Covariates/Wealth/Rasterizing, normalizing after") # MAKE SURE THIS IS RIGHT
                        dev.copy(pdf,  paste0("Average wealth index by fkt ",j,"-",i,".pdf"))
                        dev.off()
                        
                        
                        
    #### END ####

#### POPULATION DENSITY - REGISTER ####
                        
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Malaria/")
                        malaria <- read.csv("malaria_rawdata.csv", header = TRUE)  
                        
    #### END ####           
                        
#### BEDNETS - COHORT ####
                        
                        # understanding the questions about bednets
                        # qhs1q117 - moustiquaire obs / not obs
                        # qhs1q118 - how many months have you been using your bednet
                        # qhs1q119 - net has insecticide?
                        # qhs1q123 - did you sleep under the net last night?
                        
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/2014")
                        pvsiq012 <- read.dta('pvsiq012.dta')  # 2014 individual data
                        cov.hh14.se <- dplyr::select(pvsiq012, qhgrappe, qhannee, qhmois, qhmen, qhs1q123)
                        cov.hh14.se$nets <- with(cov.hh14.se, ifelse (!is.na(qhs1q123), 1, 0))
                        cov.hh14.se$qhgrappe <- as.factor(cov.hh14.se$qhgrappe)
                        cov.hh14.se.bc <- tapply(cov.hh14.se$nets, cov.hh14.se$qhgrappe, FUN = mean) 
                        cov.hh14.se.bc <- as.data.frame(cov.hh14.se.bc) ; names(cov.hh14.se.bc) <- c("nets.bc")
                        cov.hh14.se.bc$nets.sum <- tapply(cov.hh14.se$nets, cov.hh14.se$qhgrappe, FUN = sum) 
                        cov.hh14.se.bc$cluster <- c(1:80)
                        cov.hh14.se.bc$year <- 2014
                        cov.hh14.se.bc$month <- 4
                        
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/2016")
                        pvsiq022 <- read.dta('pvsiq022.dta')  # 2016 individual data
                        cov.hh16.se <- dplyr::select(pvsiq022, qhgrappe, qhannee, qhmois, qhmen, qhs1q123)
                        cov.hh16.se$nets <- with(cov.hh16.se, ifelse (!is.na(qhs1q123), 1, 0))
                        cov.hh16.se$qhgrappe <- as.factor(cov.hh16.se$qhgrappe)
                        cov.hh16.se.bc <- tapply(cov.hh16.se$nets, cov.hh16.se$qhgrappe, FUN = mean) 
                        cov.hh16.se.bc <- as.data.frame(cov.hh16.se.bc) ; names(cov.hh16.se.bc) <- c("nets.bc")
                        cov.hh16.se.bc$nets.sum <- tapply(cov.hh16.se$nets, cov.hh16.se$qhgrappe, FUN = sum) 
                        cov.hh16.se.bc$cluster <- c(1:80)
                        cov.hh16.se.bc$year <- 2016
                        cov.hh16.se.bc$month <- 8
                        
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/2018")
                        pvsiq032 <- read.dta('INDIVIDU.dta')  # 2018 individual data
                        colnames(pvsiq032) <- tolower(colnames(pvsiq032)) # put column names in lowercase
                        cov.hh18.se <- dplyr::select(pvsiq032, qhgrappe, qhannee, qhmois, qhmen, qhs1q123)
                        cov.hh18.se$nets <- with(cov.hh18.se, ifelse (!is.na(qhs1q123), 1, 0))
                        cov.hh18.se$qhgrappe <- as.factor(cov.hh18.se$qhgrappe)
                        cov.hh18.se.bc <- tapply(cov.hh18.se$nets, cov.hh18.se$qhgrappe, FUN = mean) 
                        cov.hh18.se.bc <- as.data.frame(cov.hh18.se.bc) ; names(cov.hh18.se.bc) <- c("nets.bc")
                        cov.hh18.se.bc$nets.sum <- tapply(cov.hh18.se$nets, cov.hh18.se$qhgrappe, FUN = sum) 
                        cov.hh18.se.bc$cluster <- c(1:80)
                        cov.hh18.se.bc$year <- 2018
                        cov.hh18.se.bc$month <- 5
                              
                        # what are high and low malaria seasons?
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Malaria/")
                        malaria <- read.csv("malaria_rawdata.csv", header = TRUE)
                        byseason1 <- as.data.frame(tapply(malaria$palu.pc, malaria$month, mean)) ; View(byseason1)
                        byseason2 <- as.data.frame(tapply(malaria$palu.und5.pc, malaria$month, mean)) ; View(byseason2)
                        # for both children and individuals, the high malaria season is 12, 1, 2, 3, 4, 5 and the low season is 6, 7, 8, 9, 10, 11.
                        # in 3 seasons, high: 12, 1, 2, 3; medium: 4, 5, 6, 11; low: 7, 8, 9, 10
                        
                        # yes/no bednet use by individuals: 
                        bednets <- rbind(cov.hh14.se, cov.hh16.se, cov.hh16.se)
                        bednets$qhmois <- as.character(bednets$qhmois)
                        bednets$month <- with(bednets, ifelse (qhmois == "avril", 4, ifelse ( qhmois == "mai", 5, ifelse (qhmois == "f\xe9vrier", 2, ifelse ( qhmois == "aout", 8, ifelse (qhmois == "septembre", 9, NA))))))
                        bednets <- dplyr::select(bednets, qhgrappe, qhannee, month, nets)
                        colnames(bednets) <- c("cluster", "year", "month", "nets")
                        bednets$date <- as.Date(paste(bednets$year, bednets$month, 1, sep = "-"))
                        byseason <- tapply(bednets$nets, bednets$month, mean) # ; View(byseason) # too see which months are high and low bednet season
                        bednets$season <- with(bednets, ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 12, 1, 0)) # 1 = high season, 0 = low season
                        bednets$cluster <- as.numeric(bednets$cluster)
                        
                        # average bednets use by cluster:
                        bednets.bc <- rbind(cov.hh14.se.bc, cov.hh16.se.bc, cov.hh18.se.bc) # by cluster
                        bednets.bc$date <- as.Date(paste(bednets.bc$year, bednets.bc$month, 1, sep = "-"))
                        bednets.bc$season <- with(bednets.bc, ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 12, 1, 0)) # 1 = high season, 0 = low season
                        bednets.bc$season2 <- with(bednets.bc, ifelse(month == 1 | month == 2 | month == 3 | month == 12, 2, ifelse(month == 4 | month == 5 | month == 6 | month == 11, 1, 0))) # 2 = high season, 1 = medium season, 0 = low season
                        
                        # plotting bednet use by cluster:
                        bsubset <- subset(bednets, bednets$season == 0)
                        bn <- as.data.frame(tapply(bsubset$nets, bsubset$cluster, mean)) ; names(bn) <- c("nets")
                              bn$cluster <- c(1:80) ; dat <- bn
                              temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
                              groups = group_by(temp.df, cluster)
                              cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
                              cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
                              cluster.centroid = data.frame(cluster.long,cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
                              cluster_access_loc = merge(cluster.centroid, dat, by = "cluster")
                              xy = cluster.centroid[,c(2,3)]
                              cluster_access_loc_sp <- SpatialPointsDataFrame(coords = xy, data = cluster_access_loc)
                              class(cluster_access_loc_sp) # spatial points data frame
                              head(cluster_access_loc_sp@data)
                              m.gstat <- gstat(formula = nets ~ 1, locations = ~Longitude  + Latitude,	data = cluster_access_loc_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance powe
                              z=data.frame(predict(m.gstat, my.grid.coords))
                              coordinates(z) <- c("Longitude", "Latitude") 
                              gridded(z) <- TRUE 
                              brks = seq(.5,1,0.001) ; range(z@data$var1.pred) # to help set the scale
                              my.plot = plot(z, col = rev(terrain.colors(length(brks)-1)) , breaks = brks) # if you want to set the scale
                              #my.plot=image(z, col=rev(terrain.colors(15))) # if you don't care about the scale
                              title("Overall average bednet use - low season", line = -1, adj = 0.1)
                              
                  # making a model for bednet use
                            
                        # what is the distrubtion of the data?
                              hist(bednets$nets)
                              hist(bednets.bc$nets.bc) # bednet use by cluster looks pretty normal
                            
                        # find average proportion of bednet users during high and low season.
                              
                              subset.h <- subset(bednets.bc, bednets.bc$season == 1)
                              bednets.bc.h <- as.data.frame(tapply(subset.h$nets.bc, subset.h$cluster, mean)) ; colnames(bednets.bc.h) <- c("nets.bc.bs")
                              bednets.bc.h$season <- 1
                              bednets.bc.h$cluster <- c(1:80)
                              
                              subset.l <- subset(bednets.bc, bednets.bc$season == 0)
                              bednets.bc.l <- as.data.frame(tapply(subset.l$nets.bc, subset.l$cluster, mean)) ; colnames(bednets.bc.l) <- c("nets.bc.bs")
                              bednets.bc.l$season <- 0
                              bednets.bc.l$cluster <- c(1:80)
                              
                        # use gstat model to make an estimate of bednet usage per fkt per high/low season (NOT by month!)
                        # do gstat model to plot 
                        # do moran indexes to see if there's spatial autocorrelation
                        
                        # create & plot grid in the shape of Ifanadiana:
                        my.grid = F.grid(fokontany)
                        my.grid.coords <- my.grid$grid.coords 
                        my.grid.sp <- my.grid$grid.sp 
                        par(mar=c(0,0,0,0))
                        #plot(my.grid.sp) 
                        
                        # data frame with prediction/simulation locations; 
                        # newdata should contain columns with the independent variables (if present) and the coordinates with names as defined in locations
                         my.grid.coords$season <- 0
                         my.grid.coords$year <- 2016 ; head(my.grid.coords)
                        
                        dat <- bednets.bc.l
                        
                        dat <- bednets.bc
                        
                        temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
                        groups = group_by(temp.df, cluster)
                        cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
                        cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
                        cluster.centroid = data.frame(cluster.long,cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
                        mdat = merge(cluster.centroid, dat, by = "cluster")
                        #xy = cluster.centroid[,c(2,3)]
                        mdat_sp <- SpatialPointsDataFrame(coords = mdat[c(2,3)], data = mdat)
                        class(mdat_sp) ; dim(mdat_sp@data) ; head(mdat_sp@data)
                        
                        # the models:
                        m.gstat <- gstat(formula = nets.bc.bs ~ 1, locations = ~Longitude  + Latitude,	data = mdat_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance power
                        m.gstat <- gstat(formula = nets.bc ~ season2 + year, locations = ~Longitude  + Latitude,	data = mdat_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance power
                        
                        z <- data.frame(predict(object = m.gstat, newdata = my.grid.coords))
                        
                        coordinates(z) <- c("Longitude", "Latitude") ; gridded(z) <- TRUE 
                        brks = seq(0,1,0.001) ; range(z@data$var1.pred, na.rm = TRUE) # to help set the scale
                        my.plot = plot(z, col = rev(terrain.colors(length(brks)-1)) , breaks = brks) # if you want to set the scale
                        plot(fokontany, add = TRUE)
                        title("Average bednet use - high, 2018 subset")
                      
                                # how to tell how good the model is:
                                # variogram: a function describing the degree of spatial autocorrelation
                                    # The following parameters are often used to describe variograms:
                                    # nugget n: The height of the jump of the semivariogram at the discontinuity at the origin.
                                    # sill s: Limit of the variogram tending to infinity lag distances.
                                    # range r: The distance in which the difference of the variogram from the sill becomes negligible. In models with a fixed sill, it is the distance at which this is first reached; for models with an asymptotic sill, it is conventionally taken to be the distance when the semivariance first reaches 95% of the sill.
                                    
                                    my.vgm <- variogram(m.gstat) ; plot(my.vgm)
                                    
                                    vgm.plot <- ggplot(my.vgm, aes(dist, gamma)) + geom_point() + geom_smooth() + ylim(0, .025)
                                    mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (12)), 
                                                          axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                                                          axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
                                    
                                    print(vgm.plot + mynamestheme + labs(title= "subset low 2016", y="Semivariance", x = "Distance between points"))
                              
                                    
                                    # using pgirmess correlograms - faster because it doesn't used a randomization test
                                    pgi.cor <- pgirmess::correlog(coords= z[,1:2], z$var1.pred, method="Moran", nbclass=21)
                                    pgi.cor <- pgirmess::correlog(coords= cluster_access_loc_sp@data[,2:3], ch.anth18.bc$stunt.modsev, method="Moran", nbclass=21)
                                    plot(pgi.cor)
                                    pgi.cor
                      
                        # rasterize and disaggregate by fkt?
                        alt <- raster(z) # turn spatial pixels layer into raster
                        alt_bf <- raster::extract(alt, fokontany, method = 'simple') # extract raster values within each polygon to a list object
                        alt_mean <- as.data.frame(lapply(alt_bf, FUN = mean, na.rm = TRUE)) # find mean raster value for each polygon
                        alt_mean <- as.data.frame(reshape(alt_mean, varying = colnames(alt_mean), v.names = "alt_bf", direction = "long"))
                        newpoly <- fokontany ; newpoly@data$plotme <- alt_mean$alt_bf
                        manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
                        #manual.col = colorRampPalette(c("white","blue"))
                        color.match = manual.col(length(unique(newpoly@data$plotme))) # find a shade for each fokontany
                        lookupTable = sort(unique(newpoly@data$plotme)) # sort the access values
                        newpoly@data$color = color.match[match(newpoly@data$plotme, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
                        plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
                        title(paste0("Average bednet use by fkt - high season"), line = -1)
                        legend("right", legend = c(round(min(newpoly@data$plotme, na.rm = TRUE),2),round(max(newpoly@data$plotme, na.rm = TRUE),2)), fill = c("#f7f6fd","#4635d0"))
                       
                        
                        # saving bednet estimates by season-month
                        lowseason <- alt_mean$alt_bf # definitely not right though
                        highseason <- alt_mean$alt_bf
                        
                        bednets.bs.bf <- as.data.frame(cbind(highseason, lowseason))
                        bednets.bs.bf$ID <- c(0:194)
                        
                            # save it so you don't have to redo all of this the next time R crashes
                            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Bednets/")
                            write.csv(bednets.bs.bf,'bednets.bs.bf.csv', row.names = F)
                                
                            # reopen if needed lol
                            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Bednets/")
                            bednets.bs.bf <- read.csv('bednets.bs.bf.csv', header = T)
                            
                        # reshape for easier merging
                        bednets.bs.l <- reshape(bednets.bs.bf, varying = c("highseason", "lowseason"), v.names = "bednets", timevar = "season", times = c("highseason", "lowseason"), direction = "long")
                        bednets.bs.l <- dplyr::select(bednets.bs.l, ID, season, bednets)
                        bednets.bs.l$season <- ifelse(bednets.bs.l$season == "highseason", 1, 0)
                      
                        # add in months & years
                        bednets.bs.bf2 <- as.data.frame(c(1:12)) ; colnames(bednets.bs.bf2) <- c("month")
                        bednets.bs.bf2$year <- 2014
                        bednets.bs.bf15 <- as.data.frame(c(1:12)) ; colnames(bednets.bs.bf15) <- c("month")
                        bednets.bs.bf15$year <- 2015
                        bednets.bs.bf16 <- as.data.frame(c(1:12)) ; colnames(bednets.bs.bf16) <- c("month")
                        bednets.bs.bf16$year <- 2016
                        bednets.bs.bf17 <- as.data.frame(c(1:12)) ; colnames(bednets.bs.bf17) <- c("month")
                        bednets.bs.bf17$year <- 2017
                        bednets.bs.bf2 <- rbind(bednets.bs.bf2, bednets.bs.bf15, bednets.bs.bf16, bednets.bs.bf17)
                        bednets.bs.bf2$season <- with(bednets.bs.bf2, ifelse(month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 12, 1, 0)) # 1 = high season, 0 = low season
                        bednets.bs.bf2 <- do.call(rbind, replicate(195,bednets.bs.bf2 , simplify=FALSE))
                        bednets.bs.bf2 <- as.data.frame(bednets.bs.bf2)
                        bednets.bs.bf2 <- dplyr::select(bednets.bs.bf2, year, month, season)
                        bednets.bs.bf2$ID <- rep(0:194, each = 48)
                        
                        bednets.bs.bd <- merge(bednets.bs.bf2, bednets.bs.l, by = c("ID", "season"), all.x = TRUE)
                        
                                # save it so you don't have to redo all of this the next time R crashes
                                setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Bednets/")
                                write.csv(bednets.bs.bd,'bednets.bs.bd.csv', row.names = F)
                                
                                # reopen if needed lol
                                setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Bednets/")
                                bednets.bs.bd <- read.csv('bednets.bs.bd.csv', header = T)
                        
                                # plot average bednet use over time
                                bednets.av <- with(bednets.bs.bd, aggregate(bednets ~ ID, FUN = "mean"))
                                
                                head(bednets.bs.bd)
                                bednets.bs.bd$date <- as.Date(paste(bednets.bs.bd$year, bednets.bs.bd$month,1, sep = "-"))
                                
                                mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20)), 
                                                      axis.title = element_text(family = "Helvetica", size = (15), colour = "steelblue4"),
                                                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (15)))
                                plot <- ggplot(bednets.bs.bd, aes(date,bednets)) + geom_smooth() +  geom_point()
                                
                                myplot <- print(plot + mynamestheme + labs(title= "Bednet use over time",
                                                                           y="Bednet use", x = "Date"))
                                
    #### END ####

#### ALTITUDE & SLOPE ####
  
  setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude")
  list.files()
  
      # bringing in & plotting full data to see what is the difference between these files:
  
            dpath<-"/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/hdr.adf"
            x <- new("GDALReadOnlyDataset", dpath)
            getDriverLongName(getDriver(x))
            xx<-asSGDF_GROD(x, output.dim=c(200, 200))
            class(xx) # SpatialGridDataFrame
            # simple plot:
              plot(xx)
              title("hdr.adf - Altitude")
            # fancy plot:
                spplot(xx, "band1", at=c(0, 10, 50, 100, 200, 500, 1000, 2500, max(xx$band1, na.rm=TRUE)), col.regions=brewer.pal(8,"Oranges") )
            head(xx@data) # band1 looks full?
      
            dpath<-"/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/w001001.adf"    
            x <- new("GDALReadOnlyDataset", dpath)
            getDriverLongName(getDriver(x))
            xx<-asSGDF_GROD(x, output.dim=c(200, 200))
            plot(xx)
            class(xx) # SpatialGridDataFrame
            
            dpath<-"/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/w001001x.adf"    
            x <- new("GDALReadOnlyDataset", dpath)
            getDriverLongName(getDriver(x))
            xx<-asSGDF_GROD(x, output.dim=c(200, 200))
            plot(xx)
            class(xx) # SpatialGridDataFrame
            
            dpath<-"/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/dblbnd.adf"    
            x <- new("GDALReadOnlyDataset", dpath)
            getDriverLongName(getDriver(x))
            xx<-asSGDF_GROD(x, output.dim=c(200, 200))
            plot(xx)
            class(xx) # SpatialGridDataFrame
            
            dpath<-"/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/vat.adf"    
            x <- new("GDALReadOnlyDataset", dpath)
            getDriverLongName(getDriver(x))
            xx<-asSGDF_GROD(x, output.dim=c(200, 200))
            plot(xx)
            class(xx) # SpatialGridDataFrame
            
      # turning pixel data into an average altitude by fkt:
            
            alt <- raster(xx) # turn spatial pixels layer into raster
            alt_bf <- raster::extract(alt, fokontany, method = 'simple') # extract raster values within each polygon to a list object
            alt_mean <- as.data.frame(lapply(alt_bf, FUN = mean, na.rm = TRUE)) # find mean raster value for each polygon
            alt_mean <- as.data.frame(reshape(alt_mean, varying = colnames(alt_mean), v.names = "alt_bf", direction = "long"))
            alt_mean$ID <- c(0:194) 
            
            # fill empty tail (NaN)
            alt_mean[108.1, 2] <- 1004.6667 # fill missing fkt with nearest fkt
            
            
            # plot averages by fkt:
            reset_par()
            newpoly <- fokontany
            newpoly@data$plotme <- alt_mean$alt_bf
            manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
            #manual.col = colorRampPalette(c("white","blue"))
            color.match = manual.col(length(unique(newpoly@data$plotme))) # find a shade for each fokontany
            lookupTable = sort(unique(newpoly@data$plotme)) # sort the access values
            newpoly@data$color = color.match[match(newpoly@data$plotme, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
            plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
            title(paste0("Average altitude by fkt"), line = -1)
            #legend("right", legend = c(round(min(newpoly@data$alt_mean),2),round(max(newpoly@data$alt_mean),2)), col = c("white","blue"), pch = 19)
            legend("right", legend = c(round(min(newpoly@data$plotme, na.rm = TRUE),0),round(max(newpoly@data$plotme, na.rm = TRUE),0)), fill = c("#f7f6fd","#4635d0"))
            #plot(x = newpoly@data$alt_mean, y = newpoly@data$alt_mean, col = newpoly@data$color, pch = 16) # confirm that the lowest values of access are lightest and highest values are darkest
            
        alt_mean <- alt_mean[,-1]
        alt_mean <- alt_mean[,-2]
      
        head(alt_mean) 
        
        altitude.bf <- alt_mean # AVERAGE ALTITUDE BY FKT
        
        # save it so you don't have to redo all of this the next time R crashes (lawl)
        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/")
        write.csv(altitude.bf,'altitude.bf.csv', row.names = F)
        
        # reopen if needed lol
        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/")
        altitude.bf <- read.csv('altitude.bf.csv', header = T)
        
        
    # find average slope per fkt:
        
              # start by turning altitude into slope
              slope <- terrain(alt, opt='slope', unit='degrees', neighbors=8)
              plot(slope) ; title("Slope in degrees") ; plot(fokontany, add = TRUE)
              
              slp_bf <- raster::extract(slope, fokontany, method = 'simple') # extract raster values within each polygon to a list object
              slp_mean <- as.data.frame(lapply(slp_bf, FUN = mean, na.rm = TRUE)) # find mean raster value for each polygon
              slp_mean <- as.data.frame(reshape(slp_mean, varying = colnames(slp_mean), v.names = "slp_bf", direction = "long"))
              slp_mean$ID <- c(0:194) 
              
              slp_mean[108.1, 2] <- 2.1888036
              slp_mean[93.1, 2] <- 2.1888036
        
              # plot averages by fkt:
              reset_par()
              newpoly <- fokontany
              newpoly@data$plotme <- slp_mean$slp_bf
              manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
              #manual.col = colorRampPalette(c("white","blue"))
              color.match = manual.col(length(unique(newpoly@data$plotme))) # find a shade for each fokontany
              lookupTable = sort(unique(newpoly@data$plotme)) # sort the access values
              newpoly@data$color = color.match[match(newpoly@data$plotme, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
              plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
              title(paste0("Average slope by fkt (degrees)"), line = -1)
              #legend("right", legend = c(round(min(newpoly@data$slp_mean),2),round(max(newpoly@data$slp_mean),2)), col = c("white","blue"), pch = 19)
              legend("right", legend = c(round(min(newpoly@data$plotme, na.rm = TRUE),2),round(max(newpoly@data$plotme, na.rm = TRUE),2)), fill = c("#f7f6fd","#4635d0"))
              #plot(x = newpoly@data$slp_mean, y = newpoly@data$slp_mean, col = newpoly@data$color, pch = 16) # confirm that the lowest values of access are lightest and highest values are darkest
              
              slp_mean <- slp_mean[,-1]
              slp_mean <- slp_mean[,-2]
              
              head(slp_mean) 
              
              slope.deg.bf <- slp_mean # AVERAGE SLOPE BY FKT IN DEGREES
              slope.rad.bf <- slp_mean # AVERAGE SLOPE BY FKT IN RADIANS
              
              
              # save it so you don't have to redo all of this the next time R crashes
              setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Slope (from altitude)/")
              write.csv(slope.deg.bf,'slope.deg.bf.csv', row.names = F)
              
              # reopen if needed lol
              setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Bednets/")
              slope.deg.bf <- read.csv('slope.deg.bf.csv', header = T)
      
    #### END ####
                        
#### HYDROLOGY ####
    
  # rivers:
  
      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Hydrology")
      list.files() # need .shp, .shx, .dbf, prj files
      file.exists("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Hydrology/af_riv_15s_Ifd.shp")
      riv <- readOGR("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Hydrology/af_riv_15s_Ifd.shp") 
  
      plot(riv)
    
  # changing the projections to plot with the fkts
      
      latlong <- "+proj=omerc +lat_0=-18.9 +lonc=46.437229166666 +alpha=18.9 +k=0.9995000000000001 +x_0=400000 +y_0=800000 +gamma=18.9 +ellps=intl +units=m +no_defs"
      proj4string(riv)
      riv <- spTransform(riv, CRS(latlong))
  
  # plot upcell.
      
              riv@data$UP_CELLS <- as.character(riv@data$UP_CELLS) ; str(riv@data)
              riv@data$UP_CELLS <- as.numeric(riv@data$UP_CELLS) ; str(riv@data)
              
              riv@data$UP_CELLS.n <- (riv@data$UP_CELLS - min(riv@data$UP_CELLS)) / max(riv@data$UP_CELLS) # normalize upcell to 0-1 so it can be plotted (lwd can't be 630 --> all black)
              
              hist(riv@data$UP_CELLS.n)
              head(riv@data$UP_CELLS)
              plot(fokontany) ; plot(riv, lwd = riv@data$UP_CELLS.n * 10, col = "blue", add = TRUE) # plots river thickness based on upcell
              title("Rivers with upcell")
              
  # find the average upcell.n by fkt (normalize to 0-1).
              
              fokontany@data$ID <- c(0:194)
              
              a <- riv[fokontany,] # selects all of the rivers that fall (at least partly) inside/on geometries in fokontany
             
              fkt1 <- (subset(fokontany, ID == 4)) ; plot(fkt1) # plot fkt 1
              a <- riv[fkt1,] ; plot(fokontany) ; plot(a, add = TRUE)
              
              plot(fokontany) ; plot(a, col = fokontany@data$LIB_FKT, add = TRUE)
              
              upcell = aggregate(riv, fokontany, FUN = mean)
              #plot(upcell) ; View(upcell@data)
              
              plot(fokontany)
              manual.col = colorRampPalette(c("yellow","yellowgreen"))
              color.match = manual.col(length(unique(upcell@data$UP_CELLS.n))) # find a shade for each fokontany
              lookupTable = sort(unique(upcell@data$UP_CELLS.n)) # sort the access values
              upcell@data$color = color.match[match(upcell@data$UP_CELLS.n, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
              plot(upcell, col = upcell@data$color, border= "light grey", lwd = 0.5)
              legend("right", legend = c( round(min(upcell@data$UP_CELLS.n, na.rm = TRUE),0), round(max(upcell@data$UP_CELLS.n, na.rm = TRUE),0)), fill = c("yellow","yellowgreen"))
              title("Average normalized upcell by fkt")
              
              upcell@data$ID <- c(0:194)
              
              head(upcell)
              
              upcell.bf <- as.data.frame(cbind(upcell@data$ID, upcell@data$UP_CELLS.n)) ; colnames(upcell.bf) <- c("ID", "upcell")
              
              head(upcell.bf) # AVERAGE UPCELL BY FKT
              
              # save it so you don't have to redo all of this the next time R crashes
              setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Hydrology/")
              write.csv(upcell.bf,'upcell.bf.csv', row.names = F)
              
              # reopen if needed lol
              setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Hydrology/")
              upcell.bf <- read.csv('upcell.bf.csv', header = T)
     
    #### END ####  
         
#### DEFORESTATION ####
     
  # tree cover in the year 2000:
     
      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation")
      list.files() # need .shp, .shx, .dbf, prj files
      tc <- "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/Hansen_GFC2015_treecover2000_20S_040E.tif" 
      tc <- raster(tc)
      plot(tc) ; plot(fokontany, add = TRUE) ; title("Treecover")
      
  # year of gross forest cover loss event (lossyear): A disaggregation of total forest loss to annual time scales. Encoded as either 0 (no loss) or else a value in the range 1–13, representing loss detected primarily in the year 2001–2014, respectively.
      
      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation")
      list.files() # need .shp, .shx, .dbf, prj files
    
      yfl <- "Hansen_GFC2015_lossyear_20S_040E.tif" # works better
      yfl <- raster(yfl)
     
      extent(yfl)
      yfl.crop <- crop(yfl, extent(47.2, 48.1, -21.75, -20.4))
  
      fkt <- fokontany ; fkt <- spTransform(fkt, CRS(proj4string(yfl.crop)))

      yfl.compare <- yfl.crop # just for comparison later, to make sure the 3 make sense
      
      yfl.recent <- yfl.crop
      yfl.old <- yfl.crop
      
      yfl.crop[yfl.crop != 0] <- 1 # yfl.crop - 0 if forest not lost, 1 if forest ever lost during 2000-2014
      
      yfl.recent[yfl.recent < 7] <- 0
      yfl.recent[yfl.recent >= 7 & yfl.recent <= 14] <- 1 # yfl.recent - 0 if not lost or lost between 2008-2014; 1 if lost during 2000-2007
      
      yfl.old[yfl.old > 7 | yfl.old == 0] <- 0
      yfl.old[yfl.old <= 7 & yfl.old > 0 ] <- 1 # yfl.old - 0 if not lost or lost during 2000-2007; 1 if lost during 2008-2014
      
      # plot them all 
      par(mfrow=c(1,4)) 
      plot(yfl.crop) ; plot(fkt, add = TRUE) ; title("Before any changes")
      plot(yfl.crop) ; plot(fkt, add = TRUE) ; title("Forest ever lost") # plot them again
      plot(yfl.recent) ; plot(fkt, add = TRUE) ; title("Forest lost 2007-2014") # plot them again
      plot(yfl.old) ; plot(fkt, add = TRUE) ; title("Forest lost 2000-2007") # plot them again
      
      View(yfl.crop@data) # check out the NAs
      
      head(yfl.crop) # the raster for year of forest loss with NA instead of 0
      
  # global forest cover loss 2000–2014 (loss) : Forest loss during the period 2000–2014, defined as a stand-replacement disturbance, or a change from a forest to non-forest state. Encoded as either 1 (loss) or 0 (no loss).
      
      fcl <- "Hansen_GFC2015_loss_20S_040E.tif" # works better
      fcl <- raster(fcl)
      plot(fcl) ; plot(fokontany, add = TRUE) ; title("Forest clover loss during 2000-2014")
      
  # find the average by fkt:
      
      rast <- yfl.old # add data here: yfl.crop, tc, fcl
      fl_bf <- raster::extract(rast, fokontany, method = 'simple') # extract raster values within each polygon to a list object
      fl_mean <- as.data.frame(lapply(fl_bf, FUN = mean, na.rm = TRUE)) # find mean raster value for each polygon
      fl_mean <- as.data.frame(reshape(fl_mean, varying = colnames(fl_mean), v.names = "fl_bf", direction = "long"))
      fl_mean$ID <- c(0:194) 
      
      # plot averages by fkt:
      reset_par()
      newpoly <- fokontany
      newpoly@data$plotme <- fl_mean$fl_bf
      manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
      #manual.col = colorRampPalette(c("white","blue"))
      color.match = manual.col(length(unique(newpoly@data$plotme))) # find a shade for each fokontany
      lookupTable = sort(unique(newpoly@data$plotme)) # sort the access values
      newpoly@data$color = color.match[match(newpoly@data$plotme, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
      plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
      title(paste0("Average year of forest loss - past (2000-2007)"), line = -1)
      #legend("right", legend = c(round(min(newpoly@data$fl_mean),2),round(max(newpoly@data$fl_mean),2)), col = c("white","blue"), pch = 19)
      legend("right", legend = c(round(min(newpoly@data$plotme, na.rm = TRUE),1),round(max(newpoly@data$plotme, na.rm = TRUE),1)), fill = c("#f7f6fd","#4635d0"))
      #plot(x = newpoly@data$fl_mean, y = newpoly@data$fl_mean, col = newpoly@data$color, pch = 16) # confirm that the lowest values of access are lightest and highest values are darkest
      
      yfl.bf <- fl_mean # AVERAGE YEAR OF GROSS FOREST LOSS PER FKT (0 - FOREST STILL THERE; 1-13 - FOREST LOSS IN 2001-2014; DARK PURPLE - LOST RECENTLY; WHITE - NOT LOST; LIGHT PURPLE - LOST A LONG TIME AGO)
      fcl.bf <- fl_mean # GLOBAL FOREST COVER LOSS 2000-2014 (change from a forest to non-forest state. Encoded as either 1 (loss) or 0 (no loss).)
      tc.bf <- fl_mean # TREE COVER IN THE YEAR 2000
      
      # year of forest loss:
      
      # forest ever lost:
      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
      yfl.bf <- dplyr::select(yfl.bf, ID, fl_bf)
      colnames(yfl.bf) <- c("ID", "yfl_bf")
      
          # ever forest lost (2000-2014):
          setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
          yfl.bf <- dplyr::select(yfl.bf, ID, fl_bf)
          colnames(yfl.bf) <- c("ID", "yfl.ever")
          write.csv(yfl.ever,'yfl.ever.csv', row.names = F)
          yfl.ever <- read.csv('yfl.ever.csv', header = T)
          
          # recent forest loss (2007-2014):
          setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
          yfl.bf <- dplyr::select(yfl.bf, ID, fl_bf)
          colnames(yfl.bf) <- c("ID", "yfl.rec")
          write.csv(yfl.bf,'yfl.rec.csv', row.names = F)
          yfl.rec <- read.csv('yfl.rec.csv', header = T)
          
          # non-recent forest loss (2000-2007):
          setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
          yfl.bf <- dplyr::select(yfl.bf, ID, fl_bf)
          colnames(yfl.bf) <- c("ID", "yfl.past")
          write.csv(yfl.bf,'yfl.past.csv', row.names = F)
          yfl.past <- read.csv('yfl.past.csv', header = T)
          
          
      # tree cover in 2000:
      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
      tc.bf <- dplyr::select(tc.bf, ID, fl_bf)
      colnames(tc.bf) <- c("ID", "tc_bf")
      write.csv(tc.bf,'tc.bf.csv', row.names = F) 
      
          setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
          tc.bf <- read.csv('tc.bf.csv', header = T)
          
      
      # forest cover loss:
      setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
      fcl.bf <- dplyr::select(fcl.bf, ID, fl_bf)
      colnames(fcl.bf) <- c("ID", "fcl.bf")
      write.csv(fcl.bf,'fcl.bf.csv', row.names = F) # fixed in last section
      
          setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
          fcl.bf <- read.csv('fcl.bf.csv', header = T)
          
      
    #### END ####
      
#### MALARIA ATLAS ####
  
    require(malariaAtlas) # https://cran.rstudio.com/web/packages/malariaAtlas/malariaAtlas.pdf
    list <- listRaster(printed = TRUE) # shows all rasters available for download
    isAvailable_pr(country = "Madagascar")
    MDG_shp <- getShp(ISO = "MDG", admin_level = c("admin0", "admin1"))
        # consider: 
            # All-cause fever,  
            # Malaria-attributable fever as a proportion of all-cause fever, 
            # G6PD Deficiency Allele Frequency, HbS (Sickle Hemoglobin) Allele Frequency, 
            # Indoor residual spraying (IRS) coverage, 
            # Insecticide-treated bednet (ITN) coverage, 
            # Plasmodium falciparum Incidence: available from 2000-2015 
            # Plasmodium falciparum PR2-10 (parasite rate): available from 2000 - 2015
            # Plasmodium vivax Endemicity
            # Plasmodium knowlesi Risk 
      
    # downloading rasters:
  
        # Plasmodium falciparum PR2-10 (parasite rate): available from 2000 - 2015
    
              pfpr210_14 <- getRaster(surface = "Plasmodium falciparum PR2-10", shp = MDG_shp,
                        extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                        year = 2014, vector_year = NULL)
              
                        plot(pfpr210_14) # RasterLayer
                        title("Plasmodium falciparum PR2-10 2014 MalariaAtlas")
                        
              pfpr210_15 <- getRaster(surface = "Plasmodium falciparum PR2-10", shp = MDG_shp,
                        extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                        year = 2015, vector_year = NULL)
                        
                        plot(pfpr210_15) # RasterLayer
                        title("Plasmodium falciparum PR2-10 2015 MalariaAtlas")
                        
          #  HbS (Sickle Haemoglobin) Allele Frequency
                        
              sc <- getRaster(surface = "HbS (Sickle Haemoglobin) Allele Frequency", shp = MDG_shp,
                        extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                       vector_year = NULL)
                        
                        plot(sc) # RasterLayer
                        title("HbS (Sickle Haemoglobin) Allele Frequency MalariaAtlas")
                        
          #  G6PD Deficiency Allele Frequency
                        
               g6pd <- getRaster(surface = "G6PD Deficiency Allele Frequency", shp = MDG_shp,
                        extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                        vector_year = NULL)
                        
                        plot(g6pd) # RasterLayer
                        title("G6PD Deficiency Allele Frequency")
                
          # All cause fever
                        
                allcausefever <- getRaster(surface = "All-cause fever", shp = MDG_shp,
                        extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                        year = 2014, vector_year = NULL)
                        
                        plot(allcausefever)
                        title("All cause fever 2014 MalariaAtlas")   
                        
          # Plasmodium falciparum incidence
                        
               pf_inc_13 <- getRaster(surface = "Plasmodium falciparum Incidence", shp = MDG_shp,
                          extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                          year = 2013, vector_year = NULL) # NOT WORKING?
                          
                          plot(pf_inc_13) # RasterLayer
                          title("Plasmodium falciparum PR2-10 2013 MalariaAtlas")
                         
              pf_inc_14 <- getRaster(surface = "Plasmodium falciparum Incidence", shp = MDG_shp,
                          extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                          year = 2014, vector_year = NULL) # NOT WORKING?
                          
                          plot(pf_inc_14) # RasterLayer
                          title("Plasmodium falciparum incidence 2014 MalariaAtlas")
                          
               pf_inc_15 <- getRaster(surface = "Plasmodium falciparum Incidence", shp = MDG_shp,
                          extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                          year = 2015, vector_year = NULL)
               
                           plot(pf_inc_15) # RasterLayer
                           title("Plasmodium falciparum incidence 2015 MalariaAtlas")
     
         # Insecticide-treated bednet (ITN) coverage - DOES NOT WORK
                           
               itn14 <- getRaster(surface = "Insecticide-treated bednet (ITN) coverage", shp = MDG_shp,
                           extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                           year = 2014, vector_year = NULL) # NOT WORKING?
                           
                           plot(pf_inc_14) # RasterLayer
                           title("Plasmodium falciparum incidence 2014 MalariaAtlas")
                
          # Indoor residual spraying (IRS) coverage - DOES NOT WORK
                           
               itn14 <- getRaster(surface = "Indoor residual spraying (IRS) coverage", shp = MDG_shp,
                          extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                          year = 2014, vector_year = NULL) # NOT WORKING?
                           
                           plot(pf_inc_14) # RasterLayer
                           title("Plasmodium falciparum incidence 2014 MalariaAtlas")  
      
         # Dominant vectors
                           
                           dv <- getRaster(surface = "Dominant Vectors", shp = MDG_shp,
                                           extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                                           vector_year = NULL) # NOT WORKING?
                           
                           plot(dv) # RasterLayer
                           title("Dominant malaria mosquito vectors")   
                           
          # Anopheles gambiae
                           
              ag <- getRaster(surface = "Anopheles gambiae", shp = MDG_shp,
                            extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                            vector_year = NULL) # NOT WORKING?
                           
                           plot(ag) # RasterLayer
                           title("Anopheles Gambiae probability - Giles 1902")          
                           
          # Anopheles gambiae species complex
                           
               asc <- getRaster(surface = "Anopheles gambiae species complex", shp = MDG_shp,
                           extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                           vector_year = NULL) # NOT WORKING?
                           
                           plot(asc) # RasterLayer
                           title("Anopheles Gambiae species complex")    
                          
            # Anopheles funestus
                           
                           af <- getRaster(surface = "Anopheles funestus", shp = MDG_shp,
                                            extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                                            vector_year = NULL) # NOT WORKING?
                           
                           plot(af) # RasterLayer
                           title("Anopheles funestus")                  
              
            #  Anopheles arabiensis Patton, 1905
                           
                           aa <- getRaster(surface = "Anopheles arabiensis Patton, 1905", shp = MDG_shp,
                                           extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                                           vector_year = NULL) # NOT WORKING?
                           
                           plot(aa) # RasterLayer
                           title("Anopheles arabiensis")      
                           
          #  Anopheles merus Dönitz, 1902
                           
                           am <- getRaster(surface = "Anopheles merus Dönitz, 1902", shp = MDG_shp,
                                           extent = NULL, file_path = "/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas", 
                                           vector_year = NULL) # NOT WORKING?
                           
                           plot(am) # RasterLayer
                           title("Anopheles merus")                  
                            
                          
                # turning pixel data into an average altitude by fkts:
                
                atlas <- g6pd # put data here
                
                  # plot atlas with fkts:
                  plot(atlas) ; plot(fokontany, add = TRUE)
                  # plot close-up of fkts:
                  plot(fokontany) ; plot(atlas, add = TRUE) ; plot(fokontany, add = TRUE) 
                atlas_bf <- raster::extract(atlas, fokontany) # extract raster values within each polygon to a list object
                atlas_mean <- as.data.frame(lapply(atlas_bf, FUN = mean)) # find mean raster value for each polygon
                atlas_mean <- as.data.frame(reshape(atlas_mean, varying = colnames(atlas_mean), v.names = "atlas_bf", direction = "long"))
                atlas_mean$ID <- c(0:194) 
                
                # plot averages by fkt:
                reset_par()
                newpoly <- fokontany
                newpoly@data$plotme <- atlas_mean$atlas_bf
                manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
                #manual.col = colorRampPalette(c("white","blue"))
                color.match = manual.col(length(unique(newpoly@data$plotme))) # find a shade for each fokontany
                lookupTable = sort(unique(newpoly@data$plotme)) # sort the access values
                newpoly@data$color = color.match[match(newpoly@data$plotme, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
                plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
                title(paste0("Anopheles merus"), line = -1)
                #legend("right", legend = c(round(min(newpoly@data$atlas_mean),2),round(max(newpoly@data$atlas_mean),2)), col = c("white","blue"), pch = 19)
                legend("right", legend = c(round(min(newpoly@data$plotme, na.rm = TRUE),3),round(max(newpoly@data$plotme, na.rm = TRUE),3)), fill = c("#f7f6fd","#4635d0"))
                #plot(x = newpoly@data$atlas_mean, y = newpoly@data$atlas_mean, col = newpoly@data$color, pch = 16) # confirm that the lowest values of access are lightest and highest values are darkest
                
                atlas_mean <- atlas_mean[,-1]
                atlas_mean <- atlas_mean[,-2]
                
                head(atlas_mean) # AVERAGE VALUE BY FKT
                
                        pfpr210_14.bf <- atlas_mean # 2014 PF PR2-10 prevalence by fkt
                        pfpr210_15.bf <- atlas_mean # 2015 PF PR2-10 prevalence by fkt
                        sc.bf <- atlas_mean # Sickle cell prevalence by fkt
                        g6pd.bf <- atlas_mean # G6PD prevelance by fkt
                        allcausefever14.bf <- atlas_mean # 2014 all cause fever
                        pf_inc_14.bf <- atlas_mean # 2014 PF incidence
                        pf_inc_15.bf <- atlas_mean # 2015 PF incidence
                        ag.bf <- atlas_mean # Anopheles gambia probably - 1902???
                        asc.bf <- atlas_mean # Anopheles species complex
                        af.bf <- atlas_mean # Anopheles funestus
                        aa.bf <- atlas_mean # Anopheles arabiensis
                        am.bf <- atlas_mean # Anopheles merus
                               
                        
                        # save sc
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas/")
                        write.csv(sc.bf,'sc.bf.csv', row.names = F)
                      
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas/")
                        sc.bf <- read.csv('sc.bf.csv', header = T)
                        
                        # save g6pd
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas/")
                        write.csv(g6pd.bf,'g6pd.bf.csv', row.names = F)
                        
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas/")
                        gp6d.bf <- read.csv('g6pd.bf.csv', header = T)
    #### END ####

#### NUTRITION - COHORT ####
    
# children          
              
  setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Child anthropometry 2014/")
  pvsca01 <- read.dta('pvsca01.dta')  # 2014 child anthropometry data
      
  setwd("/Users/Elizabeth/Documents/PIVOT_R/2016")
  pvsiq022 <- read.dta('pvsiq022.dta')  # 2016 individual data
  
  setwd("/Users/Elizabeth/Documents/PIVOT_R/2018")
  pvsiq032 <- read.dta('INDIVIDU.dta')  # 2018 individual data
  colnames(pvsiq032) <- tolower(colnames(pvsiq032)) # put column names in lowercase
  
  # WAZ (weight for age z-score)
  # HAZ (height for age z-score) - stunting is a measure of chronic malnutrition
  # WHZ (weight for height z-score) - wasting is a measure of acute malnutrition. Per WHO guidelines: WHZ  WHZ < -3 SD --> SAM
  
  ch.anth14 <- dplyr::select(pvsca01, qhgrappe, qhannee, qhmois, waz, haz, whz)
      ch.anth14$stunt.modsev <- ifelse(ch.anth14$haz <= -2, 1, 0)
      ch.anth14$stunt.sev <- ifelse(ch.anth14$haz <= -3, 1, 0)
      ch.anth14$wast.modsev <- ifelse(ch.anth14$whz <= -2, 1, 0)
      ch.anth14$wast.sev <- ifelse(ch.anth14$whz <= -3, 1, 0)
      ch.anth14.bc <- as.data.frame(with (ch.anth14, tapply(stunt.modsev, qhgrappe, FUN = mean))) ; colnames(ch.anth14.bc) <- c("stunt.modsev")
      ch.anth14.bc$stunt.sev <- with (ch.anth14, tapply(stunt.sev, qhgrappe, FUN = mean))
      ch.anth14.bc$wast.modsev <- with (ch.anth14, tapply(wast.modsev, qhgrappe, FUN = mean))
      ch.anth14.bc$wast.sev <- with (ch.anth14, tapply(wast.sev, qhgrappe, FUN= mean))
      ch.anth14.bc$year <- 2014
      ch.anth14.bc$month <- 4
      ch.anth14.bc$cluster <- c(1:80)
      
  ch.anth16 <- dplyr::select(pvsiq022, qhgrappe, qhannee, qhmois, waz, haz, whz)
      ch.anth16$stunt.modsev <- ifelse(ch.anth16$haz <= -2, 1, 0)
      ch.anth16$stunt.sev <- ifelse(ch.anth16$haz <= -3, 1, 0)
      ch.anth16$wast.modsev <- ifelse(ch.anth16$whz <= -2, 1, 0)
      ch.anth16$wast.sev <- ifelse(ch.anth16$whz <= -3, 1, 0)
      ch.anth16.bc <- as.data.frame(with (ch.anth16, tapply(stunt.modsev, qhgrappe, FUN = mean, na.rm = TRUE))) ; colnames(ch.anth16.bc) <- c("stunt.modsev")
      ch.anth16.bc$stunt.sev <- with (ch.anth16, tapply(stunt.sev, qhgrappe, FUN = mean, na.rm = TRUE))
      ch.anth16.bc$wast.modsev <- with (ch.anth16, tapply(wast.modsev, qhgrappe, FUN = mean, na.rm = TRUE))
      ch.anth16.bc$wast.sev <- with (ch.anth16, tapply(wast.sev, qhgrappe, FUN= mean, na.rm = TRUE))
      ch.anth16.bc$year <- 2016
      ch.anth16.bc$month <- 8
      ch.anth16.bc$cluster <- c(1:80)
      
  ch.anth18 <- dplyr::select(pvsiq032, qhgrappe, qhannee, qhmois, waz, haz, whz)
      ch.anth18$stunt.modsev <- ifelse(ch.anth18$haz <= -2, 1, 0)
      ch.anth18$stunt.sev <- ifelse(ch.anth18$haz <= -3, 1, 0)
      ch.anth18$wast.modsev <- ifelse(ch.anth18$whz <= -2, 1, 0)
      ch.anth18$wast.sev <- ifelse(ch.anth18$whz <= -3, 1, 0)
      ch.anth18.bc <- as.data.frame(with (ch.anth18, tapply(stunt.modsev, qhgrappe, FUN = mean, na.rm = TRUE))) ; colnames(ch.anth18.bc) <- c("stunt.modsev")
      ch.anth18.bc$stunt.sev <- with (ch.anth18, tapply(stunt.sev, qhgrappe, FUN = mean, na.rm = TRUE))
      ch.anth18.bc$wast.modsev <- with (ch.anth18, tapply(wast.modsev, qhgrappe, FUN = mean, na.rm = TRUE))
      ch.anth18.bc$wast.sev <- with (ch.anth18, tapply(wast.sev, qhgrappe, FUN= mean, na.rm = TRUE))
      ch.anth18.bc$year <- 2018
      ch.anth18.bc$month <- 5
      ch.anth18.bc$cluster <- c(1:80)
  
  # plot by cluster
      
      dat <- ch.anth14.bc
      dat <- ch.anth16.bc
      dat <- ch.anth18.bc
      dat <- ind.anth14.bc
      dat <- ind.anth16.bc
      dat <- ind.anth18.bc
      
            # check distribution:
            check <- rbind(ch.anth14.bc, ch.anth16.bc, ch.anth18.bc)
            hist(check$stunt.modsev) # mod-sev stunting is pretty normallty distributed
            hist(check$wast.modsev) # mod-sev wasting is not normally distributed
            hist(check$stunt.sev) # not very normal
            hist(check$wast.sev) # not normal
      
      # load data
      fokontany = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Administrative_boundaries/Limite_FKT_Distr_Ifanadiana.shp")
      healthcare = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Health_system/Centre-de-sante_Ifanadiana.shp")
      villages = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Villages/Villages_baseline.shp")
      roads = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Roads_proj/Ifanadiana_roads_projected.shp")
      
      # make cluster_access_loc_sp using dat:
      temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
      groups = group_by(temp.df, cluster)
      cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
      cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
      cluster.centroid = data.frame(cluster.long,cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
      cluster_access_loc = merge(cluster.centroid, dat, by = "cluster")
      xy = cluster.centroid[,c(2,3)]
      cluster_access_loc_sp <- SpatialPointsDataFrame(coords = xy, data = cluster_access_loc)
      class(cluster_access_loc_sp) # spatial points data frame
      head(cluster_access_loc_sp@data)
      
      my.grid = F.grid(fokontany)
      my.grid.coords <- my.grid$grid.coords 
      my.grid.sp <- my.grid$grid.sp 
      par(mar=c(0,0,0,0))
      # plot(my.grid.sp) 
      
      m.gstat <- gstat(formula = mimc.modsev ~ 1, locations = ~Longitude  + Latitude,	data = cluster_access_loc_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance power
      
      # Plotting GSTAT
      par(mfrow=c(1,1))
      z=data.frame(predict(m.gstat, my.grid.coords))
      coordinates(z) <- c("Longitude", "Latitude") 
      gridded(z) <- TRUE
      reset_par() ; brks = seq(0, 1, 0.01) ; range(z@data$var1.pred) # to help set the scale
      my.plot = plot(z, col = rev(terrain.colors(length(brks)-1)) , breaks = brks) # if you want to set the scale
      #my.plot=image(z, col=rev(terrain.colors(15))) # if you don't care about the scale
      plot(fokontany, add = TRUE)
      #add.elements(my.plot)
      title("Child mod-sev stunting 2014", line = -1, adj = 0.1)
      
        
      
          # check for spatial autocorrelation to see if we can interpolate
          # great resource: https://www.r-bloggers.com/spatial-correlograms-in-r-a-mini-overview/
    
          # using variograms on the model
          my.vgm <- variogram(m.gstat) ; plot(my.vgm)
          
          vgm.plot <- ggplot(my.vgm, aes(dist, gamma)) + geom_point() + geom_smooth() 
          mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (12)), 
                                axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                                axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))
          
          print(vgm.plot + mynamestheme + labs(title= "Variogram", y="Semivariance", x = "Distance between points"))
          
          # using Moran I
          z <- raster(z)
          x <- MoranLocal(z)
          plot(x)
          Moran(z) # .92027
        
          # using ncf correlograms - takes too long
          ncf.cor <- ncf:correlog(z$Longitude, z$Latitude, z$var1.pred, increment = 1, resamp = 500)
          
          # using pgirmess correlograms - faster because it doesn't used a randomization test
          #pgi.cor <- pgirmess::correlog(coords= z[,1:2], z$var1.pred, method="Moran", nbclass=21)
          pgi.cor <- pgirmess::correlog(coords= cluster_access_loc_sp@data[,2:3], ind.anth18.bc$mimc.modsev, method="Moran", nbclass=21)
          plot(pgi.cor)
          pgi.cor
          
          
          
# adults
          
          setwd("/Users/Elizabeth/Documents/PIVOT_R/2014")
          pvsiq012 <- read.dta('pvsiq012.dta')  # 2014 individual data
          
          setwd("/Users/Elizabeth/Documents/PIVOT_R/2016")
          pvsiq022 <- read.dta('pvsiq022.dta')  # 2016 individual data
          
          setwd("/Users/Elizabeth/Documents/PIVOT_R/2018")
          pvsiq032 <- read.dta('INDIVIDU.dta')  # 2018 individual data
          
              # BMI 16-16.9 - moderately thin
              # BMI < 16 - severely thin
          
          ind.anth14 <- dplyr::select(pvsiq012, qhgrappe, wimc, mimc)
            ind.anth14$wimc <- ifelse(ind.anth14$wimc >= 60, NA, ind.anth14$wimc) # remove 99s
            ind.anth14$mimc <- ifelse(ind.anth14$mimc >= 60, NA, ind.anth14$mimc) # remove 99s
            ind.anth14$wimc.modsev <- with(ind.anth14, ifelse ( wimc <= 17, 1, 0))
            ind.anth14$wimc.sev <- with(ind.anth14, ifelse ( wimc <= 16, 1, 0))
            ind.anth14$mimc.modsev <- with(ind.anth14, ifelse ( mimc <= 17, 1, 0))
            ind.anth14$mimc.sev <- with(ind.anth14, ifelse ( mimc <= 16, 1, 0))
    
            ind.anth14.bc <- as.data.frame(with (ind.anth14, tapply(wimc.modsev, qhgrappe, FUN = mean, na.rm = TRUE))) ; colnames(ind.anth14.bc) <- c("wimc.modsev")
            ind.anth14.bc$wimc.sev <- with (ind.anth14, tapply(wimc.sev, qhgrappe, FUN = mean, na.rm = TRUE))
            ind.anth14.bc$mimc.modsev <- with (ind.anth14, tapply(mimc.modsev, qhgrappe, FUN = mean, na.rm = TRUE))
            ind.anth14.bc$mimc.sev <- with (ind.anth14, tapply(mimc.sev, qhgrappe, FUN= mean, na.rm = TRUE))
            ind.anth14.bc$year <- 2014
            ind.anth14.bc$month <- 4
            ind.anth14.bc$cluster <- c(1:80)
            
          ind.anth16 <- dplyr::select(pvsiq022, qhgrappe, wimc, mimc)
            ind.anth16$wimc <- ifelse(ind.anth16$wimc >= 60, NA, ind.anth16$wimc) # remove 99s
            ind.anth16$mimc <- ifelse(ind.anth16$mimc >= 60, NA, ind.anth16$mimc) # remove 99s
            ind.anth16$wimc.modsev <- with(ind.anth16, ifelse ( wimc <= 17, 1, 0))
            ind.anth16$wimc.sev <- with(ind.anth16, ifelse ( wimc <= 16, 1, 0))
            ind.anth16$mimc.modsev <- with(ind.anth16, ifelse ( mimc <= 17, 1, 0))
            ind.anth16$mimc.sev <- with(ind.anth16, ifelse ( mimc <= 16, 1, 0))
            
            ind.anth16.bc <- as.data.frame(with (ind.anth16, tapply(wimc.modsev, qhgrappe, FUN = mean, na.rm = TRUE))) ; colnames(ind.anth16.bc) <- c("wimc.modsev")
            ind.anth16.bc$wimc.sev <- with (ind.anth16, tapply(wimc.sev, qhgrappe, FUN = mean, na.rm = TRUE))
            ind.anth16.bc$mimc.modsev <- with (ind.anth16, tapply(mimc.modsev, qhgrappe, FUN = mean, na.rm = TRUE))
            ind.anth16.bc$mimc.sev <- with (ind.anth16, tapply(mimc.sev, qhgrappe, FUN= mean, na.rm = TRUE))
            ind.anth16.bc$year <- 2016
            ind.anth16.bc$month <- 4
            ind.anth16.bc$cluster <- c(1:80)
            
          colnames(pvsiq032) <- tolower(colnames(pvsiq032)) # put column names in lowercase
            ind.anth18 <- dplyr::select(pvsiq032, qhgrappe, wimc, mimc)
            ind.anth18$wimc <- ifelse(ind.anth18$wimc >= 60, NA, ind.anth18$wimc) # remove 99s
            ind.anth18$mimc <- ifelse(ind.anth18$mimc >= 60, NA, ind.anth18$mimc) # remove 99s
            ind.anth18$wimc.modsev <- with(ind.anth18, ifelse ( wimc <= 17, 1, 0))
            ind.anth18$wimc.sev <- with(ind.anth18, ifelse ( wimc <= 16, 1, 0))
            ind.anth18$mimc.modsev <- with(ind.anth18, ifelse ( mimc <= 17, 1, 0))
            ind.anth18$mimc.sev <- with(ind.anth18, ifelse ( mimc <= 16, 1, 0))
            
            ind.anth18.bc <- as.data.frame(with (ind.anth18, tapply(wimc.modsev, qhgrappe, FUN = mean, na.rm = TRUE))) ; colnames(ind.anth18.bc) <- c("wimc.modsev")
            ind.anth18.bc$wimc.sev <- with (ind.anth18, tapply(wimc.sev, qhgrappe, FUN = mean, na.rm = TRUE))
            ind.anth18.bc$mimc.modsev <- with (ind.anth18, tapply(mimc.modsev, qhgrappe, FUN = mean, na.rm = TRUE))
            ind.anth18.bc$mimc.sev <- with (ind.anth18, tapply(mimc.sev, qhgrappe, FUN= mean, na.rm = TRUE))
            ind.anth18.bc$year <- 2018
            ind.anth18.bc$month <- 4
            ind.anth18.bc$cluster <- c(1:80)
            
         # plotting: 
            dat <- ind.anth18.bc
            
            temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
            groups = group_by(temp.df, cluster)
            cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
            cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
            cluster.centroid = data.frame(cluster.long,cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
            cluster_access_loc = merge(cluster.centroid, dat, by = "cluster")
            xy = cluster.centroid[,c(2,3)]
            cluster_access_loc_sp <- SpatialPointsDataFrame(coords = xy, data = cluster_access_loc)
            class(cluster_access_loc_sp) # spatial points data frame
            head(cluster_access_loc_sp@data)
            
            my.grid = F.grid(fokontany)
            my.grid.coords <- my.grid$grid.coords 
            my.grid.sp <- my.grid$grid.sp 
            par(mar=c(0,0,0,0))
            # plot(my.grid.sp) 
            
            m.gstat <- gstat(formula = mimc.sev ~ 1, locations = ~Longitude  + Latitude,	data = cluster_access_loc_sp@data , nmax = 7, set = list(idp = 0.5)) # nmax: max points (uses 5 neighbor points); idp = inverse distance power
            
            # Plotting GSTAT
            par(mfrow=c(1,1))
            z=data.frame(predict(m.gstat, my.grid.coords))
            coordinates(z) <- c("Longitude", "Latitude") 
            gridded(z) <- TRUE
            reset_par() ; brks = seq(0, .5, 0.01) ; range(z@data$var1.pred) # to help set the scale
            my.plot = plot(z, col = rev(terrain.colors(length(brks)-1)) , breaks = brks) # if you want to set the scale
            #my.plot=image(z, col=rev(terrain.colors(15))) # if you don't care about the scale
            plot(fokontany, add = TRUE)
            #add.elements(my.plot)
            title("Men's severely thin 2018", line = -1, adj = 0.1)
            
      # testing for spatial autocorrelation to see if I can interpolate: http://rspatial.org/analysis/rst/3-spauto.html
            
            ch.anth.data <- rbind(ch.anth14.bc, ch.anth16.bc, ch.anth18.bc)
            ind.anth.data <- rbind(ind.anth14.bc, ind.anth16.bc, ind.anth18.bc)
            
            # calculating correlation - very small (0 is random, 1 is the same)
            cor(ch.anth.data$stunt.modsev, ch.anth.data$year) # -0.0857
            cor(ch.anth.data$wast.modsev, ch.anth.data$year) # -0.0564
            cor(ch.anth.data$stunt.sev, ch.anth.data$year) # -0.1222
            cor(ch.anth.data$wast.sev, ch.anth.data$year) # 0.0422025
            
            acf(ch.anth.data$stunt.modsev)
            plot(x = ch.anth.data$year, y = ch.anth.data$stunt.modsev)
            
            # calculating correlation - much bigger
            cor(ind.anth.data$wimc.modsev, ind.anth.data$year) # 0.5380376
            cor(ind.anth.data$mimc.modsev, ind.anth.data$year) # 0.6210128
            cor(ind.anth.data$wimc.sev, ind.anth.data$year) # 0.6028321
            cor(ind.anth.data$mimc.sev, ind.anth.data$year) # 0.6828731
            
            acf(ind.anth.data$mimc.modsev)
            plot(x = ind.anth.data$year, y = ind.anth.data$mimc.modsev)
            
            # turn anth data into a SPDF to assess spatial autocorrelation (Moran I)
            dat <- ch.anth14.bc
            test <- dat$`wast.modsev`
            
            temp.df = data.frame(villages@coords, villages@data ) # save the coordinates from the village shapefile + the villages data (inc. lat & long) in a data.frame called "temp.df"
            groups = group_by(temp.df, cluster)
            cluster.long = summarise(groups, mean(coords.x1, na.rm=T))    
            cluster.lat = summarise(groups, mean(coords.x2, na.rm=T))
            cluster.centroid = data.frame(cluster.long,cluster.lat[,2]) ; colnames(cluster.centroid)[c(2,3)]=c('Longitude','Latitude')
            cluster_access_loc = merge(cluster.centroid, dat, by = "cluster")
            xy = cluster.centroid[,c(2,3)]
            spdf <- SpatialPointsDataFrame(coords = xy, data = cluster_access_loc)
            head(spdf@data)
          
          
    #### END #####

#### TEMPERATURE - MODIS ####
          
            # loop through hdf (high density ) and get the deets
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS")
            all.files <- list.files() ; all.files
            
            try <- get_subdatasets('MOD11C3.A2013121.041.2013156125842.hdf')
            try # available for each month: LST_Day_CMG (land surface temperature) ; LST_Night_CMG 
            try <- MOD11C3.A2013121.041.2013156125842.hdf   
            
            # make list of files to use
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/")
            all.files=list.files() ; all.files
            hdf.ext=file_ext(all.files)=="hdf"
            hdf.files=all.files[hdf.ext]
            dir.create('tif files')
            layer.list=c('LST_Day','LST_Night')
            layer.number=c(1,6)
            
            # transform all files
            for (i in 1:length(hdf.files)){
              for (j in 1:length(layer.list)){
                # Set time
                set.time <- proc.time()
                #--------------------------------
                # Read and transform rasters
                #--------------------------------
                rdata <- hdf.files[i]
                # Get a list of subdatasets names for raster with multiple layers
                sds <- get_subdatasets(rdata)
                # Select the name of the layer to be used 
                layer.name <- sds[layer.number[j]]
                file.name=substr(hdf.files[i], start=1, stop=16)
                output.file <- paste('tif files/',file.name,'_',layer.list[j],'.tif',sep='')
                gdal_translate(src_dataset=layer.name, dst_dataset = output.file)
                
                # Get time for the loop
                get.time=proc.time() - set.time
                print(get.time)
              }
            }
            
            # Read and transform raster stack
            
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/tif files/")
            all.files=list.files() ; all.files
            tif.ext=file_ext(all.files)=="tif"
            tif.files=all.files[tif.ext]
            
            # Load the Geotiff created into R
            r.all <- stack(tif.files)
            
            # Crop Raster so that everything is much faster
            newext <- extent(45, 55, -25, -15)
            r.all <- crop(r.all, newext)
            
            # Transform coordinate system of the raster to match the one from the shapefiles
            ifanadiana <- readOGR(dsn="/Users/Elizabeth/Documents/PIVOT_R/Geo/Administrative_boundaries", layer='Limite_District_Ifanadiana')
            fokontany = readOGR("/Users/Elizabeth/Documents/PIVOT_R/Geo/Administrative_boundaries/Limite_FKT_Distr_Ifanadiana.shp")
            newproj=projection(ifanadiana)
            newproj <- projection(fokontany)
            r.all <- projectRaster(r.all, crs=newproj)
            
            temp.r.all <- r.all
            
            # Plot everything to make sure I have the right projections
            fun <- function() {
              #plot(ifanadiana, lwd=2, add=T)
              plot(fokontany, lwd = 2, add = T)
              #plot(vill.clusters,border='red',add=T)
            }
            plot(r.all,xlim=c(450000,600000),ylim= c(450000,650000),addfun=fun)
            
            # Extract raster values for each buffer and calculate mean values
            r.vals <- raster::extract(r.all, fokontany, weights=T, fun=mean, df=T) 
            head(r.vals) # contains average day and night temp for each fkt in Kelvin
            
            # save it so you don't have to redo all of this the next time R crashes
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/")
            write.csv(r.vals,'MOD11C3 cluster information_v006.csv', row.names = F) # the file without v006 is v041 until like April 2016
            
            # reopen if needed lol
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/")
            r.vals <- read.csv('MOD11C3 cluster information_v006.csv', header = T)
            
            # tidy up column names to facilitate easier gather long
                colnames(r.vals) <- tolower(colnames(r.vals))
                colnames(r.vals) <- str_replace(colnames(r.vals), "mod11c3.a", "")
                colnames(r.vals) <- str_replace(colnames(r.vals), "lst_", "")
            
            # baller gather long
            r.vals.l <- tidyr::gather(r.vals, key = "date", value = "temp", "2013001_day":"2018244_night", factor_key = TRUE, na.rm = FALSE)
            r.vals.l$ID <- r.vals.l$id - 1 ; r.vals.l <- r.vals.l[,-1] # because by convention, I number fkts from 0-194, not 1-195
            
            # pull out dates
            r.vals.l$date <- as.character(r.vals.l$date)
            r.vals.l$year <- substr(r.vals.l$date, 1, 4) ; r.vals.l$year <- as.numeric(r.vals.l$year)
            r.vals.l$time <- substr(r.vals.l$date, 9, 9) # d/n for day/night
            r.vals.l$mo <- substr(r.vals.l$date, 5, 7) ; r.vals.l$mo <- as.numeric(r.vals.l$mo)
            
                  # get month from confusing naming convention: .A2006001 - Julian Date of Acquisition (A-YYYYDDD) --> so the 3 numbers represent the day of the year the data was acquired
                  unique(r.vals.l$month) # 0.03333333  4.03333333  5.06666667  6.06666667  7.10000000  8.13333333  9.13333333 10.16666667  1.06666667  2.00000000  3.03333333 11.16666667  4.06666667  5.10000000  6.10000000  7.13333333  8.16666667  9.16666667 10.20000000 11.20000000
                  table(r.vals.l$mo, r.vals.l$year)
                  r.vals.l$month <- round( ( r.vals.l$mo / 30) ,0) + 1
                  table(r.vals.l$month, r.vals.l$year)
            
            # checking some basics
            tapply(r.vals.l$temp, r.vals.l$time, mean, na.rm = TRUE) # ave day: 298 K = 76 F. ave night: 288 K = 58 F. 
            tapply(r.vals.l$temp, r.vals.l$month, mean, na.rm = TRUE) # warmest: Nov & Dec. coldest: July.
            tapply(r.vals.l$temp, r.vals.l$ID, mean) 
                  
            # tidy and spread by day/night
            r.vals.l <- dplyr::select(r.vals.l, ID, year, month, time, temp)
            r.vals.w <- tidyr::spread(r.vals.l, key = "time", value = "temp" )
            
            temp.modis <- r.vals.w
            #r.vals.w <- temp.modis
            
            # convert from kelvin (lol) in Celsius:
            temp.modis$d.c <- temp.modis$d - 273.15
            temp.modis$n.c <- temp.modis$n - 273.15
            
                  head(temp.modis) # DAY AND NIGHT TEMP AVERAGES BY FKT BY MONTH IN KELVIN & CELSIUS
                  
                  # save it so you don't have to redo all of this the next time R crashes
                  setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/")
                  write.csv(temp.modis,'MOD11C3 cluster information_v006.csv', row.names = F) # file without v006 is v041 (obselete)
                  
                  # reopen if needed lol
                  setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/")
                  temp.modis <- read.csv('MOD11C3 cluster information_v006.csv', header = T)
                  
    # confirm that there are no missing dates:
    table(temp.modis$year, temp.modis$month) # missing 2016/04
                  
            # plot a couple to see
            
            t.fkt <- tapply(r.vals.l$temp, r.vals.l$ID, mean) 
            t.fkt <- tapply( subset(r.vals.l$temp, r.vals.l$time == "d"), subset(r.vals.l$ID, r.vals.l$time == "d"), mean) 
            
            newpoly <- fokontany
            newpoly@data$a.r_mean <- t.fkt
            manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
            #manual.col = colorRampPalette(c("white","blue"))
            color.match = manual.col(length(unique(newpoly@data$a.r_mean))) # find a shade for each fokontany
            lookupTable = sort(unique(newpoly@data$a.r_mean)) # sort the access values
            newpoly@data$color = color.match[match(newpoly@data$a.r_mean, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
            plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
            title(paste0("Average day temp by fkt (Kelvin)"), cex.main = 1.75)
            #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
            legend("topleft", legend = c(round(min(newpoly@data$a.r_mean, na.rm = TRUE),0),round(max(newpoly@data$a.r_mean, na.rm = TRUE),0)), fill = c("#f7f6fd","#4635d0"), cex = 1.5)
            
            # plot day temperature over time
            head(r.vals.w)
            r.vals.w$date <- as.Date(paste(r.vals.w$year, r.vals.w$month,1, sep = "-"))
            
            mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20)), 
                                  axis.title = element_text(family = "Helvetica", size = (15), colour = "steelblue4"),
                                  axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (15)))
            plot <- ggplot(r.vals.w, aes(date,n)) + geom_smooth() +  geom_point()
            
            myplot <- print(plot + mynamestheme + labs(title= "Average night temp over time",
                                                        y="Night land surface temperature", x = "Date"))
            
           
            
    #### END ####

#### VEGETATION INDEX - MODIS ####
            
            # loop through hdf (high density ) and get the deets
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS")
            all.files <- list.files() ; all.files
            
            try <- get_subdatasets('MOD13A3.A2013001.h22v11.006.2015254151634.hdf')
            try # available for each month: 1 km monthly NDVI, 1 km monthly EVI
             
            # make list of files to use
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS")
            all.files=list.files() ; all.files
            hdf.ext=file_ext(all.files)=="hdf"
            hdf.files=all.files[hdf.ext]
            dir.create('tif files2')
            layer.list=c('NDVI','EVI')
            layer.number=c(1,2)
            
            # transform all files
            for (i in 1:length(hdf.files)){
              for (j in 1:length(layer.list)){
                # Set time
                set.time <- proc.time()
                # Read and transform rasters
                rdata <- hdf.files[i]
                # Get a list of subdatasets names for raster with multiple layers
                sds <- get_subdatasets(rdata)
                # Select the name of the layer to be used 
                layer.name <- sds[layer.number[j]]
                file.name=substr(hdf.files[i], start=1, stop=16)
                output.file <- paste('tif files2/',file.name,'_',layer.list[j],'.tif',sep='')
                gdal_translate(src_dataset=layer.name, dst_dataset = output.file)
                # Get time for the loop
                get.time=proc.time() - set.time
                print(get.time)
              }
            }
            
            # Read and transform raster stack
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS/tif files2")
            all.files=list.files()
            tif.ext=file_ext(all.files)=="tif"
            tif.files=all.files[tif.ext]
            
            # Load the Geotiff created into R
            r.all <- stack(tif.files)
            
            # Transform coordinate system of the raster to match the one from the shapefiles
            ifanadiana <- readOGR(dsn="/Users/Elizabeth/Documents/PIVOT_R/Geo/Administrative_boundaries", layer='Limite_District_Ifanadiana')
            newproj <- projection(ifanadiana)
     r.all <- projectRaster(r.all, crs=newproj) # TAKES FOREVER!!!
     
                    veg.r.all <- r.all # so you can investivate 
                    r.all <- veg.r.all
            
            # Plot everything to make sure I have the right projections
            fun <- function() {
              #plot(ifanadiana, lwd=2, add=T)
              plot(fokontany, lwd = 2, add = T)
              #plot(vill.clusters,border='red',add=T)
            }
            plot(r.all,xlim=c(450000,600000),ylim= c(450000,650000),addfun=fun)
            
            # Extract raster values for each buffer and calculate mean values
            r.vals <- raster::extract(r.all, fokontany, weights=T, method = 'simple', fun=mean, df=T, na.rm = TRUE) 
            head(r.vals) # contains average day and night temp for each fkt in Kelvin
            
            
            # save it so you don't have to redo all of this the next time R crashes
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS/")
            write.csv(r.vals,'MOD11C3 cluster information.csv', row.names = F)
            
            # reopen if needed lol
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS/")
            r.vals <- read.csv('MOD11C3 cluster information.csv', header = F)
            
            # tidy up column names to facilitate easier gather long
            colnames(r.vals) <- tolower(colnames(r.vals))
            colnames(r.vals) <- str_replace(colnames(r.vals), "mod13a3.a", "")
            
            
            # baller gather long
            r.vals.l <- tidyr::gather(r.vals, key = "date", value = "index", "2013001_evi":"2018244_ndvi", factor_key = TRUE, na.rm = FALSE)
            r.vals.l$ID <- r.vals.l$id - 1 ; r.vals.l <- r.vals.l[,-1] # because by convention, I number fkts from 0-194, not 1-195
            
            # pull out dates
            r.vals.l$date <- as.character(r.vals.l$date)
            r.vals.l$year <- substr(r.vals.l$date, 1, 4) ; r.vals.l$year <- as.numeric(r.vals.l$year)
            r.vals.l$evi.ndvi <- substr(r.vals.l$date, 9, 9) # e/n for evi/ndvi
            r.vals.l$mo <- substr(r.vals.l$date, 5, 7) ; r.vals.l$mo <- as.numeric(r.vals.l$mo)
            
            # get month from confusing naming convention: .A2006001 - Julian Date of Acquisition (A-YYYYDDD) --> so the 3 numbers represent the day of the year the data was acquired
            unique(r.vals.l$mo) #1  32  60  91 121 152 182 213 244 274 305 335  61  92 122 153 183 214 245 275 306 336
            table(r.vals.l$mo, r.vals.l$year)
            r.vals.l$month <- round( ( r.vals.l$mo / 30) ,0) + 1
            table(r.vals.l$month, r.vals.l$year)
            
            # checking some basics
            tapply(r.vals.l$index, r.vals.l$time, mean) # ave day: 298 K = 76 F. ave night: 288 K = 58 F. 
            tapply(r.vals.l$temp, r.vals.l$month, mean) # warmest: Nov & Dec. coldest: July.
            tapply(r.vals.l$temp, r.vals.l$ID, mean) 
            
            # tidy and spread by day/night
            r.vals.l <- dplyr::select(r.vals.l, ID, year, month, evi.ndvi, index)
            r.vals.w <- tidyr::spread(r.vals.l, key = "evi.ndvi", value = "index" )
            
            head(r.vals.w) # DAY AND NIGHT TEMP AVERAGES BY FKT IN KELVIN
            
            veg.modis <- r.vals.w
            colnames(veg.modis) <- c("ID", "year", "month", "evi", "ndvi")
            
                        head(veg.modis) # AVERAGE EVI AND NDVI BY MONTH BY FKT 
                        
                        # save it so you don't have to redo all of this the next time R crashes
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS/")
                        write.csv(veg.modis,'MOD11C3 cluster information2.csv', row.names = F)
                        
                        # reopen if needed lol
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS/")
                        veg.modis <- read.csv('MOD11C3 cluster information2.csv', header = T)
                        
            
            # plot a couple to see
            
            t.fkt <- tapply(veg.modis$ndvi, veg.modis$ID, mean) 
            
            t.fkt <- tapply( subset(veg.modis$temp, veg.modis$time == "d"), subset(veg.modis$ID, veg.modis$time == "d"), mean) 
            
            newpoly <- fokontany
            newpoly@data$a.r_mean <- t.fkt
            manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
            #manual.col = colorRampPalette(c("white","blue"))
            color.match = manual.col(length(unique(newpoly@data$a.r_mean))) # find a shade for each fokontany
            lookupTable = sort(unique(newpoly@data$a.r_mean)) # sort the access values
            newpoly@data$color = color.match[match(newpoly@data$a.r_mean, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
            plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
            title(paste0("Average NDVI by fkt"), cex.main = 1.75)
            #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
            legend("topleft", legend = c(round(min(newpoly@data$a.r_mean),0),round(max(newpoly@data$a.r_mean),0)), fill = c("#f7f6fd","#4635d0"), cex = 1.5)
            
            # plot EVI over time
            head(veg.modis)
            veg.modis$date <- as.Date(paste(veg.modis$year, veg.modis$month,1, sep = "-"))
                                            
            mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20)), 
                                  axis.title = element_text(family = "Helvetica", size = (15), colour = "steelblue4"),
                                  axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (15)))
            
            
            plot <- ggplot(veg.modis, aes(date,ndvi)) +  geom_point() + geom_smooth() 
            
            myplot <- print(plot + mynamestheme + labs(title= "Average NDVI over time",
                                                       y="EVI", x = "Date"))
            
            # mean evi/ndvi by month:
            par(mfrow=c(2,6))
            for (i in 1:12) {
              
              t.fkt.2 <- subset(veg.modis, veg.modis$month == i)
              t.fkt <- tapply(t.fkt.2$ndvi, t.fkt.2$ID, mean)
              
              newpoly <- fokontany
              newpoly@data$a.r_mean <- t.fkt
              manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
              #manual.col = colorRampPalette(c("white","blue"))
              color.match = manual.col(length(unique(newpoly@data$a.r_mean))) # find a shade for each fokontany
              lookupTable = sort(unique(newpoly@data$a.r_mean)) # sort the access values
              newpoly@data$color = color.match[match(newpoly@data$a.r_mean, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
              plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
              title(paste0("Average NDVI: month ", i), cex.main = 1)
              #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
              legend("topleft", legend = c(round(min(newpoly@data$a.r_mean),0),round(max(newpoly@data$a.r_mean),0)), fill = c("#f7f6fd","#4635d0"), cex = 1)
              
            }
            
            
    #### END ####
            
#### VEGETATION - DROPBOX ####
            
            dpath <-"/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation/w001001.adf"
            y <- new("GDALReadOnlyDataset", dpath)
            getDriverLongName(getDriver(y))
            yy<-asSGDF_GROD(y, output.dim=c(200, 200))
            class(yy) # SpatialGridDataFrame
            # simple plot:
            plot(yy)
            title("w001001.adf - Vegetation")
            head(yy@data) # band1 looks full?
            
    #### END ####
            
#### RAINFALL - TAMSAT ####

            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Rainfall-TAMSAT/Total Rainfall/")
            all.files <- list.files() ; all.files
            
            r <- raster("rfe2014_01.v3.nc") # data is rainfall in mm & can be exported as rasters
            plot(r) # total rainfall in all of africa lol
            
            
            r_all <- as.data.frame(c(0:194)) ; colnames(r_all) <- c("ID")
            
            for (i in 1:length(all.files)){
              
                        # Set time
                        set.time <- proc.time()
                
                r <- raster(all.files[i])
                
                r_bf <- raster::extract(r, fokontany, method = 'simple') # extract raster values within each polygon to a list object
                r_mean <- as.data.frame(lapply(r_bf, FUN = mean, na.rm = TRUE)) # find mean raster value for each polygon
                r_mean <- as.data.frame(reshape(r_mean, varying = colnames(r_mean), v.names = "r_bf", direction = "long"))
                r_mean$ID <- c(0:194) 
                
                r_mean <- r_mean[,-1]
                r_mean <- r_mean[,-2]
                
                colnames(r_mean) <- c(paste(all.files[i]), "ID")
                
                r_all <- merge(r_all, r_mean, by = c("ID"))
                
                print(head(r_all)) # so you can see what's happening
                
                          # Get time for the loop
                          get.time=proc.time() - set.time
                          print(get.time)
                        }
            
            # save it so you don't have to redo all of this the next time R crashes
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Rainfall-TAMSAT/")
            write.csv(r_all,'Total rainfall by fkt.csv', row.names = F)
            
            # reopen if needed lol
            setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Rainfall-TAMSAT/")
            r_all <- read.csv('Total rainfall by fkt.csv', header = T)
            
            # make column names as regular as possible
            colnames(r_all) <- str_replace(colnames(r_all), "rfe", "")
            colnames(r_all) <- str_replace(colnames(r_all), ".v3.nc", "")
            
            # baller gather long
            r_all.l <- tidyr::gather(r_all, key = "date", value = "rf", "2014_01":"2017_12", factor_key = TRUE, na.rm = FALSE)
            
            # pull out dates
            r_all.l$year <- str_extract(r_all.l$date, '(?<=)\\d+') ; r_all.l$year <- as.numeric(r_all.l$year)
            r_all.l$month <- str_extract(r_all.l$date, '(?<=_)\\d+') ; r_all.l$month <- as.numeric(r_all.l$month)
            str(r_all.l)
            
            r_all.l <- dplyr::select(r_all.l, id, year, month, rf)
            colnames(r_all.l) <- c("ID", "year", "month", "rf")
            
            rainfall.bf <- r_all.l
            
                        head(rainfall.bf) # RAINFALL BY FKT 
                        
                        # save it so you don't have to redo all of this the next time R crashes
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Rainfall-TAMSAT/")
                        write.csv(rainfall.bf,'Total rainfall by fkt2.csv', row.names = F)
                        
                        # reopen if needed lol
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Rainfall-TAMSAT/")
                        rainfall.bf <- read.csv('Total rainfall by fkt2.csv', header = T)
            
            # plot rainfall over time
                
                        # rainfall over time
                        r_all.l$date <- as.Date(paste(r_all.l$year, r_all.l$month,1, sep = "-"))
                        
                        mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20)), 
                                              axis.title = element_text(family = "Helvetica", size = (15), colour = "steelblue4"),
                                              axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (15)))
                        plot <- ggplot(r_all.l, aes(date,rf)) + geom_smooth() +  geom_point()
                        
                        myplot <- print(plot + mynamestheme + labs(title= "Total rainfall by month over time",
                                                                   y="Rainfall (mm)", x = "Date"))
                      
                        # mean rainfall by month
                        t.fkt <- tapply(r_all.l$rf, r_all.l$month, mean) 
                        plot(t.fkt, pch = 16) ; title("Mean total rainfall by month in Ifanadiana")
                        
                        # mean rainfall by fkt
                        t.fkt <- tapply(r_all.l$rf, r_all.l$ID, mean) 
                        
                        # mean rainfall by fkt by season
                        r_all.l$season <- with(r_all.l, ifelse(month == 10 | month == 11 | month == 12 | month == 1 | month == 2 | month == 3, 1, 2))
                        t.fkt.2 <- subset(r_all.l, r_all.l$season == 2)
                        t.fkt <- tapply(t.fkt.2$rf, t.fkt.2$ID, mean)
                        
                        newpoly <- fokontany
                        newpoly@data$a.r_mean <- t.fkt
                        manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
                        #manual.col = colorRampPalette(c("white","blue"))
                        color.match = manual.col(length(unique(newpoly@data$a.r_mean))) # find a shade for each fokontany
                        lookupTable = sort(unique(newpoly@data$a.r_mean)) # sort the access values
                        newpoly@data$color = color.match[match(newpoly@data$a.r_mean, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
                        plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
                        title(paste0("Average total rainfall by month (mm) during non-rainy season (months 4-9)"), cex.main = 1)
                        #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
                        legend("topleft", legend = c(round(min(newpoly@data$a.r_mean),0),round(max(newpoly@data$a.r_mean),0)), fill = c("#f7f6fd","#4635d0"), cex = 1.5)
                        
                        # mean rainfall by month:
                        par(mfrow=c(2,6))
                        for (i in 1:12) {
                          
                          t.fkt.2 <- subset(r_all.l, r_all.l$month == i)
                          t.fkt <- tapply(t.fkt.2$rf, t.fkt.2$ID, mean)
                          
                          newpoly <- fokontany
                          newpoly@data$a.r_mean <- t.fkt
                          manual.col = colorRampPalette(c("#f7f6fd","#4635d0")) # set the color spectrum
                          #manual.col = colorRampPalette(c("white","blue"))
                          color.match = manual.col(length(unique(newpoly@data$a.r_mean))) # find a shade for each fokontany
                          lookupTable = sort(unique(newpoly@data$a.r_mean)) # sort the access values
                          newpoly@data$color = color.match[match(newpoly@data$a.r_mean, lookupTable)] # match colors to sorted unique values & assign them to a new column in the polygon data so that they plot smallest values as lightest and largest values as darkest
                          plot(newpoly, col = newpoly@data$color, border= "light grey", lwd = 0.5)
                          title(paste0("Average total rainfall: month ", i), cex.main = 1)
                          #legend("right", legend = c(round(min(newpoly@data$a.r_mean),2),round(max(newpoly@data$a.r_mean),2)), col = c("white","blue"), pch = 19)
                          legend("topleft", legend = c(round(min(newpoly@data$a.r_mean),0),round(max(newpoly@data$a.r_mean),0)), fill = c("#f7f6fd","#4635d0"), cex = 1)
                          
                        }
                        
            
            # finding missing data :(
              View(with(rainfall.bf, aggregate(rf ~ year + month, FUN = "mean")))
            
    #### END ####
            
#### COMBINING ALL COVARIATES ####
    
              # social/economic
              
                        # wealth by hh:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Wealth/PCA/")
                        w.hh.bf <- read.csv('Household wealth by fkt.csv', header = T)
                        head(w.hh.bf)
                        
                        # population density:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Malaria/")
                        malaria <- read.csv("malaria_rawdata.csv", header = TRUE) 
                        malaria$Density_pop_sqKm <- ifelse(malaria$Density_pop_sqKm == "#DIV/0!", NA, malaria$Density_pop_sqKm)
                        
                        for (i in 1:nrow(malaria)){
                          # 2015 is an increase in 2.8% from 2014, so solving gives me this
                          if(malaria$year[i]==2014){malaria$Population[i]=round(0.9727626*malaria$Population[i],0)}else{
                            # 2016 is an increase in 2.8% from 2015, so this is quite simple
                            if(malaria$year[i]==2016){malaria$Population[i]=round(1.028*malaria$Population[i],0)}else{
                              # 2017 is an increase in 2.8% from 2016, so solving from 2015 gives me this
                              if(malaria$year[i]==2017){malaria$Population[i]=round(1.056784*malaria$Population[i],0)}
                            }}
                        }
                        
                        malaria$Population.u5 <- round(malaria$Population*0.18,0) # add updates kids pop
                        malaria$Population.over5 <- round(malaria$Population*0.82,0) # add updates adults pop
                        
                        malaria$Density_pop_sqKm <- malaria$Population / malaria$Area_sqKm
                        popdensity <- dplyr::select(malaria, ID, year, month, Density_pop_sqKm, Population, Population.u5, Population.over5)
                        #tapply(malaria$Population,malaria$year,sum)/12
                        
                        head(popdensity)
                        
                        # bednet usage:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Bednets/")
                        bednets.bs.bd <- read.csv('bednets.bs.bd.csv', header = T)
                        head(bednets.bs.bd)
                        
              # environmental
                        
                        # altitude:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Altitude/")
                        altitude.bf <- read.csv('altitude.bf.csv', header = T)
                        head(altitude.bf)
                        
                        # slope:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Slope/")
                        slope.deg.bf <- read.csv('slope.deg.bf.csv', header = T)
                        head(slope.deg.bf)
                        
                        # hydrology:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Hydrology/")
                        upcell.bf <- read.csv('upcell.bf.csv', header = T)
                        head(upcell.bf)
                        
                        # ever forest lost (2000-2014):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
                        yfl.ever <- read.csv('yfl.ever.csv', header = T)
                        
                        # recent forest loss (2007-2014):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
                        yfl.rec <- read.csv('yfl.rec.csv', header = T)
                        
                        # non-recent forest loss (2000-2007):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
                        yfl.past <- read.csv('yfl.past.csv', header = T)
                        
                        # tree cover in the year 2000:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
                        tc.bf <- read.csv('tc.bf.csv', header = T)
                        head(tc.bf)
                        
                        # forest cover loss:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Deforestation/")
                        fcl.bf <- read.csv('fcl.bf.csv', header = T)
                        head(fcl.bf)
                        
                        # day and night temperature (C):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Temperature-MODIS/")
                        temp.modis <- read.csv('MOD11C3 cluster information_v006.csv', header = T)
                        head(temp.modis)
                        
                        # vegetation (EVI, NDVI):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Vegetation-MODIS/")
                        veg.modis <- read.csv('MOD11C3 cluster information2.csv', header = T)
                        head(veg.modis)
                        
                        # rainfall:
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Rainfall-TAMSAT/")
                        rainfall.bf <- read.csv('Total rainfall by fkt2.csv', header = T)
                        head(rainfall.bf)
                        
              # medical
                        
                        # sickel cell (lol):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas/")
                        sc.bf <- read.csv('sc.bf.csv', header = T)
                        colnames(sc.bf) <- c("sc.bf", "ID")
                        head(sc.bf)
                        
                        # g6pd (lol):
                        setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/Malaria atlas/")
                        g6pd.bf <- read.csv('g6pd.bf.csv', header = T)
                        colnames(g6pd.bf) <- c("g6pd.bf", "ID")
                        head(g6pd.bf)
                        
              # combining them:
                        
              cov <- merge(w.hh.bf, popdensity, by = c("ID", "year", "month"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, bednets.bs.bd, by = c("ID", "year", "month"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, altitude.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, slope.deg.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, upcell.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, yfl.ever, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, yfl.rec, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, yfl.past, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, tc.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, fcl.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, temp.modis, by = c("ID", "year", "month"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, veg.modis, by = c("ID", "year", "month"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, rainfall.bf, by = c("ID", "year", "month"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, sc.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              cov <- merge(cov, g6pd.bf, by = c("ID"), all.x = TRUE, all.y = TRUE)
              
              cov$date <- as.Date(paste(cov$year, cov$month, 01, sep = "-"))
              
              setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/")
              write.csv(cov,'cov.csv', row.names = F)
            
              setwd("/Users/Elizabeth/Documents/PIVOT_R/Covariates/")
              cov <- read.csv('cov.csv', header = T)
              
                      # adding malaria estimates:
                      setwd("/Users/Elizabeth/Documents/PIVOT_R/Malaria/model")
                      dat <- read.csv('corrected.pools.rescales_jan.csv')
                      isabel <- dplyr::select(dat, ID, year, month, closest.csb, real.dist.csb, palu.pc.2nn_0.01, palu.und5.pc.4nn_0.01)
                    
                      cov_isabel <- merge(cov, isabel, by = c("ID", "year", "month"), all.x = TRUE, all.y = TRUE)  
                      
                      setwd("/Users/Elizabeth/Documents/PIVOT_R/Malaria/model")
                      write.csv(cov_isabel, 'corrected.pools.rescales_jan_forIsabel.csv', row.names = F)
                      
                      test <- read.csv('corrected.pools.rescales_jan_forIsabel.csv')
              
                        
    #### END ####
                  