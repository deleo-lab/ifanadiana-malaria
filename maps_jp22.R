library('lme4')
library('plotly')
library('gganimate')
library('ggplot2')
library('sf')
library('tmap')
library('tmaptools')
library('leaflet')
library('dplyr')
library('transformr')
library('png')
library('gifski')
library('ggsn')
library('ggpubr')

setwd("~/Documents/Stanford/Research /Malaria Project/Data/csv_datasets")
predictors <- read.csv('malaria_updated_nov10.csv')

setwd("~/Documents/Stanford/Research /Malaria Project/Data/spatial_datasets/Limite FKT")
fkt.bound <- st_read('Limite_FKT_Distr_Ifanadiana.shp')

setwd("~/Documents/Stanford/Research /Malaria Project/Data/maps")

predictors$total.month <- 0

predictors$total.month[predictors$year == 2014] <- predictors$month[predictors$year == 2014]
predictors$total.month[predictors$year == 2015] <- predictors$month[predictors$year == 2015] + 12
predictors$total.month[predictors$year == 2016] <- predictors$month[predictors$year == 2016] + 24
predictors$total.month[predictors$year == 2017] <- predictors$month[predictors$year == 2017] + 36

joined <- inner_join(predictors, fkt.bound)
str(joined)

pred.one.month <- joined[joined$year == 2014 & joined$month == 1,]

months <- c("January","February","March","April","May","June","July","August","September","October","November","December")
dummy <- toString(3:50)

for (i in 1:length(joined$ID)) {
  
  m <- months[joined$month[i]]
  #print(m)
  year <- toString(joined$year[i])
  #print(year)
  joined$date[i] <- paste(m,year)
  
}

joined$date <- factor(joined$date, levels = unique(joined$date))

# Forest cover

forest.cover <- ggplot(pred.one.month) + 
  geom_sf(aes(fill = forest.cover, geometry = geometry))+
  blank() +
  scale_fill_gradient(low = 'green', high = 'black') + 
  labs( fill = 'Forest cover (%)') 

forest.gain <- ggplot(pred.one.month) + 
  geom_sf(aes(fill = forest.gain, geometry = geometry))+
  blank() +
  scale_fill_gradient(low = 'green', high = 'black') + 
  labs( fill = 'Forest gain (%)') 

ggarrange(forest.cover, forest.gain)


# Forest loss

pred.one.month <- joined[joined$month == 1,]

forest.loss.2014 <- ggplot(pred.one.month) + 
  geom_sf(aes(fill = loss.year.of, geometry = geometry))+
  blank() +
  scale_fill_gradient(low = 'white', high = 'red') + 
  labs( fill = 'Forest loss (%)') +
  facet_grid(cols = vars(year))


forest.loss.2014

# Monthly precipitation
p <- ggplot(joined) + geom_sf(aes(fill = monthly.p, geometry = geometry)) +
  labs(title = "{closest_state}", fill = "Precipitation (mm)") +
  scale_fill_gradient(low = 'white', high = '#0066CC') +
  transition_states(date,
                    transition_length = 2,
                    state_length = 1) 

animate(p, duration = 48)

anim_save('precip.gif', last_animation())


# Monthly max temperature
temp_max <- ggplot(joined) + geom_sf(aes(fill = monthly.max.t, geometry = geometry)) +
  labs(title = "{closest_state}", fill = "Max temperature (Celsius)") +
  scale_fill_gradient(low = 'white', high = 'red') +
  transition_states(date,
                    transition_length = 2,
                    state_length = 1)

animate(temp_max, duration = 48)

anim_save('temp_max.gif', last_animation())


# Monthly min temperature

temp_min <- ggplot(joined) + geom_sf(aes(fill = monthly.min.t, geometry = geometry)) +
  labs(title = "{closest_state}", fill = "Min temperature (Celsius)") +
  scale_fill_gradient(low = 'blue', high = 'white') +
  transition_states(date,
                    transition_length = 2,
                    state_length = 1)

animate(temp_min, duration = 48)

anim_save('temp_in.gif', last_animation())

# Malaria incidence

malaria.total <- ggplot(joined) + geom_sf(aes(fill = palu.pc.2nn_0.08, geometry = geometry)) +
  labs(title = "{closest_state}", fill = "Malaria incidence") +
  transition_states(date,
                    transition_length = 2,
                    state_length = 2) +
  scale_fill_distiller(palette = "Spectral")

animate(malaria.total, duration = 40)

anim_save('malaria.total.gif', last_animation())
