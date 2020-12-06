library(tidyverse)

p <- read.csv('precipitation_monthly.csv')
tmin <- read.csv('minT_monthly.csv')
tmax <- read.csv('maxT_monthly.csv')

#Remove useless variables
p$LIB_P = NULL
p$LIB_R = NULL
p$LIB_D = NULL
p$LIB_C = NULL

tmin$LIB_P = NULL
tmin$LIB_R = NULL
tmin$LIB_D = NULL
tmin$LIB_C = NULL

tmax$LIB_P = NULL
tmax$LIB_R = NULL
tmax$LIB_D = NULL
tmax$LIB_C = NULL

# Wide to tall format and adjust IDs
p_tall <- p %>%
  gather(names(p)[3:50], key = Date, value = Precipitation)

p_tall$OBJEC = p_tall$OBJEC - 1


tMax_tall <- tmax %>%
  gather(names(tmax)[3:50], key = Date, value = maxT)

tMax_tall$OBJEC <- tMax_tall$OBJEC - 1


tMin_tall <- tmin %>%
  gather(names(tmin)[3:50], key = Date, value = minT)

tMin_tall$OBJEC <- tMin_tall$OBJEC - 1


# Save
write.csv(p_tall,'p_tall.csv')
write.csv(tMax_tall,'tMax_tall.csv')
write.csv(tMin_tall,'tMin_tall.csv')
