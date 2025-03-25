
library(lubridate)
library(tsibble)
library(zoo)
library(tidyverse)
library(magrittr)

# open the table with time and xy positions, use replace section to replace individual variabels faster
setwd("D:/PHD/ROV/ROV raw video density estimates/navigation_smoothing")
read_csv("20210107-093101 COMServer_matchedtovideo.csv") -> navigation

# select the relevant columns
navigation %<>% select(Sperre_LOG_DATETIME, Sperre_Depth, Sperre_Heading, Survey_ROV_LON, Survey_ROV_LAT, Survey_LOG_RELTIME) 

# make sure XY columns are numeric by removing |xb0 (for navigation files with symbols of degree and minutes)
navigation$Survey_ROV_LON <- str_remove(navigation$Survey_ROV_LON, pattern = "\xb0")
navigation$Survey_ROV_LAT <- str_remove(navigation$Survey_ROV_LAT, pattern = "\xb0")

navigation %>% mutate(Survey_ROV_LON2 = as.numeric(Survey_ROV_LON), Survey_ROV_LAT2 =as.numeric(Survey_ROV_LAT)) -> navigation
view(navigation) ### why is as numeric removing the last digits of the Latitude and longitude?
# fill the gaps in the shp file
# time managment
navigation %>% 
  # make the time column - if you have a data and a time column
  mutate(mtime = Sperre_LOG_DATETIME)-> maskxy
# if you have a dmy_hms character strimg
#mutate( mtime = dmy_hms(time)) -> maskxy


# create the missing time stamps so that you have a reading at each second 
maskxy %>% select(mtime, Survey_LOG_RELTIME,Survey_ROV_LON2,Survey_ROV_LAT2, Sperre_LOG_DATETIME, Sperre_Depth, Sperre_Heading) %>% 
  as_tsibble(index = mtime) %>%
  fill_gaps() %>% # automatically creates the missing timestamps
  as_tibble() %>% 
  
  
  
  # interpolate the coordinates values for the timestamps you have created
  # these wont be accurate but you will replace them soon
  # they are still a basic interpolation of the closest coordinates 
  mutate(LON2 = zoo::na.approx(as.vector(Survey_ROV_LON2)),
         LAT2 = zoo::na.approx(as.vector(Survey_ROV_LAT2))) -> maskxy  
# note this might also be enough if your track loog good

maskxy %>%  filter( is.na(LAT2) ) # there shouldn???t be any NA left - but check does no harm
is.na(maskxy$LAT2)

# Smooth the track 
t <- 1:nrow(maskxy)
x <- maskxy$LON2
y <- maskxy$LAT2
# Fit a cubic smoothing spline to each dimension
sx <- smooth.spline(t, x, df = 50, spar = 0.3, cv = TRUE)
sy <- smooth.spline(t, y, df = 50, spar = 0.3, cv = TRUE)

sx
sy

# plot the recorded tracks 
plot(x,y, cex = 0.25, col = "black", main = paste0("Difference of un-smoothed and smoothed ROV transect "))
# overlay the smoothed track
lines(sx[[2]], sy[[2]], col = 2, lwd = 2)

legend("topleft", legend=c("smoothed", "un-smoothed"),
       col=c("red", "black"), lty=1:2, cex=0.8)

# add the new coordinates to the table
tibble( xsmoothed = sx[[2]] ,ysmoothed = sy[[2]] )  %>%
  bind_cols(maskxy,.) -> smoothed_navigation

write_csv( smoothed_navigation, "smoothed_navigation.csv")
