
library(lubridate)
library(tsibble)
library(zoo)
library(tidyverse)
library(magrittr)

# open the table with time and xy positions, use replace section to replace individual variabels faster
setwd("D:/PHD/ROV/ROV raw video density estimates/navigation_smoothing")
read_csv("20210107-093101 COMServer_matchedtovideo.csv") -> nav_anevik_1_11

# select the relevant columns
nav_anevik_1_11 %<>% select(Sperre_LOG_DATETIME, Sperre_Depth, Sperre_Heading, Survey_ROV_LON, Survey_ROV_LAT, Survey_LOG_RELTIME) 

# make sure XY columns are numeric by removing |xb0 (for Anevik navigation files)
nav_anevik_1_11$Survey_ROV_LON <- str_remove(nav_anevik_1_11$Survey_ROV_LON, pattern = "\xb0")
nav_anevik_1_11$Survey_ROV_LAT <- str_remove(nav_anevik_1_11$Survey_ROV_LAT, pattern = "\xb0")

nav_anevik_1_11 %>% mutate(Survey_ROV_LON2 = as.numeric(Survey_ROV_LON), Survey_ROV_LAT2 =as.numeric(Survey_ROV_LAT)) -> nav_anevik_1_11
view(nav_anevik_1_11) ### why is as numeric removing the last digits of the Latitude and longitude?
# fill the gaps in the shp file
# time managment
nav_anevik_1_11 %>% 
  # make the time column - if you have a data and a time column
  mutate(mtime = Sperre_LOG_DATETIME)-> shpxy
# if you have a dmy_hms character strimg
#mutate( mtime = dmy_hms(time)) -> shpxy


# create the missing time stamps so that you have a reading at each second 
shpxy %>% select(mtime, Survey_LOG_RELTIME,Survey_ROV_LON2,Survey_ROV_LAT2, Sperre_LOG_DATETIME, Sperre_Depth, Sperre_Heading) %>% 
  as_tsibble(index = mtime) %>%
  fill_gaps() %>% # automatically creates the missing timestamps
  as_tibble() %>% 
  
  
  
  # interpolate the coordinates values for the timestamps you have created
  # these wont be accurate but you will replace them soon
  # they are still a basic interpolation of the closest coordinates 
  mutate(LON2 = zoo::na.approx(as.vector(Survey_ROV_LON2)),
         LAT2 = zoo::na.approx(as.vector(Survey_ROV_LAT2))) -> shpxy  
# note this might also be enough if your track loog good

shpxy %>%  filter( is.na(LAT2) ) # there shouldn???t be any NA left - but check does no harm
is.na(shpxy$LAT2)

# Smooth the track 
t <- 1:nrow(shpxy)
x <- shpxy$LON2
y <- shpxy$LAT2
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
  bind_cols(shpxy,.) -> smoothed_nav_anevik_1_11

write_csv( smoothed_nav_anevik_1_11, "smoothed_nav_anevik_1_11.csv")
