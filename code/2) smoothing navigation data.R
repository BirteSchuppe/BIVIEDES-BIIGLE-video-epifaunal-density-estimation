
library(lubridate)
library(tsibble)
library(zoo) # for interpolations
library(plotly) # for interactive plots 
library(tidyverse)
library(magrittr)


# open the table with time and xy positions, here maybe need to chag e/delete working direction??? (linked to my local drive)
# setwd("D:/PHD/ROV/ROV raw video density estimates/navigation_smoothing")
navigation_file <- "Generic_navigation.csv"
read_csv(paste0("nav/",navigation_file) ) -> navigation

 ### why is as numeric removing the last digits of the Latitude and longitude?
# fill the gaps in the shp file
# time managment
navigation %>% 
  # make the time column - if you have a data and a time column
  mutate(mtime = Sperre_LOG_DATETIME) -> maskxy
# if you have a dmy_hms character string
#mutate( mtime = dmy_hms(time)) -> maskxy


# create the missing time stamps so that you have a reading at each second 
maskxy %>% select(mtime, Survey_LOG_RELTIME,Survey_ROV_LON,Survey_ROV_LAT, Sperre_LOG_DATETIME, Sperre_Depth, Sperre_Heading) %>% 
  as_tsibble(index = mtime) %>%
  fill_gaps() %>% # automatically creates the missing timestamps
  as_tibble() %>% 
  # interpolate the coordinates values for the timestamps you have created
  # these wont be accurate but you will replace them soon
  # they are still a basic interpolation of the closest coordinates 
  mutate(LON2 = zoo::na.approx(as.vector(Survey_ROV_LON)),
         LAT2 = zoo::na.approx(as.vector(Survey_ROV_LAT)), 
         DEPTH2 = zoo::na.approx(as.vector(DEPTH)) ) -> maskxy  
# note this might also be enough if your track look good

maskxy %>%  filter( is.na(LAT2) ) # there shouldn???t be any NA left - but check does no harm
is.na(maskxy$LAT2)

# Smooth the track 
t <- 1:nrow(maskxy)
x <- maskxy$LON2
y <- maskxy$LAT2
# z  # no smoothing of th ~DEPTH: interpolation should be enough 
# Fit a cubic smoothing spline to each dimension
# !!! this is user input. You can decide on the right smoothing paramter after checking the plot below
dfs = 50
spars = 0.3

sx <- smooth.spline(t, x, df = dfs, spar = spars, cv = TRUE)
sy <- smooth.spline(t, y, df = dfs, spar = spars, cv = TRUE)
 


# plot the recorded tracks 
plot(x,y, cex = 0.25, col = "black", main = paste0("Difference of un-smoothed and smoothed ROV transect "))
# overlay the smoothed track
lines(sx[[2]], sy[[2]], col = 2, lwd = 2)
legend("topleft", legend=c("smoothed", "un-smoothed"),
       col=c("red", "black"), lty=1:2, cex=0.8)

# make the same plot in plotly
plot_ly() %>%
  add_trace(x = x, y = y, mode = "markers", type = "scatter", name = "un-smoothed") %>%
  add_trace(x = sx[[2]], y = sy[[2]], mode = "lines", type = "scatter", name = "smoothed") %>%
  layout(title = "Difference of un-smoothed and smoothed ROV transect",
         xaxis = list(title = "Longitude"),
         yaxis = list(title = "Latitude"))


# add the new coordinates to the table
tibble( xsmoothed = sx[[2]] ,ysmoothed = sy[[2]] )  %>%
  bind_cols(maskxy,.) -> smoothed_navigation

write_csv( smoothed_navigation, paste0("nav/smoothed_",navigation_file))   
