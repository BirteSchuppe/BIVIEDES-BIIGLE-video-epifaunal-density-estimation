





# create the missing time stamps so that you have a reading at each second 
maskxy %>%
  select(datetime, videotime,lon,lat, depth ) %>% 
  as_tsibble(index = datetime) %>%
  fill_gaps() %>% # automatically creates the missing timestamps
  as_tibble() %>% 
  # interpolate the coordinates values for the timestamps you have created
  # these wont be accurate but you will replace them soon
  # they are still a basic interpolation of the closest coordinates 
  mutate(LON2 = zoo::na.approx(as.vector(lon)),
         LAT2 = zoo::na.approx(as.vector(lat)), 
         DEPTH2 = zoo::na.approx(as.vector(depth)) ) -> maskxy  

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
sy <- stats::smooth.spline(t, y, df = dfs, spar = spars, cv = TRUE)

# plot the recorded tracks 
plot(x,y, cex = 0.25, col = "black", main = paste0("Difference of un-smoothed and smoothed ROV transect "))
# overlay the smoothed track
lines(sx[[2]], sy[[2]], col = 2, lwd = 2)
lines(sx[[1]], sy[[1]], col = 3, lwd = 2)
legend("topleft", legend=c("smoothed", "un-smoothed"),
       col=c("red", "black"), lty=1:2, cex=0.8)


smoothed_navigation <- maskxy %>% 
  mutate(LON2 = sx[[2]],
         LAT2 = sy[[2]],
         DEPTH2 = maskxy$DEPTH2) %>% 
  select(datetime, videotime, LON2, LAT2, DEPTH2)
