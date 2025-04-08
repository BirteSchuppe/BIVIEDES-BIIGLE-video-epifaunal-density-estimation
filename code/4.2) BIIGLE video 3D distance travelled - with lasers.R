library(sf)
library(rgl)
library(magrittr)
library(tidyverse)


# proocessing metadata ========================================================================
# put path to your processed navigation file with smoothed XYZ and image width
paste0("./Output/laserCal_biigleannotation.csv" ) %>% read_csv() -> smoothed_navigation_laser_data


# if you know it, put the corresponding filename here
 # - Check if needed !!!!
smoothed_navigation_laser_data %>% mutate(filename = "transect_1" ) %>%
  # put the filename in first place
  relocate(filename,.before = names(smoothed_navigation_laser_data)[1])

#convert time/date coloumn as date character
smoothed_navigation_laser_data %>%
  filter(!is.na(realtime)) %>%  # make sure missing timestamps are not included
  mutate(datetime = realtime  ) -> smoothed_navigation_laser_data

 
# make a time in video column
start_time <- smoothed_navigation_laser_data$datetime[1]

time_stamps <- smoothed_navigation_laser_data  %>%  
  mutate(sec_in_video = seconds(datetime  -  start_time   )  %>% seconds_to_period()  ) %>% 
  mutate(time_in_video =sprintf('%02d:%02d:%02d', sec_in_video$hour,sec_in_video$minute, sec_in_video$second) )

# change format of time column in navigation table
time_stamps %<>% 
  mutate( sec_in_video =  sec_in_video %>% seconds() )

# does this look legit? 
time_stamps %>%  select( sec_in_video , time_in_video )



# distance between point  -----------------------------------------------------------------------------
 
# latlong to to utm
traj.sf <-st_as_sf(smoothed_navigation_laser_data ,coords =c("xsmoothed","ysmoothed"),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# reproject to UTM 33
# find out the right UTM zone and convert to it
traj.sf %<>% st_transform( "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
# get coordinates 
traj.sf$geometry %>% st_coordinates() %>% as_tibble() %>% bind_cols(smoothed_navigation_laser_data) -> joint_navigation_annotation_utm

# USE interpolated depth HERE - renaming interpolated to just depth for simplicity
joint_navigation_annotation_utm %>%
  dplyr::select(X, Y,depth = depth_interpolated)  -> dpoint


# plot your points in 3d

open3d()
x <- dpoint$X %>%  scale()
y <- dpoint$Y %>%  scale()
depth <-  dpoint$depth %>%  scale() # USE interpolated depth HERE !!!!!!
plot3d(x, y, depth, col = rainbow(1000))


# at each point, use coordinates of the previous point 
dpoint %>%
  slice(-n()) %>%  bind_rows(dpoint %>% slice(1),.) %>% 
  rename(X2 = X,Y2 = Y, depth2 = depth ) %>%  # USE interpolated depth HERE !!!!!!
  bind_cols(dpoint,.) %>%
  # calculate distance between each point and the point before (so it starts at 0)
  rowwise() %>%
  mutate(distance_3d =
           # put coordinates into a matrix
           matrix(c(X2,Y2,depth2,X,Y,depth), # USE interpolated depth HERE !!!!!!
                  nrow= 2,
                  ncol = 3, # coordinates matrix has  columns
                  byrow = TRUE ) %>% 
           # calculate Euclidean distance between two sets of points 
           dist() %>%  as.numeric()) %>% 
  ungroup() -> dpoints2

#cumulated distance travelled 
dpoints2  %>% 
  mutate(index = 1:nrow(.)) %>% 
  #group_by(index) %>% 
  mutate(distance_travelled = cumsum(distance_3d)) %>% 
  select(distance_travelled ) %>% 
  bind_cols(dpoints2,. ) -> cumulated_3D_distance_travelled

# print the whole transects length/distance travelled
cumulated_3D_distance_travelled %>% tail(1) %>% pull(distance_travelled) %>% 
  paste0("Distance travelled in 3D: ",.) %>% 
  print()

# add the distance column
joint_navigation_annotation <- bind_cols(joint_navigation_annotation_utm,
                                         cumulated_3D_distance_travelled %>% select(distance_3d, distance_travelled) )

# export the smoothed and calibrated navigation into the 
joint_navigation_annotation %>%
  write_csv(paste0("./Output/dist_calibrated_smoothed_navigation.csv" ) )

# =======================================================================================
# Add a bin column
# =======================================================================================


# open image calibration info to calculate the bin surface
paste0("./Output/dist_calibrated_smoothed_navigation.csv" ) %>%  read_csv() -> joint_navigation_annotation

# set a bin size in m 
bin_size <- 50

# create bins of 50m distance travelled 
breaks <- seq(0, max(joint_navigation_annotation$distance_travelled) + bin_size, by = bin_size)
binned_numbers <- cut(joint_navigation_annotation$distance_travelled, breaks = breaks, include.lowest = TRUE)
joint_navigation_annotation <- mutate(joint_navigation_annotation, bin = binned_numbers) 


# show the average width in each bin 
joint_navigation_annotation %>%  group_by(bin) %>% 
  summarise(n = n(), 
            # caluculate the mean depth in each bin
            mean_depth = mean(depth, na.rm = TRUE),
            # and the width
            mean_width = mean(width, na.rm = TRUE),
  ) %>% 
  # surface of a bin is the average width * 50m
  mutate(bin_surface = mean_width * bin_size) -> bins_metadata

# add a line to the tabl showing the average values for the entire table
bins_metadata %>% 
  summarise(mean_depth = mean(mean_depth),
            mean_width = mean(mean_width),
            bin_surface = sum(bin_surface)) %>% 
  mutate(bin = paste0( "All(",breaks %>% head(1),",",breaks %>% last() ,"]"), n = nrow(joint_navigation_annotation) ) %>% 
  bind_rows(bins_metadata) -> bins_metadata


# expor the smoothed and calibrated navigation into the 
joint_navigation_annotation %>%
  write_csv(paste0("./Output/bins_dist_calibrated_smoothed_navigation.csv" ) )

