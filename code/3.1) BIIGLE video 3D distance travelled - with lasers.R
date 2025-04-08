library(sf)
library(rgl)
library(magrittr)
library(tidyverse)


# proocessing metadata ========================================================================
# put path to your navigation file

paste0("nav/smoothed_",navigation_file) %>% 
read_csv( col_names = TRUE, trim_ws = TRUE) -> navigation_smoothed


paste0("./Output/laserCal_biigleannotation.csv" ) %>% read_csv() -> smoothed_navigation_laser_data
smoothed_navigation_laser_data %>% select()


# open the table of annotations
arranged_annotations_file <-  paste0("./annotations/","arranged_generic_annotation.csv" )
read_csv(arranged_annotations_file) -> arranged_biigle_annotations 

# if you know it, put the corresponding filename here
 # - Check if needed !!!!
smoothed_navigation_laser_data %>% mutate(filename = "transect_1" ) %>%
  # put the filename in first place
  relocate(filename,.before = names(smoothed_navigation_laser_data)[1])

#convert time/date coloumn as date character
smoothed_navigation_laser_data %>%
  filter(!is.na(mtime)) %>% 
  mutate(datetime = mtime  ) -> smoothed_navigation_laser_data


"red"
replace all smooth nav with the new surface nav dataset
"red"

# make a time in video column
start_time <- navigation_smoothed$datetime[1]

time_stamps <- navigation_smoothed  %>%  
  mutate(sec_in_video = seconds(datetime  -  start_time   )  %>% seconds_to_period()  ) %>% 
  mutate(time_in_video =sprintf('%02d:%02d:%02d', sec_in_video$hour,sec_in_video$minute, sec_in_video$second) )

# change format of time column in navigation table
time_stamps %<>% 
  mutate( sec_in_video =  sec_in_video %>% seconds() )

# does this look legit? 
time_stamps %>%  select( sec_in_video , time_in_video )

# merge with Biigle annotations ========================================================

# first and last frame of each annotation 
arranged_biigle_annotations %<>%
  # start frame
  mutate(startframe =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~head(.x,1) %>% as.numeric),
         # endframe
         endframe =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~tail(.x,1) %>% as.numeric) )

# make a start and end second for each annotation
arranged_biigle_annotations %<>%
  mutate( startsec = startframe %>% seconds() %>% floor, 
          endsec = endframe %>% seconds() %>% floor)


# attach metadata at the time something disappear (last second - where nearest to the camera) ----------

# join to nearest timestamp in metadata  
by <- join_by(  closest(endsec  <= sec_in_video )) # !! joining by closest time stamp - MAY NOT BE EXACT !!!!!!!!!!!
time_stamps  %>%  
  left_join(arranged_biigle_annotations, . ,by , suffix = c(".annotations", ".metadata"))   -> joint_navigation_annotation

# look at the navigation and matched annotations locations 
time_stamps %>% select(xsmoothed, ysmoothed) %>%  plot()
joint_navigation_annotation %>%  select(xsmoothed, ysmoothed) %>%  points(pch = 20, col = "red" )

 

# distance between point  -----------------------------------------------------------------------------
 
# latlong to to utm
traj.sf <-st_as_sf(joint_navigation_annotation ,coords =c("xsmoothed","ysmoothed"),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# reproject to UTM 33
# find out the right UTM zone and convert to it
traj.sf %<>% st_transform( "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
# get coordinates 
traj.sf$geometry %>% st_coordinates() %>% as_tibble() %>% bind_cols(joint_navigation_annotation)-> joint_navigation_annotation_utm

# USE interpolated depth HERE !!!!!!
joint_navigation_annotation_utm %>% dplyr::select(X, Y,Sperre_Depth)  -> dpoint


# plot your points in 3d

open3d()
x <- dpoint$X %>%  scale()
y <- dpoint$Y %>%  scale()
depth <-  dpoint$Sperre_Depth %>%  scale() # USE interpolated depth HERE !!!!!!
plot3d(x, y, depth, col = rainbow(1000))



# at each point, use coordinates of the previous point 
dpoint %>%
  slice(-n()) %>%  bind_rows(dpoint %>% slice(1),.) %>% 
  rename(X2 = X,Y2 = Y,Sperre_Depttime_stamps1 = Sperre_Depth) %>%  # USE interpolated depth HERE !!!!!!
  bind_cols(dpoint,.) %>%
  # calculate distance between each point and the point before (so it starts at 0)
  rowwise() %>%
  mutate(distance_3d =
           # put coordinates into a matrix
           matrix(c(X2,Y2,Sperre_Depttime_stamps1,X,Y,Sperre_Depth), # USE interpolated depth HERE !!!!!!
                  nrow= 2,
                  ncol = 3, # coordinates matrix has  columns
                  byrow = TRUE ) %>%  dist() %>%  as.numeric()) %>% 
  ungroup() -> dpoints2

#cumulated distance travelled 
dpoints2  %>% 
  mutate(index = 1:nrow(.)) %>% 
  #group_by(index) %>% 
  mutate(distance_travelled = cumsum(distance_3d)) %>% 
  select(distance_travelled ) %>% 
  bind_cols(dpoints2,. ) -> cumulated_3D_distance_travelled

# print the whole transects length/distance travelled
cumulated_3D_distance_travelled %>% tail(1) %>% pull(distance_travelled) 

# add the distance column
joint_navigation_annotation <- bind_cols(joint_navigation_annotation, cumulated_3D_distance_travelled)
 

# export the table
joint_navigation_annotation %>% write_csv(paste0("./Output/distancetravelled_biigleannotation.csv" ) )



NOW ADD THE LASERS CLIBRATION AAND EXPORT


library(tsibble)
library(zoo) # for interpolations
library(gridExtra)
library(magrittr)
library(tidyverse)
# proocessing metadata ========================================================================
# put path to your navigation filefix error with workind dir
smoothed_navigation_file <- "smoothed_Generic_navigation.csv"
read_csv (paste0("nav/",smoothed_navigation_file)) -> smoothed_navigation_data
paste0("./Output/distancetravelled_biigleannotation.csv" ) %>% read_csv() -> joint_navigation_annotation

paste0("./Output/distancetravelled_biigleannotation.csv" ) %>% read_csv()   -> joint_navigation_annotation 
  

joint_navigation_annotation %>% select(datetime, X, Y, DEPTH2 , distance_3d, distance_travelled )
  



read_csv ( paste0("./annotations/arranged_",generic_annotation_file ) ) -> annotation_data


view(annotation_data)


# the properties of the images and laser scale? 

# MAKE SURE IT IS CORRECT FOR YOUR STUDY!!!!!! 
ROV_imageproperties <- tibble( width = 1920, 
                               height = 1080, 
                               nb_pxl = 1920 * 1080 , 
                               laserscaledist_m = 0.075 # in meters (not cm)
) 

# extract the first line of nav as the starttime - need to import the metadata as nav
video_starttime <- joint_navigation_annotation %>% slice(1) %>% 
  mutate(date_time = anytime::anytime(paste(Sperre_LOG_DATETIME)))  %>% pull(date_time)

# load Biigle annotations 
# The laser point annotations must be unique to each laser points - 
# tracked annotations (more thant one time stamps ) will not work 

# first and last frame of each annotation 
annotation_data %<>%
  # start frame
  mutate(startframe =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~head(.x,1) %>% as.numeric),
         # endframe
         endframe =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~tail(.x,1) %>% as.numeric) )

# make a start and end second for each annotation
annotation_data %<>%
  mutate( startsec = startframe %>% seconds() %>% floor, 
          endsec = endframe %>% seconds() %>% floor)

# attach metadata at the time something disappear (last second - where nearest to the camera) ----------

annotation_data   %>% filter(  label_name == "Laser points") -> laser

# get all the images with 2 laser dots
laser %>% count(frames) %>% count(n)

# # frames that are 2 entries and entries that are within 1 frame
laser %>% filter(startframe == endframe) %>% count(frames) %>% filter(n == 2) %>% pull(frames) -> laserIMG
laser %>% filter(frames %in% laserIMG)-> laser


# convert char point coordinate to numerical
laser %<>%
  # select(frames, points) %>% 
  mutate(points = str_sub(points, 3,-3 )) %>% 
  separate(points,into = c("pointx","pointy"),sep = ",") %>% 
  mutate(across(c("pointx","pointy"), as.numeric )) 



# calculate distance between points 
laser %<>% 
  select(frames,pointx, pointy) %>% 
  split(.$frames) %>%
  map(~ select(.,pointx, pointy) %>% dist() %>%  tibble(laser_scale = .) ) %>%
  bind_rows(.id = "frames") %>% 
  left_join(laser,by = join_by(frames)) %>% 
  mutate(pixelsize_m = ROV_imageproperties$laserscaledist_m/laser_scale %>%  as.numeric )  

# Calculate image width and height from the pixelsize and the image properties
laser %>% 
  # select(laser_scale, frames, pixelsize_m) %>% 
  mutate(height = (pixelsize_m) * ROV_imageproperties$height,
         width = (pixelsize_m) * ROV_imageproperties$width ) %>% 
  distinct(frames, .keep_all = T) %>% 
  # keep the important variables for exporting 
  select(frames, pixelsize_m, width, height  ) -> surface_data 


# frame in seconds 
surface_data %<>% 
  mutate(timeinvid_sec =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~head(.x,1) %>% as.numeric %>%
                     # round down to the second 
                     floor()   ) ) %>% arrange(timeinvid_sec)

# time in hms form 
surface_data %<>% 
  mutate(v= seconds_to_period(timeinvid_sec)) %>%
  # format duration to hh:mm:ss - with leading 0s
  mutate(Time_hms =strftime(as.POSIXct("00:00:00", format="%H:%M:%S") + v, format="%H:%M:%S") ) 

# real time : video start time + time in video 
surface_data %<>% 
  mutate(realtime = video_starttime + timeinvid_sec) %>% 
  select(-v)


# average image width in meters 
average_image_width <- surface_data$width %>% mean()
print(paste0("average iamge width in meters: ",average_image_width))



# interpolate surface data == === == === == === = = == = = =

# create the missing time stamps so that you have a reading at each second 
surface_data %>% 
  as_tsibble(index = realtime ) %>%
  fill_gaps() %>% # automatically creates the missing timestamps
  as_tibble() %>% 
  # interpolate the coordinates values for the timestamps you have created
  # these wont be accurate but you will replace them soon
  # they are still a basic interpolation of the closest coordinates 
  mutate(width2 = zoo::na.approx(as.vector(width)),
         height2 = zoo::na.approx(as.vector(height))  ) %>%
  select(laser_frames = frames, width = width2 , height = height2, datetime = realtime) -> surface_data_1s  


# position in video 
# join to nearest timestamp in metadata  
by <- join_by(  closest(datetime  <= datetime )) # !! joining by closest time stamp - MAY NOT BE EXACT !!!!!!!!!!!
surface_data_1s   %>% 
  left_join(joint_navigation_annotation  ,
            by # custom join function 
            ) -> smoothed_nav_dist_surface   

### export the table
surface_data_1s %>% write.csv(paste0("./Output/laserCal_biigleannotation.csv" ) )


