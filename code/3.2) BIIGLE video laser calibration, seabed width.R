library(tsibble)
library(zoo) # for interpolations
library(gridExtra)
library(magrittr)
library(tidyverse)
# proocessing metadata ========================================================================
# put path to your navigation filefix error with workind dir
smoothed_navigation_file <- "smoothed_Generic_navigation.csv"
read_csv (paste0("nav/",smoothed_navigation_file)) -> smoothed_navigation_data
read_csv ( paste0("./annotations/arranged_",generic_annotation_file ) ) -> annotation_data

# the propeorties of the images and laser sacle? 

# MAKE SURE IT IS CORRECT FOR YOUR STUDY!!!!!! 
  
ROV_imageproperties <- tibble( width = 1920, 
                      height = 1080, 
                      nb_pxl = 1920 * 1080 , 
                      laserscaledist_m = 0.075 # in meters (not cm)
) 

# extract the first line of nav as the starttime - need to import the metadata as nav
video_starttime <- smoothed_navigation_data %>% slice(1) %>% pull(mtime)

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
print(paste0("average image width in meters: ",average_image_width))
 


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
         height2 = zoo::na.approx(as.vector(height))  )  %>%
  select(laser_frames = frames, width = width2 , height = height2, realtime) -> surface_data_1s  


# position in video 
# join to nearest timestamp in metadata
surface_data_1s  %>% 
  left_join(smoothed_navigation_data %>% select(realtime = mtime, xsmoothed,ysmoothed,   depth_interpolated = DEPTH2),
            by = join_by(realtime) ,
            suffix = c(".1s", ".orifreq") ) -> surface_data_1s_nav   

### export the table
surface_data_1s_nav %>%
  write_csv(paste0("./Output/laserCal_biigleannotation.csv" ) )

# =======================================================================================
# plotting the data
# =======================================================================================

### make plots of interp
# plot width over time 
surface_data %>% 
  ggplot(aes(x = realtime, y = width)) +
  geom_point(color = "red") +
  labs(title = "Width of images over time",
       x = "Time (original frequency)",
       y = "Width (m)") +
  theme_minimal() -> plot10s


# plot width over time 
surface_data_1s_nav %>% 
  ggplot(aes(x = realtime , y = width)) +
  geom_line() +
  labs(title = "Interpolated width of images over time",
       x = "Time (1s)",
       y = "Width (m)") +
  theme_minimal() -> plot1s


# add the line of width from surface data to plot1s and add a legend
plot1s + 
  geom_point(data = surface_data, aes(x = realtime, y = width), color = "red") +
  # add the red dots as original and black line as interpolated in the legend
  scale_color_manual(values = c("red", "black"), labels = c("Original", "Interpolated")) +
  theme_minimal()  -> plot1_10s
 
# put the two plots into a grid with 1 column and 2 rows
gridExtra::grid.arrange(plot10s, plot1s,plot1_10s, ncol = 1)
