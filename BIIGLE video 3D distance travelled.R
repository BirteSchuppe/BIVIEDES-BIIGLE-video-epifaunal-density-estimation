library(magrittr)
library(tidyverse)


# proocessing metadata ========================================================================
# put path to your navigation file
setwd("D:/PHD/ROV/ROV raw video density estimates/navigation_smoothing")
read_csv("smoothed_nav_anevik_1_11.csv", col_names = TRUE, trim_ws = TRUE) -> h1

annotations_file <- "26-anevik-1-11_arranged.csv"


# open the table of annotations

read_csv(annotations_file) -> ha1 

view(ha1)



# if you know it, put the corresponding filename here
h1 %>% mutate(filename = "transect_1" ) %>%
  # put the filename in first place
  relocate(filename,.before = names(h1)[1])

#convert time/date coloumn as date character
h1 %>%
  filter(!is.na(Sperre_LOG_DATETIME)) %>% 
  mutate(datetime = Sperre_LOG_DATETIME  ) -> h1

# make a time in video column
start_time <- h1$datetime[1]

h2 <- h1  %>%  
  mutate(sec_in_video = seconds(datetime  -  start_time   )  %>% seconds_to_period()  ) %>% 
  mutate(time_in_video =sprintf('%02d:%02d:%02d', sec_in_video$hour,sec_in_video$minute, sec_in_video$second) )

# change format of time column in navigation table
h2 %<>% 
  mutate( sec_in_video =  sec_in_video %>% seconds() )


# does this look legit? 
h2 %>%  select( sec_in_video , time_in_video )





# since not all time stamp in video have corresponding metadata entry
# make a join function with the closest time stamps
# by <- join_by(  closest(time_in_video <= time_in_video))
# left_join(frames_times,d ,by , suffix = c(".framegrab", ".metadata")) -> dd


# export csv table
h2 %>% write_csv("time_smoothed_nav_anevik_1_11.csv")



# merge with Biigle annotations ========================================================


# first and last frame of each annotation 
ha1 %<>%
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
ha1 %<>%
  mutate( startsec = startframe %>% seconds() %>% floor, 
          endsec = endframe %>% seconds() %>% floor)


# attach metadata at the time something disappear (last second - where nearest to the camera) ----------

# join to nearest timestamp in metadata  
by <- join_by(  closest(endsec  <= sec_in_video )) # !! joining by closest time stamp - MAY NOT BE EXACT !!!!!!!!!!!
h2  %>%  
  left_join(ha1, . ,by , suffix = c(".annotations", ".metadata"))   -> annotations_meta

# look at the navigation and matched annotations locations 
h2 %>% select(xsmoothed, ysmoothed) %>%  plot()
annotations_meta %>%  select(xsmoothed, ysmoothed) %>%  points(pch = 20, col = "red" )



# export table of annotation with metadata 
annotations_meta %>% write_csv("smoothed_nav_annotation_anevik_1_11.csv")



# distance between point  -----------------------------------------------------------------------------

# open the table

#Nils approach:
library(sf)

read_csv("smoothed_nav_annotation_anevik_1_11.csv",col_names = TRUE) -> t1
view(t1)



# latlong to to utm
traj.sf <-st_as_sf(t1 ,coords =c("xsmoothed","ysmoothed"),crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
traj.sf %<>% st_transform( "+proj=utm +zone=33 +datum=WGS84 +units=m +no_defs")
# get coordinates 
traj.sf$geometry %>% st_coordinates() %>% as_tibble() %>% bind_cols(t1)-> t1_utm

t1_utm %>% dplyr::select(X, Y,Sperre_Depth)  -> dpoint


# plot your points in 3d
library(rgl)
open3d()
x <- dpoint$X %>%  scale()
y <- dpoint$Y %>%  scale()
depth <-  dpoint$Sperre_Depth %>%  scale()
plot3d(x, y, depth, col = rainbow(1000))



# at each point, use coordinates of the previous point 
dpoint %>%
  slice(-n()) %>%  bind_rows(dpoint %>% slice(1),.) %>%
  rename(X2 = X,Y2 = Y,Sperre_Depth21 = Sperre_Depth) %>% 
  bind_cols(dpoint,.) %>%
  # calculate distance between each point and the point before (so it starts at 0)
  rowwise() %>%
  mutate(distance_3d =
           # put coordinates into a matrix
           matrix(c(X2,Y2,Sperre_Depth21,X,Y,Sperre_Depth),
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
  bind_cols(dpoints2,. ) -> distance_travelled_z

distance_travelled_z %>% tail(1) %>% pull(distance_travelled) 

t1 <- cbind(t1, distance_travelled_z)
view(t1)


t1 %>% write.csv("smoothed_distancetravelled_annotationfixed_anevik_1_11.csv")