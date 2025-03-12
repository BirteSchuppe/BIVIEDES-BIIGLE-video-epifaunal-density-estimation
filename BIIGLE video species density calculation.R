library(tidyverse)
library(readr)
library(stringr)
library(ggplot2)


#set working direction and load csv of matched BIIGLE annotation with position data
setwd("D:/PHD/ROV/ROV raw video density estimates/navigation_smoothing")
read_delim("smoothed_distancetravelled_annotationfixed_anevik_1_11.csv") -> dataframe

view(dataframe)

### attach a new column with video transect width, here the mean image width is used
dataframe %>% mutate(mean_video_width = 4.54) -> dataframe

### mutate new columns of single species count or counts including higher taxonomic species groups along whole dataframe, do so each time if column "label_name" (has to be the same name as in the BIIGLE label tree)finds a corresponding entry, here the example for single species count is the gorgonian coral Primnoa, here labelled as "Primnoa stet."
### each time the code finds the corresponding BIIGLE label in a datarow (as string ? copied from the dataframe), a count of 1 is given for this datarow, in case it is not a corresponding entry, the datarow sets a 0
### need help here: possible to make a loop so that script does recognize labels by itself instead of manually entering?
dataframe$Primnoa_count<- ifelse(dataframe$label_name=='Primnoa stet.', 1, 0)

##### the same logic for higher taxonomic species groups, here all the annotated species inside this group has to be pasted into the code to be included in the counts
#### example: all crustaceans are counted annotated should be counted, including caridea, crabs, other not further identified crustaceans


dataframe$crustacea_count <- ifelse(dataframe$label_name=='Caridea stet.', 1, 0)+ ifelse(dataframe$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(dataframe$label_name=='Munida stet.', 1,0)+ ifelse(dataframe$label_name=='Arthropoda stet.', 1,0)+ ifelse(dataframe$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along dataframe of previously individual species/or species group counts
dataframe %>%mutate(total_Primnoa_cumulated_count = cumsum(Primnoa_count)) -> dataframe
dataframe %>%mutate(total_Crustacea_cumulated_count= cumsum(crustacea_count)) -> dataframe


###mutate new column with calculation of total seafloor area covered in squaremetres along the whole dataframe: mean width of video in meters * 3D distance travelled in meters
dataframe %>%mutate(total_ROV_area=mean_video_width * dataframe$distance_travelled) -> dataframe
### retrieve maximum total seafloor area as single value
ROV_area_total <- max(dataframe$total_ROV_area, na.rm = TRUE)



### to find out how species density changes along the video transect, we split the dataframe into subsets based on the column desired distance_travelled, here sections of 50m distance_travelled subsets are filtered consecutively along the whole dataframe
### to do so, copy and paste the dataframe cell of distance travelled containing the maximul value of the sub section, here 50.4891174321858m

dataframe %>% filter (between (dataframe$distance_travelled,0.000000,50.4891174321858)) -> subset_zero_fifty_meters
###calculate subset distance travelled in meters based on subtraction of maximum distance travelled and minimum distance travelled in subset OR apply a rounded distance travelled of 50 across whole dataset????
subset_zero_fifty_meters %>% mutate(ROV_area_distance_travelled_zero_fifty=50.4891174321858 - 0.000000 ) -> subset_zero_fifty_meters

### mutate new column with calculation of constant subset seafloor area covered in squaremeters : mean width of video in meters * subset distance travelled in meters calculated in previous step
subset_zero_fifty_meters %>%mutate(subset_ROV_area_zero_fifty=mean_video_width * ROV_area_distance_travelled_zero_fifty) -> subset_zero_fifty_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_zero_fifty <- max(subset_zero_fifty_meters$subset_ROV_area_zero_fifty, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_zero_fifty_meters$Primnoa_count_subset <- ifelse(subset_zero_fifty_meters$label_name=='Primnoa stet.', 1, 0)
subset_zero_fifty_meters$Crustacea_count_subset <- ifelse(subset_zero_fifty_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_zero_fifty_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_zero_fifty_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_zero_fifty_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_zero_fifty_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_zero_fifty_meters %>%mutate(Primnoa_cumulated_count_subset_zero_fifty = cumsum(Primnoa_count_subset)) -> subset_zero_fifty_meters
subset_zero_fifty_meters %>%mutate(Crustacea_cumulated_count_subset_zero_fifty = cumsum(Crustacea_count_subset)) -> subset_zero_fifty_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_zero_fifty_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_zero_fifty/subset_ROV_area_zero_fifty) -> subset_zero_fifty_meters
subset_zero_fifty_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_zero_fifty/subset_ROV_area_zero_fifty) -> subset_zero_fifty_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_zero_fifty <- tail(subset_zero_fifty_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_zero_fifty <- tail(subset_zero_fifty_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_zero_fifty_meters %>% write.csv("anevik_1_11_densities_subset_zero_fifty.csv")


### repeat steps above for filtering the next subset of 51-100m distance travelled, copy and paste consecutive distance travelled value to avoid duplicates
dataframe %>% filter (between (dataframe$distance_travelled,51.2314313023432,100.052977552811)) -> subset_fifty_hundret_meters
###calculate subset distance travelled based on subtraction of maximum distance travelled and minimum distance travelled in subset
subset_fifty_hundret_meters %>% mutate(ROV_area_distance_travelled_fifty_hundret=100.052977552811 - 51.2314313023432 ) -> subset_fifty_hundret_meters

### mutate new column with calculation of subset seafloor area covered in squaremeters along the whole subset: mean width of video in meters * subset 3D distance travelled in meters
subset_fifty_hundret_meters %>%mutate(subset_ROV_area_fifty_hundret=mean_video_width * ROV_area_distance_travelled_fifty_hundret) -> subset_fifty_hundret_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_fifty_hundret <- max(subset_fifty_hundret_meters$subset_ROV_area_fifty_hundret, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_fifty_hundret_meters$Primnoa_count_subset <- ifelse(subset_fifty_hundret_meters$label_name=='Primnoa stet.', 1, 0)
subset_fifty_hundret_meters$Crustacea_count_subset <- ifelse(subset_fifty_hundret_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_fifty_hundret_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_fifty_hundret_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_fifty_hundret_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_fifty_hundret_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_fifty_hundret_meters %>%mutate(Primnoa_cumulated_count_subset_fifty_hundret = cumsum(Primnoa_count_subset)) -> subset_fifty_hundret_meters
subset_fifty_hundret_meters %>%mutate(Crustacea_cumulated_count_subset_fifty_hundret = cumsum(Crustacea_count_subset)) -> subset_fifty_hundret_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_fifty_hundret_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_fifty_hundret/subset_ROV_area_fifty_hundret) -> subset_fifty_hundret_meters
subset_fifty_hundret_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_fifty_hundret/subset_ROV_area_fifty_hundret) -> subset_fifty_hundret_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_fifty_hundret <- tail(subset_fifty_hundret_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_fifty_hundret <- tail(subset_fifty_hundret_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_fifty_hundret_meters %>% write.csv("anevik_1_11_densities_subset_fifty_hundret.csv")


### repeat steps above for filtering the next subset of 101-150m distance travelled, copy and paste consecutive distance travelled value (cell value +1 distance travelled of previous subset end value) to avoid duplicates
dataframe %>% filter (between (dataframe$distance_travelled,101.265521690495,150.656469081094)) -> subset_hundret_hundretfifty_meters
###calculate subset distance travelled based on subtraction of maximum distance travelled and minimum distance travelled in subset
subset_hundret_hundretfifty_meters %>% mutate(ROV_area_distance_travelled_hundret_hundretfifty=150.656469081094 - 101.265521690495 ) -> subset_hundret_hundretfifty_meters

### mutate new column with calculation of subset seafloor area covered in squaremeters along the whole subset: mean width of video in meters * subset 3D distance travelled in meters
subset_hundret_hundretfifty_meters %>%mutate(subset_ROV_area_hundret_hundretfifty=mean_video_width * ROV_area_distance_travelled_hundret_hundretfifty) -> subset_hundret_hundretfifty_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_hundret_hundretfifty <- max(subset_hundret_hundretfifty_meters$subset_ROV_area_hundret_hundretfifty, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_hundret_hundretfifty_meters$Primnoa_count_subset <- ifelse(subset_hundret_hundretfifty_meters$label_name=='Primnoa stet.', 1, 0)
subset_hundret_hundretfifty_meters$Crustacea_count_subset <- ifelse(subset_hundret_hundretfifty_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_hundret_hundretfifty_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_hundret_hundretfifty_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_hundret_hundretfifty_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_hundret_hundretfifty_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_hundret_hundretfifty_meters %>%mutate(Primnoa_cumulated_count_subset_hundret_hundretfifty = cumsum(Primnoa_count_subset)) -> subset_hundret_hundretfifty_meters
subset_hundret_hundretfifty_meters %>%mutate(Crustacea_cumulated_count_subset_hundret_hundretfifty = cumsum(Crustacea_count_subset)) -> subset_hundret_hundretfifty_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_hundret_hundretfifty_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_hundret_hundretfifty/subset_ROV_area_hundret_hundretfifty) -> subset_hundret_hundretfifty_meters
subset_hundret_hundretfifty_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_hundret_hundretfifty/subset_ROV_area_hundret_hundretfifty) -> subset_hundret_hundretfifty_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_hundret_hundretfifty <- tail(subset_hundret_hundretfifty_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_hundret_hundretfifty <- tail(subset_hundret_hundretfifty_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_hundret_hundretfifty_meters %>% write.csv("anevik_1_11_densities_subset_hundret_hundretfifty.csv")

### repeat steps above for filtering the next subset of 151-200m distance travelled, copy and paste consecutive distance travelled value (cell value +1 distance travelled of previous subset end value) to avoid duplicates
dataframe %>% filter (between (dataframe$distance_travelled,151.043270002979,200.920185442804)) -> subset_hundretfifty_twohundret_meters
###calculate subset distance travelled based on subtraction of maximum distance travelled and minimum distance travelled in subset
subset_hundretfifty_twohundret_meters %>% mutate(ROV_area_distance_travelled_hundretfifty_twohundret=200.920185442804 - 151.043270002979 ) -> subset_hundretfifty_twohundret_meters

### mutate new column with calculation of subset seafloor area covered in squaremeters along the whole subset: mean width of video in meters * subset 3D distance travelled in meters
subset_hundretfifty_twohundret_meters %>%mutate(subset_ROV_area_hundretfifty_twohundret=mean_video_width * ROV_area_distance_travelled_hundretfifty_twohundret) -> subset_hundretfifty_twohundret_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_hundretfifty_twohundret <- max(subset_hundretfifty_twohundret_meters$subset_ROV_area_hundretfifty_twohundret, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_hundretfifty_twohundret_meters$Primnoa_count_subset <- ifelse(subset_hundretfifty_twohundret_meters$label_name=='Primnoa stet.', 1, 0)
subset_hundretfifty_twohundret_meters$Crustacea_count_subset <- ifelse(subset_hundretfifty_twohundret_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_hundretfifty_twohundret_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_hundretfifty_twohundret_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_hundretfifty_twohundret_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_hundretfifty_twohundret_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_hundretfifty_twohundret_meters %>%mutate(Primnoa_cumulated_count_subset_hundretfifty_twohundret = cumsum(Primnoa_count_subset)) -> subset_hundretfifty_twohundret_meters
subset_hundretfifty_twohundret_meters %>%mutate(Crustacea_cumulated_count_subset_hundretfifty_twohundret = cumsum(Crustacea_count_subset)) -> subset_hundretfifty_twohundret_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_hundretfifty_twohundret_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_hundretfifty_twohundret/subset_ROV_area_hundretfifty_twohundret) -> subset_hundretfifty_twohundret_meters
subset_hundretfifty_twohundret_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_hundretfifty_twohundret/subset_ROV_area_hundretfifty_twohundret) -> subset_hundretfifty_twohundret_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_hundretfifty_twohundret <- tail(subset_hundretfifty_twohundret_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_hundretfifty_twohundret <- tail(subset_hundretfifty_twohundret_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_hundretfifty_twohundret_meters %>% write.csv("anevik_1_11_densities_subset_hundretfifty_twohundret.csv")



### repeat steps above for filtering the next subset of 201-250m distance travelled, copy and paste consecutive distance travelled value (cell value +1 distance travelled of previous subset end value) to avoid duplicates
dataframe %>% filter (between (dataframe$distance_travelled,201.196880718376,250.898632419209)) -> subset_twohundret_twohundretfifty_meters
###calculate subset distance travelled based on subtraction of maximum distance travelled and minimum distance travelled in subset
subset_twohundret_twohundretfifty_meters %>% mutate(ROV_area_distance_travelled_twohundret_twohundretfifty=250.898632419209 - 201.196880718376 ) -> subset_twohundret_twohundretfifty_meters

### mutate new column with calculation of subset seafloor area covered in squaremeters along the whole subset: mean width of video in meters * subset 3D distance travelled in meters
subset_twohundret_twohundretfifty_meters %>%mutate(subset_ROV_area_twohundret_twohundretfifty=mean_video_width * ROV_area_distance_travelled_twohundret_twohundretfifty) -> subset_twohundret_twohundretfifty_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_twohundret_twohundretfifty <- max(subset_twohundret_twohundretfifty_meters$subset_ROV_area_twohundret_twohundretfifty, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_twohundret_twohundretfifty_meters$Primnoa_count_subset <- ifelse(subset_twohundret_twohundretfifty_meters$label_name=='Primnoa stet.', 1, 0)
subset_twohundret_twohundretfifty_meters$Crustacea_count_subset <- ifelse(subset_twohundret_twohundretfifty_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_twohundret_twohundretfifty_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_twohundret_twohundretfifty_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_twohundret_twohundretfifty_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_twohundret_twohundretfifty_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_twohundret_twohundretfifty_meters %>%mutate(Primnoa_cumulated_count_subset_twohundret_twohundretfifty = cumsum(Primnoa_count_subset)) -> subset_twohundret_twohundretfifty_meters
subset_twohundret_twohundretfifty_meters %>%mutate(Crustacea_cumulated_count_subset_twohundret_twohundretfifty = cumsum(Crustacea_count_subset)) -> subset_twohundret_twohundretfifty_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_twohundret_twohundretfifty_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_twohundret_twohundretfifty/subset_ROV_area_twohundret_twohundretfifty) -> subset_twohundret_twohundretfifty_meters
subset_twohundret_twohundretfifty_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_twohundret_twohundretfifty/subset_ROV_area_twohundret_twohundretfifty) -> subset_twohundret_twohundretfifty_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_twohundret_twohundretfifty <- tail(subset_twohundret_twohundretfifty_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_twohundret_twohundretfifty <- tail(subset_twohundret_twohundretfifty_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_twohundret_twohundretfifty_meters %>% write.csv("anevik_1_11_densities_subset_twohundret_twohundretfifty.csv")



### repeat steps above for filtering the next subset of 251-300m distance travelled, copy and paste consecutive distance travelled value (cell value +1 distance travelled of previous subset end value) to avoid duplicates
dataframe %>% filter (between (dataframe$distance_travelled,251.247662312574,299.697097722611)) -> subset_twohundretfifty_threehundret_meters
###calculate subset distance travelled based on subtraction of maximum distance travelled and minimum distance travelled in subset
subset_twohundretfifty_threehundret_meters %>% mutate(ROV_area_distance_travelled_twohundretfifty_threehundret=299.697097722611 - 251.247662312574 ) -> subset_twohundretfifty_threehundret_meters

### mutate new column with calculation of subset seafloor area covered in squaremeters along the whole subset: mean width of video in meters * subset 3D distance travelled in meters
subset_twohundretfifty_threehundret_meters %>%mutate(subset_ROV_area_twohundretfifty_threehundret=mean_video_width * ROV_area_distance_travelled_twohundretfifty_threehundret) -> subset_twohundretfifty_threehundret_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_twohundretfifty_threehundret <- max(subset_twohundretfifty_threehundret_meters$subset_ROV_area_twohundretfifty_threehundret, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_twohundretfifty_threehundret_meters$Primnoa_count_subset <- ifelse(subset_twohundretfifty_threehundret_meters$label_name=='Primnoa stet.', 1, 0)
subset_twohundretfifty_threehundret_meters$Crustacea_count_subset <- ifelse(subset_twohundretfifty_threehundret_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_twohundretfifty_threehundret_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_twohundretfifty_threehundret_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_twohundretfifty_threehundret_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_twohundretfifty_threehundret_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_twohundretfifty_threehundret_meters %>%mutate(Primnoa_cumulated_count_subset_twohundretfifty_threehundret = cumsum(Primnoa_count_subset)) -> subset_twohundretfifty_threehundret_meters
subset_twohundretfifty_threehundret_meters %>%mutate(Crustacea_cumulated_count_subset_twohundretfifty_threehundret = cumsum(Crustacea_count_subset)) -> subset_twohundretfifty_threehundret_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_twohundretfifty_threehundret_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_twohundretfifty_threehundret/subset_ROV_area_twohundretfifty_threehundret) -> subset_twohundretfifty_threehundret_meters
subset_twohundretfifty_threehundret_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_twohundretfifty_threehundret/subset_ROV_area_twohundretfifty_threehundret) -> subset_twohundretfifty_threehundret_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_twohundretfifty_threehundret <- tail(subset_twohundretfifty_threehundret_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_twohundretfifty_threehundret <- tail(subset_twohundretfifty_threehundret_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_twohundretfifty_threehundret_meters %>% write.csv("anevik_1_11_densities_subset_twohundretfifty_threehundret.csv")


### repeat steps above for filtering the next subset of 301-350m distance travelled, copy and paste consecutive distance travelled value (cell value +1 distance travelled of previous subset end value) to avoid duplicates
dataframe %>% filter (between (dataframe$distance_travelled,301.954519565726,350.947295845965)) -> subset_threehundret_threehundretfifty_meters
###calculate subset distance travelled based on subtraction of maximum distance travelled and minimum distance travelled in subset
subset_threehundret_threehundretfifty_meters %>% mutate(ROV_area_distance_travelled_threehundret_threehundretfifty=350.947295845965 - 301.954519565726 ) -> subset_threehundret_threehundretfifty_meters

### mutate new column with calculation of subset seafloor area covered in squaremeters along the whole subset: mean width of video in meters * subset 3D distance travelled in meters
subset_threehundret_threehundretfifty_meters %>%mutate(subset_ROV_area_threehundret_threehundretfifty=mean_video_width * ROV_area_distance_travelled_threehundret_threehundretfifty) -> subset_threehundret_threehundretfifty_meters
### retrieve maximum subset zero_fifty meters seafloor area as single value
subset_max_ROV_area_threehundret_threehundretfifty <- max(subset_threehundret_threehundretfifty_meters$subset_ROV_area_threehundret_threehundretfifty, na.rm = TRUE)

### mutate new columns of single species count or counts including higher taxonomic species groups along whole subset of 0-50m distance travelled, using the same logic as above
subset_threehundret_threehundretfifty_meters$Primnoa_count_subset <- ifelse(subset_threehundret_threehundretfifty_meters$label_name=='Primnoa stet.', 1, 0)
subset_threehundret_threehundretfifty_meters$Crustacea_count_subset <- ifelse(subset_threehundret_threehundretfifty_meters$label_name=='Caridea stet.', 1, 0)+ ifelse(subset_threehundret_threehundretfifty_meters$label_name=='Cancer pagurus stet.', 1,0)+ ifelse(subset_threehundret_threehundretfifty_meters$label_name=='Munida stet.', 1,0)+ ifelse(subset_threehundret_threehundretfifty_meters$label_name=='Arthropoda stet.', 1,0)+ ifelse(subset_threehundret_threehundretfifty_meters$label_name=='Brachyura stet.', 1,0)

###mutate new column of cumulated counts (alike abundances) along subset of previously individual species/or species group counts
subset_threehundret_threehundretfifty_meters %>%mutate(Primnoa_cumulated_count_subset_threehundret_threehundretfifty = cumsum(Primnoa_count_subset)) -> subset_threehundret_threehundretfifty_meters
subset_threehundret_threehundretfifty_meters %>%mutate(Crustacea_cumulated_count_subset_threehundret_threehundretfifty = cumsum(Crustacea_count_subset)) -> subset_threehundret_threehundretfifty_meters

###calculate species/or species groups density of subset as species/species group density= subset species/species group cumulated count value/subset cumulated ROV area value
subset_threehundret_threehundretfifty_meters %>% mutate(Primnoa_density_subset = Primnoa_cumulated_count_subset_threehundret_threehundretfifty/subset_ROV_area_threehundret_threehundretfifty) -> subset_threehundret_threehundretfifty_meters
subset_threehundret_threehundretfifty_meters %>% mutate(Crustacea_density_subset = Crustacea_cumulated_count_subset_threehundret_threehundretfifty/subset_ROV_area_threehundret_threehundretfifty) -> subset_threehundret_threehundretfifty_meters


### only select subset total calculated density based on total species count cumulated and subset max. distance travelled, so picking the last value of the density estimates column 
Primnoa_density_subset_threehundret_threehundretfifty <- tail(subset_threehundret_threehundretfifty_meters$Primnoa_density_subset, n=1)
Crustacea_density_subset_threehundret_threehundretfifty <- tail(subset_threehundret_threehundretfifty_meters$Crustacea_density_subset, n=1)

#### export subset data 
subset_threehundret_threehundretfifty_meters %>% write.csv("anevik_1_11_densities_subset_threehundret_threehundretfifty.csv")