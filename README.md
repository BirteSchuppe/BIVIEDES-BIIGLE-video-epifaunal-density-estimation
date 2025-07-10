# BIIGLE-video-epifaunal-density-estimation




**Input: ** There are two input files needed for the scripts to run.
1) annotations: this is the standard Biigle Video annotations csv file with 1-frames annotations ( tracked annotation will not work) and lasers marked regularly, wheras each dot was marked individually using the "point annotation tool"
2) navigation: Following data needs to be extracted from the raw data derived from the (ROV) GPS transponder: with time in real-world, time in video and XYZ coordinates, depth, pitch, roll, yaw, and camera parameters (focal length, sensor size, etc.) - this is the standard Biigle Video navigation csv file with 1-frame navigation data and lasers marked regularly

|  datetime| video time | lat | long | depth | other metadata |
| :---: | :---: | :---: | :---: | :---: | :---: |
| dmy_hms | hms | dec.deg | dec.deg | (-)meters | XXX |
| dmy_hms | hms | dec.deg | dec.deg | (-)meters | XXX | 

Pre-processing of the navigation data can be done by selecting relevant data described about, and replace their column names to match 
** replace the column names here
names(your_raw_navigation)[1:6] <- c("datetime","depth","heading","lon","lat", "videotime")

**notes**: make sure time columns in your raw data are all in the same time-zones or let R assume they are UTC/your local time zone

*** Description of workflow (NOTE: the order of script applied has to be followed chronically !!!)****

***Script 1*** 
was applied to the BIIGLE annotation csv. report to sort the annotations chronically by time in video, represented by chronically order of “frames” column. 
This ensures that “frames” and thus time in video is the regulating column for arranging any annotations and not as observed initially, the column “created_at”.  

