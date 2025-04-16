# BIIGLE-video-epifaunal-density-estimation

**notes**: make sure time columns in your raw data are all in the same time-zones or let R assume they are UTC/your local time zone

make sure the navigation data is 1s frequency 

make sure the video time starts at 0s
Birte: probably need to replace the starting video time in the navigation csv file? often I have to remove the first logged navigation row(s) of the csv, since lat and long are often not matching with the lat and long video overlay.This results that the column "sperre log reltime" starts at +2 seconds or more.....
Often the column Sperre log reltime has some time lags from the sensor I assume, so need to make sure that this column having the same time intervalls along the whole file..... 
UPDATE: Birte has fixed time in video, starting at 0 seconds and uploaded to nav folder as new file. However, it was observed that sometimes there is a mis-match between time in video 8duration) and timestamp...
# order of script needed 
- DONE: load Metadata in
- DONE: load observation in
- run scrip[name].r
- results are in folder
- run script script2[name2].r
- results are in ...
- Run script number 4 to calibrate the image size - USER INPUT FOR CAMERA PARAMETERS NEEDED HERE !!!! 


**Input: ** There are two input files needed for the scripts to run.
1) annotations: this is the standard Biigle Video annotations csv file with 1-frames annotations and lasers marked regularly
2) navigation: with time in real-world, time in video and XYZ coordinates, depth, pitch, roll, yaw, and camera parameters (focal length, sensor size, etc.) - this is the standard Biigle Video navigation csv file with 1-frame navigation data and lasers marked regularly

|  datetime| video time | lat | long | depth | other metadata |
| :---: | :---: | :---: | :---: | :---: | :---: |
| dmy_hms | hms | dec.deg | dec.deg | (-)meters | XXX |
| dmy_hms | hms | dec.deg | dec.deg | (-)meters | XXX | 





*NEEDS: *
1) DONE: dummy data. so that the code can run directly when cloning the repo: Birte has uploaded dummy data of navigation (removal of first digit of both latutude and longitude) and dummy data of annotations (prelimnary BIIGLE annotation from the same transect used in the navigation data 
2) the data that is used in the paper? Birte: upload of prelimnary annotation, need some help to verify species ID in near future
3) arrange scripts order 
    - Deal with all the navigation stuff before merging with annotation (except use of laser annotation in calibration script)
    - make bins on the navigation to avoid biases due to non-homogeneous sampling event
    - merge nav and annotation in last
4) tidy up scripts - maybe put into QMDs - maybe condensate into several scripts Birte has started to tidy up 

**tasks identified at 07/03/2025 meeting**

- find out the right UTM zone and convert to it
- 
