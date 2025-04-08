# BIIGLE-video-epifaunal-density-estimation

**notes**: make sure time columns in your raw data are all in the same time-zones or let R assume they are UTC/your local time zone

# order of script needed 
- DONE: load Metadata in
- DONE: load observation in
- run scrip[name].r
- results are in folder
- run script script2[name2].r
- results are in ...
- Run script number 4 to calibrate the image size - USER INPUT FOR CAMERA PARAMETERS NEEDED HERE !!!! 


*NEEDS: *
1) DONE: dummy data. so that the code can run directly when cloning the repo: Birte has uploaded dummy data of navigation (removal of first digit of both latutude and longitude) and dummy data of annotations (prelimnary BIIGLE annotation from the same transect used in the navigation data 
2) the data that is used in the paper? Birte: upload of prelimnary annotation, need some help to verify species ID in near future
3) arrange scripts order 
    - Deal with all the navigation stuff before merging with annotation (except use of laser annotation in clibration script)
4) tidy up scripts - maybe put into QMDs - maybe condensate into several scripts Birte has started to tidy up 

**tasks identified at 07/03/2025 meeting**
- Interpolate DEPTH values !!! 
- find out the right UTM zone and convert to it
- check numnbers rounding and precision - DONE
- Check how the numbers get rounded + make sure nothing is lost  - DONE
- Make some code to display the smoothed coordinates of the original ones - DONE
- Calculate total sum of each species abundance 
- Make bins of 50 m transects and calculate density within these subsections - DONE
- average depth and other factors values in these bins 
