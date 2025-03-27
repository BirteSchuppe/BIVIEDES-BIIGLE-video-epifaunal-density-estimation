library(dplyr)
library(stringr)
library(readr)
library(purrr)

#### set direction of BIIGLE annotations in zipped folder
"D:/PHD/ROV/BIIGLE files/63" -> reports_folder
# list the csv tables - 


list.files("./annotations", pattern = ".csv$") %>% 
# exclude the "arranged" pattern 
str_subset("arranged", negate = T) -> generic_annotation_file

 
# 2) make a table --------------------------------------------------------------------------------

tibble(file = generic_annotation_file ) %>%
  mutate(table_name = str_remove(file,pattern = ".csv")) -> annotationTables
  # make a column of volume ID number
    
    read_csv( paste0("./annotations/",generic_annotation_file )  ) -> All_annotations
  
  # add the metadata 
    All_annotations %>% 
      mutate(table_name = annotationTables$table_name) -> All_annotations

### sort annotations chronically by time in video, represented by chronically order of "frames" column
All_annotations %>%
  # make frame to numerical
  mutate(frame_new =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~head(.x,1) %>% as.numeric)) -> All_annotations

# sort per image frame 
All_annotations %>%  arrange(frame_new) -> All_annotations
 
# export re-arranged annotations 
write_csv(All_annotations, paste0("./annotations/arranged_",generic_annotation_file )  )
