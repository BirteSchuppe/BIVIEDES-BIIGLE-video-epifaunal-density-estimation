library(dplyr)
library(stringr)
library(readr)
library(purrr)


"D:/PHD/ROV/BIIGLE files/63" -> reports_folder
# list the csv tables - 
list.files(reports_folder, pattern = "26-anevik-1-11.csv") -> files

# 2) make a metadata table --------------------------------------------------------------------------------

tibble(file = files ) %>%
  mutate(table_name = str_remove(file,pattern = ".csv")) %>% 
  # make a column of volume ID number
  mutate(volume = str_remove(table_name,pattern = "26-anevik-1-11.csv") ) -> annotationTables




# make a list to store all transformed tables tables
Dframes  <- as.list(1:nrow(annotationTables))

for (i in seq(Dframes)) {
  # select the table number i
  annotationTables %>% slice(i) -> meta.i
  # import it
  meta.i %>% pull(file ) %>% paste(reports_folder,.,sep = "/") %>%
    read_csv( ) -> D.i
  
  # add the metadata 
  bind_cols( D.i,meta.i) ->  Dframes[[i]]
  
}

# This is your table of everything 
Dframes %>% bind_rows() -> All_annotations


All_annotations$created_at %>% plot()
View(All_annotations)

# sort per image names 
All_annotations %>%  arrange(filename)

# sort per image IDs
All_annotations %>%  arrange(image_id)

All_annotations %>%
  # make frame to numerical
  mutate(frame_new =    frames %>% str_sub( 2, -2)  %>% 
           str_replace("null",replacement = "0") %>% 
           str_split(pattern = ",") %>%
           map_vec(~head(.x,1) %>% as.numeric)) -> All_annotations

# sort per image frame 
All_annotations %>%  arrange(frame_new) -> All_annotations
View(All_annotations)

write_csv(All_annotations, "26-anevik-1-11_arranged.csv")

# sort the rows per time they were created 
All_annotations %>%  arrange(created_at %>% desc) 

# last annotations first just to see
All_annotations %>%  arrange(created_at %>% desc) 