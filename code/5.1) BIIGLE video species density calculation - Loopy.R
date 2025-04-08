library(tidyverse)
library(readr)
library(plotly)
library(tidyverse)




#===============================================================================================================
# load output from script 4) BIIGLE video 3D distance travelled.R
paste0("./Output/distancetravelled_biigleannotation.csv") %>%
  read_csv() ->  dataset

# open the table of annotations
arranged_annotations_file <-  paste0("./annotations/","arranged_generic_annotation.csv" )
read_csv(arranged_annotations_file) -> arranged_biigle_annotations 

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

arranged_biigle_annotations %>% select(endsec, startsec, frames)  


# attach metadata at the time something disappear (last second - where nearest to the camera) ----------

# join to nearest timestamp in metadata  
by <- join_by(  closest(endsec  <= sec_in_video )) # !! joining by closest time stamp - MAY NOT BE EXACT !!!!!!!!!!!
time_stamps  %>%  
  left_join(arranged_biigle_annotations, . ,by , suffix = c(".annotations", ".metadata"))   -> dataset

# attach the width of the seabed to the dataset
dataset %>% left_join(navigation_data %>% select(X,Y,depth = depth_interpolated,width,   realtime , bin ), by = c("realtime" = "realtime")) -> dataset


# plot XY depth in 3d with the bin for color
plot_ly(dataset, x = ~X, y = ~Y, z = ~-depth,
        color = ~bin, 
        # set the color palette to rainbow
        #colors = rainbow(length(breaks)),
        colors = "Set3",
        type = "scatter3d", mode = "markers", 
        # smaller pch size
        marker = list(size = 2))  %>%
  layout(title = "XY depth in 3D with bin color")

# loop over each bins and calculate the the number of occurrence of each label_name
bins_list <- list()

for (i in unique(dataset$bin)){
  dataset %>% filter(bin == i)  -> bin.i 
  
  bin.i %>% count(label_name) -> bin.i.counts
  # attach to the list
  bin.i.counts$bin <- i 
  bins_list[[i]] <- bin.i.counts
  
}

bins_list %>% bind_rows() -> bins_df

# pivot the table to have the label_name as columns and bins as rows
bins_df %>%
  pivot_wider(names_from = label_name, values_from = n) %>% 
  replace(is.na(.), 0) -> bins_df


#### export table of species abundance per bin
bins_df %>% 
  write_csv(paste0("./Output/50mbins_species_abundances.csv" ) )
  

# calculate the density of each species in each bin
# divide each abundance value in bin_df by the corresponding bin surface in bins_metadata.
bins_df %>% 
  select(-bin) %>% 
  map(~ .x / bins_metadata$bin_surface) %>% 
  bind_cols(bins_df %>% select(bin)) %>% 
  # Add the bins names back 
bind_cols(bins= bins_df$bin, .) -> bins_df_density
 

#### export tables of species densities per bin
bins_df_density %>% 
  write_csv(paste0("./Output/50mbins_species_densities.csv" ) )


# = = = = = = = = = = = = = = = = = = = 
# make a quick PCA and plot where the bins are in the 2d space of the main axis
bins_df %>% 
  select(-bin) %>% 
  prcomp(center = TRUE, scale. = TRUE) -> bins_pca

# transform the bins into a color vector where the bins names are replaced by color codes
# Get unique levels in the vector
unique_levels <- unique(bins_df$bin)# to get a bind as an id

unique_levels <- unique(bins_metadata$mean_depth) 

# Choose a color palette with the number of unique levels
color_palette <-  rainbow(length(unique_levels))
# Create a named vector to map unique levels to colors
color_mapping <- setNames(color_palette, unique_levels)
# Map the characters to colors
char_colors <- color_mapping[unique_levels %>%  as.character()]
# plot it 
plot_ly() %>%
  add_trace(x = bins_pca$x[,1], y = bins_pca$x[,2], z = bins_pca$x[,3],
            color = unique_levels, type = "scatter3d",
            mode = "markers") %>%
  # legend = list(title = "Depth (m)", traceorder = "reversed"),
  layout(title = "PCA of species density in 50m bins", 
         scene = list(xaxis = list(title = "PC1"),
                      yaxis = list(title = "PC2"),
                      zaxis = list(title = "PC3")),
         # add a title to the legend and reverse the color scale
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'Depth (m)',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         )
  )
         

#### export table of species abundance per bin
bins_df %>% 
  write_csv(paste0("./Output/50mbins_species_abundances.csv" ) )
  
# calculate the density of each species in each bin
# divide each abundance value in bin_df by the corresponding bin surface in bins_metadata.
bins_df %>% 
  select(-bin) %>% 
  map(~ .x / bins_metadata$bin_surface) %>% 
  bind_cols(bins_df %>% select(bin)) %>% 
  # Add the bins names back 
bind_cols(bins= bins_df$bin, .) -> bins_df_density
 

#### export tables of species densities per bin
bins_df_density %>% 
  write_csv(paste0("./Output/50mbins_species_densities.csv" ) )

bins_df
bins_df_density <- bins_df %>%
  select(-bin) %>%
  map(~ .x / bins_metadata$bin_surface) %>%
  bind_cols(bins_df %>% select(bin)) %>%
  bind_cols(bins= bins_df$bin, .)

 
bins_df_density %>% 
  # remove the all 
  filter(bin != "All(0,950]") %>%
  # ignore
  select(-bins) %>% 
  prcomp(center = TRUE, scale. = TRUE)


bins_df_density %>% 
  # remove the all 
  filter(bin != "All(0,950]") %>%
  # ignore
  select(-bins) -> bins_forpcs

# show column types in bins_forpcs
bins_forpcs %>% 
  map_df(~ class(.x)) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("column") %>% 
  rename(type = value)  




