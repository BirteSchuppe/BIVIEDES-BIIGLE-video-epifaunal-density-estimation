

# proocessing metadata ========================================================================





# =======================================================================================
# Add a bin column
# =======================================================================================


# open image calibration info to calculate the bin surface
paste0("./Output/dist_calibrated_smoothed_navigation.csv" ) %>%  read_csv() -> joint_navigation_annotation

# set a bin size in m 
bin_size <- 50

# create bins of 50m distance travelled 
breaks <- seq(0, max(joint_navigation_annotation$distance_travelled) + bin_size, by = bin_size)
binned_numbers <- cut(joint_navigation_annotation$distance_travelled, breaks = breaks, include.lowest = TRUE)
joint_navigation_annotation <- mutate(joint_navigation_annotation, bin = binned_numbers) 


# show the average width in each bin 
joint_navigation_annotation %>%  group_by(bin) %>% 
  summarise(n = n(), 
            # caluculate the mean depth in each bin
            mean_depth = mean(depth, na.rm = TRUE),
            # and the width
            mean_width = mean(width, na.rm = TRUE),
  ) %>% 
  # surface of a bin is the average width * 50m
  mutate(bin_surface = mean_width * bin_size) -> bins_metadata

# add a line to the tabl showing the average values for the entire table
bins_metadata %>% 
  summarise(mean_depth = mean(mean_depth),
            mean_width = mean(mean_width),
            bin_surface = sum(bin_surface)) %>% 
  mutate(bin = paste0( "All(",breaks %>% head(1),",",breaks %>% last() ,"]"), n = nrow(joint_navigation_annotation) ) %>% 
  bind_rows(bins_metadata) -> bins_metadata


# expor the smoothed and calibrated navigation into the 
joint_navigation_annotation %>%
  write_csv(paste0("./Output/bins_dist_calibrated_smoothed_navigation.csv" ) )

