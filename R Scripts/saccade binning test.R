Imported <- read_csv(file.choose(), col_names = TRUE) %>% data.frame()  

  bin_size <- 100
  saccades <- seq(from = 250, to = 30000, by = 250)
  saccade_number <- seq(from = 1, to = 120, by = 1)
  saccade_bin_start <- saccades - bin_size
  saccade_bin_end <- saccades + bin_size
  saccade_bin_ranges <-  paste(saccade_bin_start,"-", saccade_bin_end, sep = "")
  
  binned_data <<- data.frame(saccade_bin_start, saccade_bin_end, saccade_bin_ranges, saccade_number)
  names(binned_data) <- c("bin_start","bin_end", "bin_ranges", "saccade_num")
  
  bin_factors <<- as.factor(cut(Imported$Total_dist, breaks =  binned_data$bin_start, labels = FALSE))
  
  
binning_function <- function(x,y) {
  x %>% 
    filter(Total_dist >= y$bin_start, Total_dist < y$bin_end) %>% 
    mutate(duration = max(Elapsed_Time) - min(Elapsed_Time))
  }  
map2_df(Imported, binned_data, binning_function)
  
  test <- Imported %>% 
    mutate(bin_num = bin_factors) %>%
    group_by(bin_num) %>% 
    summarise(bin_duration = max(Elapsed_Time) - min(Elapsed_Time),
              avg_speed = mean(Speed), 
              sd_speed = sd(Speed),
              avg_lanepos = mean(Lateral_Lane_Pos), 
              sd_lanepos = sd(Lateral_Lane_Pos),
              avg_throttle = mean(Throttle_input), 
              sd_throttle = sd(Throttle_input))