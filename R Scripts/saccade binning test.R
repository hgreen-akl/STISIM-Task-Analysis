Imported <- read_csv(file.choose(), col_names = TRUE) %>% data.frame() %>% as_tibble()

  bin_size <- 100
  saccades <- seq(from = 250, to = 30000, by = 250)
  saccade_number <- seq(from = 1, to = 120, by = 1)
  saccade_bin_start <- saccades - bin_size
  saccade_bin_end <- saccades + bin_size
  saccade_bin_ranges <-  paste(saccade_bin_start,"-", saccade_bin_end, sep = "")
  
  bins <<- data.frame(saccade_bin_start, saccade_bin_end, saccade_bin_ranges, saccade_number) %>% as_tibble()
  names(binned_data) <- c("bin_start","bin_end", "bin_ranges", "saccade_num")
  
  bin_factors <<- as.factor(cut(Imported$Total_dist, breaks =  binned_data$bin_start, labels = FALSE))
  
  binned_data["nested"] <- NA
binning_function <- function(saccade_bin_start, saccade_bin_end, saccade_bin_ranges, saccade_number) {
 Imported %>% 
  filter(Total_dist >= saccade_bin_start, Total_dist < saccade_bin_end) %>% as.tibble()
  }  
func_output <- pmap(bins, binning_function)
binned_data <- bins %>% mutate(nested = pmap(bins, binning_function)) %>% as_tibble()





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
  
  
  
tested <- function(x) {
  print(x$bin_start,x$bin_end)
}

trial1 <- lmap(binned_data, tested)
  
  
test2 <- Imported %>% filter(Total_dist >= binned_data$bin_start[1] & Total_dist < binned_data$bin_end[1]) %>% as_tibble()
  
  nested_bins <- binned_data
  nested_bins["nested"] <- NA
  nested_bins$nested[1] <- test2 %>% nest()
  nested_bins$duration <- NA
  nested_bins$duration <- max(nested_bins$nestedElapsed_Time) - min(nested_bins$nestedElapsed_Time)
  nested_bins$nested[[Elapsed_Time]]

splitted <- str_split(binned_data$bin_ranges, pattern = "-")    
