# Method to import the csv 

library(tidyverse)

file_to_check <- file.choose()
csv_to_check <- read_csv(file_to_check, col_names = TRUE) %>% data.frame() %>% as_tibble()

to_plot <- csv_to_check %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value")

plot_by_time <- ggplot(to_plot, aes(Elapsed_Time, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
plot_by_time


plot_by_distance <- ggplot(to_plot, aes(Total_dist, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
plot_by_distance

csv_to_check %>% ggplot(aes(x = Steering_count, y = Lateral_Veloc)) + geom_point() + ylim(-50,50)


check_raw_data <- function(x) {
 x %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value") %>% 
    ggplot(aes(Elapsed_Time, value)) + 
    geom_line() + 
    facet_wrap(~ Variable, scales = "free") %>% plot()
  

  
  check1 <- menu(c("Ok to Proceed", "Need to Fix"),graphics = TRUE, title = "Is Data OK??")
  
  stopifnot(check1 == 1)
  
  by_dist <- x %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value") %>%
    ggplot(aes(Total_dist, value)) + 
    geom_line() + 
    facet_wrap(~ Variable, scales = "free") %>% print()

 
   check2 <- menu(c("Ok to Proceed", "Need to Fix"),graphics = TRUE, title = "Is Data OK??")
   stopifnot(check2 == 1)
   
   x
}

check_raw_data(csv_to_check)
checked_data <- check_raw_data(csv_to_check)
checked_data



check_data <- function() {
  csv_to_check <- read_csv(filename, col_names = TRUE) %>% data.frame() %>% as_tibble()
  
  to_plot <- csv_to_check %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value")
  
  plot_by_time <- ggplot(to_plot, aes(Elapsed_Time, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
  plot_by_time
}

check_data()
