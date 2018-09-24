library(tidyverse)

## Loading of the file containg the list of existing files. 
filename_Database <- "G:\\Team Drives\\Research Team\\Projects\\2018 Driving Sim Reproducibility\\Participant Information.xlsx" 
list_of_DAT_files <- read_xlsx(filename_Database, sheet = "Scenario Coding") %>% 
  mutate(pattern_name = paste0(Participant,"_", Run_Number))

list_of_DAT_files %>% head()

## function to ask which number row to check

number <- readline("What is the value of x?") %>% as.numeric()
x <- list_of_DAT_files$pattern_name[readline("What is the value of x?") %>% as.numeric()]

## if a checked file exists it will give an output to say so or else it will load a file to check

file_to_check <- if(list_of_DAT_files$Scenario[number] == "Familiarisation"){ print("CSV file is a Familiarisation") 
  } else {
    list.files(path = "G:/Team Drives/Research Team/Projects/2018 Driving Sim Reproducibility/3_Raw Data/",
                pattern = paste0("?",x,"_trimmed"), recursive = TRUE, full.names = TRUE)}

csv_to_check <- read_csv(file_to_check, col_names = TRUE) %>% data.frame() %>% as_tibble()

csv_to_check <- csv_to_check %>% mutate(session = rep(list_of_DAT_files$Session[number],times = nrow(csv_to_check)) ,scenario = rep(list_of_DAT_files$Scenario[number],times = nrow(csv_to_check)))

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

check_data <- csv_to_check

save_location <- str_replace(file_to_check, "trimmed", "checked")

write.csv(checked_data)