
# Method to import the csv 

library(tidyverse)
library(readxl)
library(xlsx)

## These are functions to call prior that plots the data for checking.
# a function that plots the data x (which should be csv to check) by time, it gathers the data into long form with each variable being indexed, 
# then it plots it in a facted format
plot_by_time <- function(x) {
  data_to_plot <- x %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value")
  ggplot(data_to_plot, aes(Elapsed_Time, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
}

## a function that plots the data x (which should be csv to check) by distance, it gathers the data into long form with each variable being indexed, 
## then it plots it in a facted format
plot_by_distance <- function(x){
  
  data_to_plot <- x %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value")
  ggplot(data_to_plot, aes(Total_dist, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
}

##function used to save the data
save_data <- function(x) {
     selection <- 1
  if (file.exists(save_location)) {
    selection <- menu(c("Overwrite","Stop"), graphics = TRUE, title = "trimmed csv data already exists do you wish to overwrite") %>% as.numeric()
    } 
   if (selection = 2) {
     stop("Stopped saving the file by user request")
   }   if (selection = 1) {
      save_location <- str_replace(file_to_check, "trimmed", "checked")
      write.csv(x, file = save_location, row.names = FALSE)
      print(paste0(x,"_trimmed.csv is saved"))
    }}


## Loading of the file containg the list of existing files. 
filename_Database <- "G:\\Team Drives\\Research Team\\Projects\\2018 Driving Sim Reproducibility\\Participant Information.xlsx" 
list_of_DAT_files <- read_xlsx(filename_Database, sheet = "Scenario Coding") %>% 
  mutate(pattern_name = paste0(Participant,"_", Run_Number))

list_of_DAT_files %>% head()

files2 <- list_of_DAT_files %>% filter(Scenario == "Country")
files3 <- list_of_DAT_files %>% filter(Scenario == "Urban")
## function to ask which number row to check

number <- readline("What is the value of x?") %>% as.numeric()
x <- 5
pat <- files2$pattern_name[x]

## if a checked file exists it will give an output to say so or else it will load a file to check

file_to_check <- list.files(path = "G:/Team Drives/Research Team/Projects/2018 Driving Sim Reproducibility/3_Raw Data/",
                pattern = paste0("?",pat,"_trimmed"), recursive = TRUE, full.names = TRUE)

csv_to_check <- read_csv(file_to_check, col_names = TRUE) %>% data.frame() %>% as_tibble()
csv_to_check <- csv_to_check %>% mutate(session = rep(list_of_DAT_files$Session[x],times = nrow(csv_to_check)) ,
                                        scenario = rep(list_of_DAT_files$Scenario[x],times = nrow(csv_to_check)))

plot_by_time <- ggplot(to_plot, aes(Elapsed_Time, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
plot_by_time

check1 <- menu(c("Ok to Proceed", "Need to Fix"), graphics = TRUE )

stopifnot(check1 == 1)


## Some commonly used filters and plots to cover and check unexplainable spikes in lateral lane deviations
csv_to_check <- csv_to_check %>% filter(Lateral_Veloc < 100 & Lateral_Veloc> -100)
csv_to_check %>% filter(Total_dist > 29000) %>% ggplot(aes(x = Total_dist, y = Lateral_Lane_Pos)) + geom_line()


csv_to_check %>% save_data()


save_location <- str_replace(file_to_check, "trimmed", "checked")
write.csv(checked_data, file = save_location, row.names = FALSE)
print(paste0(x,"_trimmed.csv is saved"))
