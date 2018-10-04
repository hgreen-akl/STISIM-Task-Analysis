
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
  save_location <- str_replace(file_to_check, "trimmed", "checked")   
  existing_file <- if_else(file.exists(save_location), TRUE, FALSE) 
  code <- paste0(x$ID[1] %>% as.character(),"_" ,x$Run_Number[1] %>% as.character())
  if(existing_file == TRUE) {
     overwrite <- menu(c("Overwrite","Stop"), graphics = TRUE, title = "trimmed csv data already exists do you wish to overwrite") %>% as.numeric()
     if(overwrite == 2) {
       stop("ERROR:: file already exists")
      } else {
       write.csv(x, file = save_location, row.names = FALSE)
       print(paste0(code,"_checked.csv is saved"))
  } } 
    else if (existing_file == FALSE) {
  write.csv(x, file = save_location, row.names = FALSE)
  return(paste0(code,"_checked.csv is saved")) 
    } else {
    stop("ERROR :: error in saving process save manually")
        }
  } 

## if a checked file exists it will give an output to say so or else it will load a file to check

load_trimmed <- function(x, df) {
  pat <- df$pattern_name[x]
  file_to_check <<- list.files(path = "G:/Team Drives/Research Team/Projects/2018 Driving Sim Reproducibility/3_Raw Data/",
                               pattern = paste0("?",pat,"_trimmed"), recursive = TRUE, full.names = TRUE)
  save_location <- str_replace(file_to_check, "trimmed", "checked")   
  existing_file <- if_else(file.exists(save_location), TRUE, FALSE) 
  if (existing_file) {
    recheck <- menu(c("Recheck","Stop"), graphics = TRUE, title = paste(pat, "checked csv data already exists do you wish to overwrite")) %>% as.numeric()
    if(recheck == 2) {
      stop("ERROR:: file already exists")
    }
  }
  csv_to_check <- read_csv(file_to_check, col_names = TRUE) %>% data.frame() %>% as_tibble()
  csv_to_check %>% mutate(session = rep(df$Session[x],times = nrow(csv_to_check)) ,
                          scenario = rep(df$Scenario[x],times = nrow(csv_to_check)))
}



## Loading of the file containg the list of existing files. 
filename_Database <- "G:\\Team Drives\\Research Team\\Projects\\2018 Driving Sim Reproducibility\\Participant Information.xlsx" 
list_of_DAT_files <- read_xlsx(filename_Database, sheet = "Scenario Coding") %>% 
  mutate(pattern_name = paste0(Participant,"_", Run_Number))

list_of_DAT_files %>% glimpse()

files2 <- list_of_DAT_files %>% filter(Scenario == "Country", CSV_checked == "No")
files3 <- list_of_DAT_files %>% filter(Scenario == "Urban", CSV_checked == "No")



## once functions are loaded you can change x and then run this block once to check then save, change x and repeat
x <- 49
csv_to_check <- x %>% load_trimmed(df = files2)
csv_to_check %>% plot_by_distance()
csv_to_check %>% plot_by_time()

##USE for urban environments
## csv_to_check <- x %>% load_trimmed(df = files3) %>% filter(Lateral_Veloc < 100 & Lateral_Veloc> -100)

## Some commonly used filters and plots to cover and check unexplainable spikes in lateral lane deviations
csv_to_check <- csv_to_check %>% filter(Lateral_Veloc < 100 & Lateral_Veloc> -100)
csv_to_check %>% filter(Total_dist > 29000) %>% ggplot(aes(x = Total_dist, y = Lateral_Lane_Pos)) + geom_line()


## function to save the data, errors will be presented if there is an existing checked file

csv_to_check %>% save_data()
x <- x+1

