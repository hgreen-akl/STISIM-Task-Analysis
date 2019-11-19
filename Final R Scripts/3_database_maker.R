library(tidyverse)


directory <- choose.dir() #where you have your files
filenames <- list.files(path = directory,pattern = "*_summarised.csv", full.names = TRUE, recursive = TRUE)
your_data_frame <- do.call(rbind,lapply(filenames,read.csv))

write.csv(your_data_frame, file.choose())
