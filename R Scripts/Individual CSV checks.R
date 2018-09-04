# Method to import the csv 

library(tidyverse)
csv_to_check <- read_csv(file.choose()) 

to_plot <- csv_to_check %>% gather(Longitudinal_Veloc:Road_Curve, key = "Variable", value = "value")

plot_by_time <- ggplot(to_plot, aes(Elapsed_Time, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
plot_by_time

plot_by_distance <- ggplot(to_plot, aes(Total_dist, value)) + geom_line() + facet_wrap(~ Variable, scales = "free")
plot_by_distance

csv_to_check %>% ggplot(aes(x = Steering_count, y = Lateral_Veloc)) + geom_point() + ylim(-50,50)


ggplot(csv_to_check, aes(x = ))

