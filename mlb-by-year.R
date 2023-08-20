rm(list = ls())

library(tidyverse)
library(caret)
library(ggplot2)
library(data.table)
library(mosaic)

# after loading in our packages we are ready to load our data
data <- read.csv("percent-mlbpitchers-year.csv")
tj_list <- read.csv("tj-list.csv")

# let's get a summary of the data and tommy john list
head(data)
head(tj_list)

# the year data looks very simple so we can create our visualization right away
ggplot(data, aes(x = Year, y = Percent)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_text(aes(label = Percent), vjust = -0.75, color = "black", size = 2.5) +
  labs(title = "Percentage of MLB Pitchers that Underwent Tommy John Surgery",
       x = "Year",
       y = "Percentage") +
  scale_x_continuous(breaks = c(seq(2016, 2023, 1)))

# Now we have a graph that illustrates the percentages
# so now we can play around with the tj_list
# lets find how long this data dates back to first
num_per_year <-
  tj_list %>%
    group_by(Year) %>%
    count()

# now we can create a graph similar to the first one
ggplot(num_per_year, aes(x = Year, y = n)) +
  geom_col(color = "black") +
  labs(title = "Number of Tommy John Surgeries* by Year",
       x = "Year",
       y = "Surgeries")

# so we know this data tracks back until 1974
# now something fun we can do is get the average age of each years TJ
age_and_year <- 
  tj_list %>%
    group_by(Year) %>%
    summarise(Avg_Age = mean(Age, na.rm = TRUE),
              Surgery_Count = n())

# now we can compare the total tjs by year to the average age
ggplot(age_and_year, aes(x = Year, y = Avg_Age)) +
  geom_line(color = "black") +
  geom_point(color = "black") + # was thinking of using surgery count here
  labs(title = "Average Age of Tommy John Surgeries by Year",
       x = "Year",
       y = "Average Age")

# that was using a line graph, now we can use a bar graph like we did because that will look better with three variables
# turns out I don't really like how any of the three variable ones look
ggplot(age_and_year, aes(x = Year, y = Avg_Age)) +
  geom_col(fill = age_and_year$Surgery_Count) +
  labs(title = "Number of Tommy John Surgeries* by Year",
       x = "Year",
       y = "Surgeries")

#something I really want to do is a map
# however I needed a shapefile to do this and I tried to find the us census one
library(maps)
us_map <- map_data("state")
state_data <- read.csv("states_stats.csv")

# i needed to make this lowercase because that is how the map data comes
state_data <- state_data %>%
  mutate(HS_State = tolower(HS_State))

#without doing that I would get NAs in the surgery column
merged_state <- merge(us_map, state_data, by.x = "region", by.y = "HS_State", all.x = TRUE)

ggplot(data = merged_state, aes(x = long, y = lat, group = group, fill = TJ_Surgeries)) +
  geom_polygon(color = "black", size = 0.5) +
  scale_fill_gradient(low = "blue", high = "red", name = "Surgery Count") +
  labs(title = "Tommy John Surgeries by State",
       fill = "Number of Surgeries") +
  coord_map()

# Create a bubble plot
# this ended up being pretty ugly but I am not against a bubble plot look
ggplot(data = merged_state, aes(x = TJ_Surgeries, y = reorder(region, -TJ_Surgeries), size = TJ_Surgeries, color = region)) +
  geom_point() +
  scale_size_continuous(range = c(3, 15)) +  # Adjust bubble size range
  labs(title = "Tommy John Surgeries by State",
       x = "Number of Surgeries",
       y = "State") +
  guides(color = "none") +
  theme(axis.text.y = element_text(hjust = 0))  # Adjust y-axis labels alignment


###############################
# I want to load in new data relating to recovery time
recovery_data <- read.csv("recovery-times.csv")
monthly_data <- read.csv("monthy_surgery.csv")

# first lets look at the age and return percentages
# to do this we have to make the percentage col into whole numbers
recovery_data <- recovery_data %>%
  mutate(return_percent = as.integer(gsub("%", "", Return.)))

# I want decimal numbers too for this next graph
recovery_data <- recovery_data %>%
  mutate(DecimalNumber = as.numeric(sub("%", "", Return.)) / 100)

#to make this stacked graph work I need to multiply some columns
recovery_data$ReturnTP <- recovery_data$Surgeries * recovery_data$DecimalNumber

#now we can make a graph that illustrates the relationship
# I want to try to illustrate all of them in one graph
ggplot(recovery_data, aes(x = Age)) +
  geom_bar(aes(y = Surgeries, fill = "Surgery Count"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = ReturnTP, fill = "Return to Play"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Return to Play" = "blue", "Surgery Count" = "red")) +
  labs(title = "Comparison of Surgery Count and Returning to Play by Age Range",
       x = "Age Range (years old)",
       y = "Count") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Another thing I want to represent is the recovery times
# this will probably be my last visualization
ggplot(recovery_data, aes(x = Age, y = Average)) +
  geom_col(fill = "black") +
  geom_text(aes(label = Average), vjust = -0.75, color = "black", size = 2.5) +
  labs(title = "Average Recovery Time for Tommy John Surgeries by Age",
       x = "Age Range (years old)",
       y = "Average Recovery Time (months)")
