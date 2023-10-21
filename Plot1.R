# Downloading the zip file to project folder and saving it as a text file
url <- paste0("https://d396qusza40orc.cloudfront.net/",
              "exdata%2Fdata%2Fhousehold_power_consumption.zip")
tmp <- tempfile()
download.file(url, tmp)
data <- unlist(read.table(unz(tmp, "household_power_consumption.txt")))
unlink(tmp)

# The first row of the text file is column headers
# extracting the column headers and removing it from data set
names(data) <- c(NULL)
column_names <- unlist(strsplit(data[1], ";"))
data <- data[-1]

# extracting the indexes of 1st Feb and 2nd Feb rows from the data set
indexes <- grep("^([12]/2/2007.*)", data)
# adding 1 to the last index to obtain first index of 3rd Feb
indexes[length(indexes)]+1
# Extracted indexes and the first index of 3rd Feb is the data set required
use_data <- data[c(indexes, indexes[length(indexes)]+1)]

# defining an empty data frame with 9 columns and column names
hh_Pconsumption <- data.frame(matrix(vector(), 0, 9,
                                     dimnames = list(c(), column_names)))

# Populating the data frame with data values, row-wise
for(i in 1:length(use_data)) {
  hh_Pconsumption[nrow(hh_Pconsumption)+1, ] <- unlist(strsplit(use_data[i],
                                                                ";"))
}

# combining the Date and Time columns to get a single column with both date
# and time
library(tidyverse)
hh_Pconsumption <- hh_Pconsumption %>%
  unite(datetime, Date, Time, remove = FALSE, sep = " ")

# converting the datetime to datetime format with as_datetime from
# lubridate package and
# converting numeric columns to numeric format
hh_Pconsumption <- hh_Pconsumption %>%
  mutate(datetime = as_datetime(datetime, format = "%d/%m/%Y %H:%M:%S"),
         Global_active_power = as.numeric(Global_active_power),
         Global_reactive_power = as.numeric(Global_reactive_power),
         Voltage = as.numeric(Voltage),
         Global_intensity = as.numeric(Global_intensity),
         Sub_metering_1 = as.numeric(Sub_metering_1),
         Sub_metering_2 = as.numeric(Sub_metering_2),
         Sub_metering_3 = as.numeric(Sub_metering_3)
  )

# Generating Plot 1
png("Plot1.png", width = 480, height = 480, units = "px", bg = "white")
with(hh_Pconsumption, hist(Global_active_power, col = "red",
                           xlab = "Global Active Power (kilowatts)",
                           main = "Global Active Power"))
dev.off()