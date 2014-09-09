# This script will identify coincident pairs within a set of Gowalla data, meaning two unique UserIDs at the same location at the 'same time' (as defined by you!). The results will be output to a new CSV file named 'coincident_pairs.csv'

# Before beginning, be sure to set your working directory to the folder containing the Gowalla data.

# Next, load the data into a variable called 'data' using the following command:
#     data = read.csv("insert filename here")
# Be sure to include the quotation marks ("") around the filename and suffix (ex. "gowalladata.csv") or it won't import

# Once the above steps are complete, you are ready to run this script... Stand back and prepare to be amazed!

# DISCLAIMER: R holds its data in local memory so the amount of data you can run this against and how long it takes to churn is limited by the size and power of your hardware.

#------------------------------------------------------------------------------------------#

# Load your data
cat("\n","What is the filename for your data? (include the .csv)","\n")
fn = scan(n=1,what="character")
data = read.csv(fn)

# Establish time difference tolerance
cat("\n","What is your time difference tolerance (in minutes)?","\n")
timetol = scan(n=1)

# Combine and format the date and time fields
data$DateTime = paste(data$Date, data$Time)
data$DateTime = strptime(data$DateTime, format="%Y-%m-%d %I:%M:%S %p")

# Sort data in reverse chronological order
data <- data[order(data$DateTime),]

# Add a new column that calulates the time difference in minutes between each subsequent record
data$deltaTime <- abs(as.numeric(difftime(data$DateTime[1:length(data$DateTime)], data$DateTime[2:length(data$DateTime)]), units = "mins", format="%Y-%m-%d %H:%M:%S"))
data$deltaTime[nrow(data)] = NA  # Change the deltaTime entry for the last record to NA

# Remove any NA's
for(i in 1:nrow(data)){
     if(is.na(data$deltaTime[i])) data <- data[-i,]
}

# Create a new empty dataframe with the same structure as data
coincident = data
del = c(0:nrow(data))
coincident = coincident[-del,]

# Compare each row of data to the following row and check three conditions:
#      1. The time difference (deltaTime) is within the tolerance you defined
#      2. The location ID's (CheckinLoc) are the same
#      3. It's not the same user (UserID)
# If these three conditions are met, add the pair to the 'coincident' dataframe
for(i in 1:(nrow(data)-1)){
     x = i+1
     if((data$deltaTime[i] <= timetol) && (data$CheckinLoc[i] == data$CheckinLoc[x]) && (data$UserID[i] != data$UserID[x])){
          coincident <- rbind(coincident,data[i,1:9])
          coincident <- rbind(coincident,data[x,1:9])
     }
}

# Remove the deltaTime column from the 'coincident' dataframe since it's no longer helpful
coincident$deltaTime = NULL

# Output the results to a new CSV file for further analysis
write.csv(coincident,file="coincident_pairs.csv")
print("Processing complete! Results were output to a file named 'coincident_pairs.csv' in your working directory.")