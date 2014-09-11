# This script will identify coincident pairs within a set of Gowalla (or other social media) data, meaning two unique UserIDs at the same location at the 'same time' (as defined by you!). The results will be output to a new CSV file named 'coincident_pairs.csv'

# Before beginning, make sure your data is properly conditioned:
#	1. Heading for the column containing date should be 'Date' and format should be 'yyyy-mm-dd'
#	2. Heading for the column containing time should be 'Time' and format should be 'hh:mm:ss AM/PM'
#	3. Heading for the column containing the userID or other unique identifier should be 'UserID'
#	4. Location format:
#		a. If using a unique location ID code to identify location, the column header should be 'CheckinLoc'
#		b. If using coordinates to identify location, the column headers should be 'Lat' and 'Lon' and format should be decimal degrees

# Also be sure to set your working directory to the folder containing the csv file using the 'setwd()' command. 
#	ex. setwd("C:/Users/JohnSmith/Gowalla")

# Once the above is complete, you are ready to run this script... Stand back and prepare to be amazed!

# DISCLAIMER: R holds its data in local memory so the amount of data you can run this against and how long it takes to churn is limited by the size and power of your hardware.

#------------------------------------------------------------------------------------------#

# Load your data
cat("\n","What is the filename for your data? (include the .csv)","\n")
fn = scan(n=1,what="character")
data = read.csv(fn)

# Establish time difference tolerance
cat("\n","What is your time difference tolerance (in minutes)?","\n")
timetol = scan(n=1)

# Add unique IDs to each row (so you can confirm no duplicates in the results)
data$rowID <- 1:nrow(data)

# Combine and format the date and time fields
data$DateTime = paste(data$Date, data$Time)
data$DateTime = strptime(data$DateTime, format="%Y-%m-%d %I:%M:%S %p")

# Sort data in chronological order
data <- data[order(data$DateTime),]

# Create a new empty dataframe with the same structure as data to store results
coincident = data
del = c(0:nrow(data))
coincident = coincident[-del,]

# Find out of you're working with a unique location ID or coordinates
loc <- ""
repeat{
	cat("\n", "Do you want to use a unique ID or coordinates for your location? (specify 'ID' or 'coords')","\n")
	input = tolower(scan(n=1,what="character"))
	valid = (input == "id" || input == "coords")
	if(valid){
		loc = input
		break
	}
	else print("Invalid entry.")
}
	
if(loc == "id"){
	# Compare each row of data to subsequent rows and check three conditions:
	#      1. The time difference is within the tolerance you defined
	#      2. The location ID's (CheckinLoc) are the same
	#      3. It's not the same user (UserID)
	# If these three conditions are met, add the pair to the 'coincident' dataframe
	for(i in 1:nrow(data)){
		for(j in i:nrow(data)){
			if(i != j){	
				time = abs(as.numeric(difftime(data$DateTime[i], data$DateTime[j], units = "mins")))
				if(!is.na(time) && time < timetol){
					if((data$CheckinLoc[i] == data$CheckinLoc[j]) && (data$UserID[i] != data$UserID[j])){
						coincident <- rbind(coincident,data[i,])
						coincident <- rbind(coincident,data[j,])
					}
				}
				else break
			}
		}
	}
}
if(loc == "coords"){
	# Utility functions for distance calculations
	toRadians <- function(coord){
	rads = coord * pi / 180
	return(rads)
	}
	toMeters <- function(km){
		m = km * 1000
		return(m)
	}
	calcDistance <- function(lat1,lon1,lat2,lon2){
		R <- 6371  # Radius of the earth in km
		x1 <- toRadians(lat1)
		x2 <- toRadians(lat2)
		deltaX <- toRadians((lat2 - lat1))
		deltaY <- toRadians((lon2 - lon1))
		# Distance calculation using Haversine formula
		a <- sin²(deltaX / 2) + cos(x1) * cos(x2) * sin²(deltaY / 2)
		c <- 2 * atan2(sqrt(a),sqrt(1-a))
		d <- R * c
		
		return(abs(d))	
	}
	
	# Set your distance threshold and preferred unit of measure
	cat("\n", "What unit of measure do you want to use for distance? (m or km)","\n")
	unitMes = tolower(scan(n=1,what="character"))
	cat("\n", "What is your distance threshold?","\n")
	distol = scan(n=1)
	
	# Time to crunch!
	for(i in 1:nrow(data)){
		for(j in i:nrow(data)){
			if(i != j){	
				time = abs(as.numeric(difftime(data$DateTime[i], data$DateTime[j], units = "mins")))
				if(!is.na(time) && time < timetol){
					dis = calcDistance(data$Lat[i],data$Lon[i],data$Lat[j],data$Lon[j])
					if(unitMes == "m") dis = toMeters(dis)
					if((dis <= distol) && (data$UserID[i] != data$UserID[j])){
						data$Distance[i] <- dis
						data$Distance[j] <- NA
						coincident <- rbind(coincident,data[i,])
						coincident <- rbind(coincident,data[j,])
					}
				}
				else break
			}
		}
	}
}

# Remove duplicates from the 'coincident' results dataframe
coincident = unique(coincident)

# Output the results to a new CSV file for further analysis
filename = "coincident_pairs.csv"
counter = 1
while(file.exists(filename)){
	filename = gsub("\\s","",paste("coincident_pairs","(",counter,")",".csv"))
	counter = counter + 1
}
write.csv(coincident,file=filename)

print("Complete!")
system('CMD /C "ECHO Processing complete! Results were output to a file named "coincident_pairs.csv" in your working directory. && PAUSE"', invisible=FALSE, wait=FALSE)
