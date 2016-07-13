# Unit 7 - Recitation


# VIDEO 3 - Bar Charts

# Load ggplot library
library(ggplot2)

# Load our data, which lives in intl.csv
intl <- read.csv("intl.csv")
str(intl)

# We want to make a bar plot with region on the X axis
# and Percentage on the y-axis.
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=PercentOfIntl))

# what is this stat = "identity"?
# Well, it's pretty simple. Geometry bar has multiple modes of operation, 
# and stat = "identity" says, use the value of the y variable as is, which is what we want.

# Make Region an ordered factor
# We can do this with the re-order command and transform command. The negative sign in front of the variable
# "(-PercentOfIntl)" means decreasing order
intl <- transform(intl, Region = reorder(Region, -PercentOfIntl))

# Look at the structure
str(intl)

# Make the percentages out of 100 instead of fractions
intl$PercentOfIntl = intl$PercentOfIntl * 100

# Make the plot
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
        geom_bar(stat="identity", fill="dark blue") +
        geom_text(aes(label=PercentOfIntl), vjust=-0.4) +
        ylab("Percent of International Students") +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# "vjust" moves where the value sits above the bar. (+) raises. (-) lowers the value
)
# VIDEO 5 - World map

# Load the ggmap package
library(ggmap)

# Load in the international student data
intlall <- read.csv("intlall.csv",stringsAsFactors=FALSE)

# Lets look at the first few rows
head(intlall)

# Those NAs are really 0s, and we can replace them easily
intlall[is.na(intlall)] = 0

# Now lets look again
head(intlall) 

# Load the world map
world_map <- map_data("world")
str(world_map)

# Lets merge intlall into world_map using the merge command
world_map <- merge(world_map, intlall, by.x ="region", by.y = "Citizenship")
str(world_map)

# Plot the map.
# Note "group=group" means group countries into their own polygons
ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(fill="white", color="black") +coord_map("mercator")

# Reorder the data
world_map <- world_map[order(world_map$group, world_map$order),]

# Redo the plot
ggplot(world_map, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black") +coord_map("mercator")

# Lets look for China - mismatch of names of China between merge data sets. Therefore data dropped off.
table(intlall$Citizenship)

# Lets "fix" that in the intlall dataset
intlall$Citizenship[intlall$Citizenship=="China (People's Republic Of)"] = "China"

# We'll repeat our merge and order from before
world_map <- merge(map_data("world"), intlall, 
                  by.x ="region",
                  by.y = "Citizenship")

world_map <- world_map[order(world_map$group, world_map$order),]

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator")


# We can try other projections - this global #d view is visually interesting
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(20, 30, 0))
# Change view change orientation coordinates

ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation=c(-37, 175, 0))

#VIDEO 6 - Scales

# VIDEO 7 - Line Charts

# First, lets make sure we have ggplot2 loaded
library(ggplot2)

# Now lets load our dataframe
households <- read.csv("households.csv")
str(households)

# ggplot needs aesthetic command (aes) in the order year, group, function.
# Use "melt" function from reshape2 package to convert the data into a form we want.
# Load reshape2
library(reshape2)

# Lets look at the first two columns of our households dataframe.
# [ rows, columns] in this case want all rows, so leave pace before comma.
households[,1:2]

# First few rows of our melted households dataframe
head(melt(households, id="Year"))

households[,1:3]

melt(households, id="Year")[1:10,3]
melt(households, id="Year")[1:10,]

# Plot it
ggplot(melt(households, id="Year"),       
       aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) + geom_point(size=5) +  
  ylab("Percentage of Households")
