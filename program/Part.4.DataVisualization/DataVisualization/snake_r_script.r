############################################################
## Welcome to the R script; let's crush it
############################################################

## 3/22, added my own notes to each section. 
      ###### more notes on boxnotes, visual_communication


## JPJ 22 iii 21
## Modified on 16 March 2022 by Robert Eugene del Carlo, Ph.D. (REdC)

## Change working directory

## Load in data

snake_data <- read.csv("SnakeInfo_2022_03_15_REdC.csv", header=TRUE)
	dim(snake_data)
	head(snake_data)
	
	## column 1 = individual Snake id
	## column 2 = county
	## column 3 = Latitude
	## column 4 = Longitude
	## column 5 = Species_Genotype
	## column 6 = principal component 1 (PC1)
	## column 7 = principal component 2 (PC2)


## make new matrix for maps (county, species_genotype, lat, long)

unique_counties <- unique(snake_data[,2])
# get number of counties


#### MAKE MATRIX
map_matrix <- matrix(0, length(unique_counties), 4)
# make the matrix

for (i in 1:length(unique_counties))
	{
	sub_data <- subset(snake_data, snake_data[,2]==unique_counties[i])
	map_matrix[i,1] <- unique_counties[i]
	map_matrix[i,2] <- sub_data[1,5]
	map_matrix[i,3] <- sub_data[1,3]
	map_matrix[i,4] <- sub_data[1,4]
	}
# POPULATE matrix. 

## make a very basic map

## v0.1: basic
quartz(height=6, width=6)
# claim space on page. 6x6. Gives size
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]))
  # call a blank plot. Will not put in points. 


## v0.2: add graphical parameter formatting using par
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
  # short for parameter. Mar is margins. white space around figure
plot(as.numeric(map_mat[,4]), as.numeric(map_mat[,3]))


## v0.3: standardize x and y limits (make roughly proportional to one another)
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9))
  # ylim & xlim. how much variable the axis will take up. How large. For maps, keep them standard. 1 degree in one axis will be the same in another axis/map/figure
  # keep lat/long degrees the same size. Much more easy in arc

## v0.4: add axis labels
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)")
  # xlab and ylab to add labels. standardize label sizes (cex.lab)

## v0.5: make axis labels larger
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", cex.lab=1.25)
  # standardize label sizes

## v0.6: rotate y axis labels
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", cex.lab=1.25, las=1)
# las = 1, --> makes y-axis values vertical, not horizontal. increases readability. 

## v0.7: fill in points (white points are lame, gray are great)
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", cex.lab=1.25, 
las=1, pch=21, bg="gray")
# adds a background to points. 


## v0.8: make points bigger
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", cex.lab=1.25, 
las=1, pch=21, bg="gray", cex=2.5)
# cex changes size of points. Better readability. 


## v0.9: make points line widths thicker
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", cex.lab=1.25, 
las=1, pch=21, bg="gray", cex=2.5, lwd=1.5)
# lwd --> changes line width. 


## v0.10: make plot box line thicker
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", cex.lab=1.25, 
las=1, pch=21, bg="gray", cex=2.5, lwd=1.5); box(lwd=2)
# changes width of box outline


## make a map with colors and shape	

## v1.1: start with an empty plot (type=n), with all of the fun formatting from the basic plot
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", 
cex.lab=1.25, las=1); box(lwd=2)
    # changes different 
# make a line for each of your unique groups (groups are for species-genotype combinations). to identify unique groups, call "map_matrix" in R. 



## v1.2: add points with colors and shapes (colors match Species_Genotype designations)
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)", 
cex.lab=1.25, las=1); box(lwd=2)
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="blue", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="blue", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="blue", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="blue", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="purple", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="green", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="orange", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="orange", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="red", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="red", cex=2.5, lwd=1.5)



#pch codes (google), controls point shape. bg = "color", image found on google of list. Makes it easier to look at. 
  # take pic of study sight, make color palette off of image, use those colors for figures. Journals have different rules for colors



## v1.3: let's use aesthetically pleasing, color blind friendly colors
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#648FFF", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#648FFF", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#648FFF", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#648FFF", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#785EF0", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#DC267F", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#FE6100", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#FE6100", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#FFB000", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#FFB000", cex=2.5, lwd=1.5)

# 



## v1.4: that last palette wasn't that great, try again
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)


## v1.5: let's add a gray background since we have a really light color
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)


## v1.6: let's add a legend

# add legend in order. label1, shape1, "color1". Don't use complex, custom acronyms in legend. 
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("bottomleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)", 
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"))


## v1.7: make legend points larger
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("bottomleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)", 
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"), pt.cex=2)


## v1.8: make legend points lines thicker
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("bottomleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)", 
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"), pt.cex=2), pt.lwd=1.5)
# pt.lwd = 1 --> baseline. =2 --> twice as large as baseline, etc


## v1.9: make legend box line thicker
quartz(height=6, width=6)
par(mar=c(5,5,1,1))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("bottomleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)", 
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"), pt.cex=2), pt.lwd=1.5, box.lwd=1.5)
# box.lwd --> makes box lines bigger

## add pca next to map

## v2.1: use par(mfrow) to add another plot panel
  ## add another pane;
quartz(height=6, width=12)#  bigger area, new plot
par(mar=c(5,5,1,1), mfrow=c(1,2))#  new plot
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
# points(snake_data[snake_data[,5]=="group1",6]). Keep groups close to each other the same color, reuse the legend code
# plot(snake_data[,6],snake_data[,7],type="n",xlab...ylab...cex.lab, las); bo(lwd=2)). 

points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("topleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)", 
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"), 
pt.cex=2), pt.lwd=1.5, box.lwd=1.5)


## v2.2: start with an empty pca plot
quartz(height=6, width=12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("topleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)",
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"), pt.cex=2)
, pt.lwd=1.5, box.lwd=1.5)

plot(snake_data[,6], snake_data[,7], type="n", xlab="Principal component 1", ylab="Principal component 2", cex.lab=1.25, las=1); box(lwd=2)

## v2.3: add pca points (shapes and colors match map)
quartz(height=6, width=12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("topleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)", 
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"), 
pt.cex=2, pt.lwd=1.5, box.lwd=1.5)

plot(snake_data[,6], snake_data[,7], type="n", xlab="Principal component 1", ylab="Principal component 2", cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(snake_data[snake_data[,5]=="atratus_EPN",6], snake_data[snake_data[,5]=="atratus_EPN",7], pch=24, bg="#2c7fb8", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="atratus_P",6], snake_data[snake_data[,5]=="atratus_P",7], pch=24, bg="#253494", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="atratus_WT",6], snake_data[snake_data[,5]=="atratus_WT",7], pch=24, bg="black", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="couchii_T",6], snake_data[snake_data[,5]=="couchii_T",7], pch=22, bg="#a1dab4", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="elegans_WT",6], snake_data[snake_data[,5]=="elegans_WT",7], pch=21, bg="white", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="hammondii_WT",6], snake_data[snake_data[,5]=="hammondii_WT",7], pch=3, bg="brown", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_+/LVNV",6], snake_data[snake_data[,5]=="sirtalis_+/LVNV",7], pch=21, bg="#775511", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_LVNV",6], snake_data[snake_data[,5]=="sirtalis_LVNV",7], pch=25, bg="#CB0505", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_V",6], snake_data[snake_data[,5]=="sirtalis_V",7], pch=25, bg="#ffffcc", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_V/VA",6], snake_data[snake_data[,5]=="sirtalis_V/VA",7], pch=25, bg="#ba7570", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_VA",6], snake_data[snake_data[,5]=="sirtalis_VA",7], pch=25, bg="#41b6c4", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_WT",6], snake_data[snake_data[,5]=="sirtalis_WT",7], pch=25, bg="black", cex=1.5, lwd=1.25)

## v2.4: swap pcs 1 and 2 (in label only)
quartz(height=6, width=12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("topleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)",
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"),
pt.cex=2, pt.lwd=1.5, box.lwd=1.5)

plot(snake_data[,6], snake_data[,7], type="n", xlab="Principal component 2", ylab="Principal component 1", cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(snake_data[snake_data[,5]=="atratus_EPN",6], snake_data[snake_data[,5]=="atratus_EPN",7], pch=24, bg="#2c7fb8", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="atratus_P",6], snake_data[snake_data[,5]=="atratus_P",7], pch=24, bg="#253494", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="atratus_WT",6], snake_data[snake_data[,5]=="atratus_WT",7], pch=24, bg="black", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="couchii_T",6], snake_data[snake_data[,5]=="couchii_T",7], pch=22, bg="#a1dab4", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="elegans_WT",6], snake_data[snake_data[,5]=="elegans_WT",7], pch=21, bg="white", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="hammondii_WT",6], snake_data[snake_data[,5]=="hammondii_WT",7], pch=3, bg="brown", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_+/LVNV",6], snake_data[snake_data[,5]=="sirtalis_+/LVNV",7], pch=21, bg="#775511", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_LVNV",6], snake_data[snake_data[,5]=="sirtalis_LVNV",7], pch=25, bg="#CB0505", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_V",6], snake_data[snake_data[,5]=="sirtalis_V",7], pch=25, bg="#ffffcc", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_V/VA",6], snake_data[snake_data[,5]=="sirtalis_V/VA",7], pch=25, bg="#ba7570", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_VA",6], snake_data[snake_data[,5]=="sirtalis_VA",7], pch=25, bg="#41b6c4", cex=1.5, lwd=1.25)
points(snake_data[snake_data[,5]=="sirtalis_WT",6], snake_data[snake_data[,5]=="sirtalis_WT",7], pch=25, bg="black", cex=1.5, lwd=1.25)


## v2.5: flip the x and y axes on the pca (replace snake_data with -1*snake_data)
quartz(height=6, width=12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("topleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)",
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"),
pt.cex=1, pt.lwd=1.5, box.lwd=.5)

plot(-1*snake_data[,7], -1*snake_data[,6], type="n", xlab="Principal component 2", ylab="Principal component 1", cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(-1*snake_data[snake_data[,5]=="atratus_EPN",7], -1*snake_data[snake_data[,5]=="atratus_EPN",6], pch=24, bg="#2c7fb8", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="atratus_P",7], -1*snake_data[snake_data[,5]=="atratus_P",6], pch=24, bg="#253494", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="atratus_WT",7], -1*snake_data[snake_data[,5]=="atratus_WT",6], pch=24, bg="black", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="couchii_T",7], -1*snake_data[snake_data[,5]=="couchii_T",6], pch=22, bg="#a1dab4", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="elegans_WT",7], -1*snake_data[snake_data[,5]=="elegans_WT",6], pch=21, bg="white", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="hammondii_WT",7], -1*snake_data[snake_data[,5]=="hammondii_WT",6], pch=3, bg="brown", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_+/LVNV",7], -1*snake_data[snake_data[,5]=="sirtalis_+/LVNV",6], pch=21, bg="#775511", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_LVNV",7], -1*snake_data[snake_data[,5]=="sirtalis_LVNV",6], pch=25, bg="#CB0505", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_V",7], -1*snake_data[snake_data[,5]=="sirtalis_V",6], pch=25, bg="#ffffcc", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_V/VA",7], -1*snake_data[snake_data[,5]=="sirtalis_V/VA",6], pch=25, bg="#ba7570", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_VA",7], -1*snake_data[snake_data[,5]=="sirtalis_VA",6], pch=25, bg="#41b6c4", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_WT",7], -1*snake_data[snake_data[,5]=="sirtalis_WT",6], pch=25, bg="black", cex=1.5, lwd=1.25)


## fireworks plot awesomeness
## v3.1: Maintain previous code for map and add firework plot
  # takes one group of points, generates a mean, plots a line from the mean to each point. 
quartz(height=6, width=12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-117.1, -124.9), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#2c7fb8", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#253494", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="black", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#a1dab4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="white", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=3, bg="brown", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#ffffcc", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#41b6c4", cex=2.5, lwd=1.5)
legend("topleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)",
"T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), pch=c(21,22,24,24,24,3,25,25), pt.bg=c("white", "#a1dab4", "black", "#253494", "#2c7fb8", "brown", "#ffffcc", "#41b6c4"),
pt.cex=2, pt.lwd=1.5, box.lwd=1.5)


## control the colors, control the shapes
firework_colors <- c("#2c7fb8", "#253494", "black", "#a1dab4", "white", "brown", "#775511", "#CB0505", "#ffffcc", "#ba7570", "#41b6c4", "black")
firework_pchs <- c(24,24,24,22,21,3,25,25,25,25,25,25)
# plot data
plot(-1*snake_data[,7], -1*snake_data[,6], type="n", xlab="Principal component 2", ylab="Principal component 1", cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="light gray")
mean_long_vect <- vector()
mean_lat_vect <- vector()
for (i in 1:length(unique_species_genotypes))
	{
	sub_pca <- subset(snake_data, snake_data[,5]==unique_species_genotypes[i])
	mean_long_vect[i] <- -1 * mean(as.numeric(sub_pca[,7]))
	mean_lat_vect[i] <- -1 * mean(as.numeric(sub_pca[,6]))
	for (j in 1:dim(sub_pca)[1])
		{
		segments(mean_long_vect[i], mean_lat_vect[i], -1*sub_pca[,7], -1*sub_pca[,6], lwd=2, col=firework_colors[i])
		}
	}
for (i in 1: length(unique_species_genotypes))
	{
	points(mean_long_vect[i], mean_lat_vect[i], pch=firework_pchs[i], bg=firework_colors[i], cex=2.5, lwd=1.5)
	}

# plot for loops. 

## add pictures overlay to figures. (map, or images of organism with labels/legend with names)
# 


# quartz, par, plot (call plot, add labels), rect (color, background), legend, 
  # point code (each line): 






##### my homework code #####

quartz(height=6, width=12)
par(mar=c(5,5,1,1), mfrow=c(1,2))
plot(as.numeric(map_matrix[,4]), as.numeric(map_matrix[,3]), type="n", ylim=c(32.5, 46.6), xlim=c(-124.9, -117.1), xlab="Longitude (degrees)", ylab="Latitude (degrees)",
     cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#EDEDED")
points(as.numeric(map_matrix[1,4]), as.numeric(map_matrix[1,3]), pch=24, bg="#F50ECE", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[2,4]), as.numeric(map_matrix[2,3]), pch=24, bg="#F50ECE", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[3,4]), as.numeric(map_matrix[3,3]), pch=24, bg="#F50ECE", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[4,4]), as.numeric(map_matrix[4,3]), pch=24, bg="#F50ECE", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[5,4]), as.numeric(map_matrix[5,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[6,4]), as.numeric(map_matrix[6,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[7,4]), as.numeric(map_matrix[7,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[8,4]), as.numeric(map_matrix[8,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[9,4]), as.numeric(map_matrix[9,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[10,4]), as.numeric(map_matrix[10,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[11,4]), as.numeric(map_matrix[11,3]), pch=24, bg="#E41717", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[12,4]), as.numeric(map_matrix[12,3]), pch=24, bg="#FB8619", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[13,4]), as.numeric(map_matrix[13,3]), pch=24, bg="#FB8619", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[14,4]), as.numeric(map_matrix[14,3]), pch=24, bg="#FB8619", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[15,4]), as.numeric(map_matrix[15,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[16,4]), as.numeric(map_matrix[16,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[17,4]), as.numeric(map_matrix[17,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[18,4]), as.numeric(map_matrix[18,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[19,4]), as.numeric(map_matrix[19,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[20,4]), as.numeric(map_matrix[20,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[21,4]), as.numeric(map_matrix[21,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[22,4]), as.numeric(map_matrix[22,3]), pch=22, bg="#F5F10E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[23,4]), as.numeric(map_matrix[23,3]), pch=21, bg="#5FF50E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[24,4]), as.numeric(map_matrix[24,3]), pch=21, bg="#5FF50E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[25,4]), as.numeric(map_matrix[25,3]), pch=21, bg="#5FF50E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[26,4]), as.numeric(map_matrix[26,3]), pch=21, bg="#5FF50E", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[27,4]), as.numeric(map_matrix[27,3]), pch=23, bg="#0EA6F5", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[28,4]), as.numeric(map_matrix[28,3]), pch=25, bg="#0E25F5", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[29,4]), as.numeric(map_matrix[29,3]), pch=25, bg="#0E25F5", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[30,4]), as.numeric(map_matrix[30,3]), pch=25, bg="#A90EF5", cex=1.5, lwd=1.5)
points(as.numeric(map_matrix[31,4]), as.numeric(map_matrix[31,3]), pch=25, bg="#A90EF5", cex=1.5, lwd=1.5)
legend("bottomleft", legend=c("T. elegans (TTXs)", "T. couchii (TTXr.m)", "T. atratus (TTXs)", "T. atratus (TTXr.m)", "T. atratus (TTXr.x)", "T. hammondii (TTXs)",
                           "T. sirtalis (TTXr.m)", "T.sirtalis (TTXr.x)"), cex=.80, pch=c(21,22,24,24,24,23,25,25), pt.bg=c("green", "yellow", "orange", "red", "#F50ECE", "brown", "#0E25F5", "purple"),
       pt.cex=1, pt.lwd=1.5, box.lwd=.5)

plot(-1*snake_data[,7], -1*snake_data[,6], type="n", xlab="Principal component 2", ylab="Principal component 1", cex.lab=1.25, las=1); box(lwd=2)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="#EDEDED")
points(-1*snake_data[snake_data[,5]=="atratus_EPN",7], -1*snake_data[snake_data[,5]=="atratus_EPN",6], pch=24, bg="#2c7fb8", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="atratus_P",7], -1*snake_data[snake_data[,5]=="atratus_P",6], pch=24, bg="#253494", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="atratus_WT",7], -1*snake_data[snake_data[,5]=="atratus_WT",6], pch=24, bg="black", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="couchii_T",7], -1*snake_data[snake_data[,5]=="couchii_T",6], pch=22, bg="#a1dab4", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="elegans_WT",7], -1*snake_data[snake_data[,5]=="elegans_WT",6], pch=21, bg="white", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="hammondii_WT",7], -1*snake_data[snake_data[,5]=="hammondii_WT",6], pch=23, bg="brown", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_+/LVNV",7], -1*snake_data[snake_data[,5]=="sirtalis_+/LVNV",6], pch=21, bg="#775511", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_LVNV",7], -1*snake_data[snake_data[,5]=="sirtalis_LVNV",6], pch=25, bg="#CB0505", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_V",7], -1*snake_data[snake_data[,5]=="sirtalis_V",6], pch=25, bg="#ffffcc", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_V/VA",7], -1*snake_data[snake_data[,5]=="sirtalis_V/VA",6], pch=25, bg="#ba7570", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_VA",7], -1*snake_data[snake_data[,5]=="sirtalis_VA",6], pch=25, bg="#41b6c4", cex=1.5, lwd=1.25)
points(-1*snake_data[snake_data[,5]=="sirtalis_WT",7], -1*snake_data[snake_data[,5]=="sirtalis_WT",6], pch=25, bg="black", cex=1.5, lwd=1.25)

legend("bottomleft", legend=c("atratus_EPN", "atratus_P", "atratus_WT", "couchii_T", "elegans_WT", "hammondii_WT", "sirtalis_+/LVNV", "sirtalis_LVNV",
                              "sirtalis_V", "sirtalis_V/VA", "sirtalis_VA", "sirtalis_WT"), cex=.80, pch=c(24,24,24,22,21,23,21,25,25,25,25,25), pt.bg=c("#2c7fb8", "#253494", "black", 
                                                                                                                                                         "#a1dab4", "white", "brown", "#775511", "#CB0505", "#ffffcc", 
                                                                                                                                                         "#ba7570", "#41b6c4", "black"),
       pt.cex=1, pt.lwd=1.5, box.lwd=.5)



