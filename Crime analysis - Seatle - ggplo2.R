############################################################################
# Preliminary
library(ggplot2)
rm(list=ls(all=TRUE))
############################################################################
# 1. Load data
# Seattle data
Seattle=read.table('Seattle.csv', header=T, sep=',')

# Cases and variables
dim(Seattle)

# Variables' characteristics
str(Seattle)

# Correct the data type
Seattle$RMS.CDW.ID <- as.character(Seattle$RMS.CDW.ID)
Seattle$General.Offense.Number <- as.factor(Seattle$General.Offense.Number)
Seattle$Offense.Code.Extension <- as.factor(Seattle$Offense.Code.Extension)
Seattle$Census.Tract.2000 <- as.factor(Seattle$Census.Tract.2000)

Seattle$Date.Reported <- strptime(Seattle$Date.Reported,"%m/%d/%Y %I:%M:%S %p") # "%m/%d/%Y %H:%M:%S" if there is no AM/PM
Seattle$Occurred.Date.or.Date.Range.Start <- strptime(Seattle$Occurred.Date.or.Date.Range.Start, "%m/%d/%Y %I:%M:%S %p")
Seattle$Occurred.Date.Range.End <- strptime(Seattle$Occurred.Date.Range.End, "%m/%d/%Y %I:%M:%S %p") 
############################################################################
# 1) How do incidents vary by time of day?

# Separate days from hours
Hours <- format(Seattle$Occurred.Date.or.Date.Range.Start,format = "%H")
Minutes <- format(Seattle$Occurred.Date.or.Date.Range.Start,format = "%M")
Dates <- format(Seattle$Occurred.Date.or.Date.Range.Start,format = "%m/%d/%Y")

Seattle$Occurred.Hours <- Hours
Seattle$Occurred.Minutes <- Minutes
Seattle$Occurred.Dates <- Dates

# Convert Seattle$Occurred.Hours and Seattle$Occurred.Minutes into numeric variables; combine them
Seattle$Occurred.Hours <- as.numeric(Seattle$Occurred.Hours)
Seattle$Occurred.Minutes <- as.numeric(Seattle$Occurred.Minutes)
Seattle$Occurred.Time <- Seattle$Occurred.Hours + Seattle$Occurred.Minutes*(1/60)

# Histogram --> GGPLOT
# Seattle is a data frame --> ok

    # Basic histogram
Hours_Hist <- hist(Seattle$Occurred.Time, breaks=24, cex.axis=0.7, freq=T, right=F,col="coral", border="coral3", main="Incidents by time of day", xlab = "Time of day", ylab="No. of incidents", xlim=c(0,24), xaxt='n')
axis(side=1,at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), cex.axis=0.7, labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24), tick=T, line=1)
m <- max(Hours_Hist$counts)
abline(h=m, col="darkblue") # Horizontal line for the max frequency
Hours_Hist# Details about how did R built the histogram

    # ggplo2 histogram with manual color coding --> this was not used in the assignment
colorgg_1 = rep("olivedrab2", each= 7)
colorgg_2 = rep("gold", each= 4)
colorgg_3 = rep("darkorange", each= 9)
colorgg = c("orangered", colorgg_1, colorgg_2, "orangered", "darkorange", "gold", colorgg_3)

linegg_1 = rep("olivedrab", each= 7)
linegg_2 = rep("goldenrod4", each= 4)
linegg_3 = rep("darkorange4", each= 9)
linegg = c("orangered4", linegg_1,linegg_2, "orangered4", "darkorange4","goldenrod4", linegg_3)

ggplot(Seattle, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', fill=colorgg, colour=linegg) + labs(title="Incidents by time of day",x="Time of day", y = "No. of incidents")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = m) + geom_hline(yintercept = 2000, color="azure4") + geom_hline(yintercept = 1500, color="azure4") + geom_hline(yintercept = 1000, color="azure4")


    # ggplo2 histogram with automatic color coding (gradient)
ggplot(Seattle, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=..count..), colour=linegg) + scale_fill_gradient("Count", low = "green", high = "red") + labs(title="Incidents by time of day",x="Time of day", y = "No. of incidents")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = m) + geom_hline(yintercept = 2000, color="azure4") + geom_hline(yintercept = 1500, color="azure4") + geom_hline(yintercept = 1000, color="azure4")


    # ggplo2 histogram with automatic color coding (hue)
colours <- c("#92D900", "#FFF56A", "#FF8D15", "#EB0A0A","#EB0A0A") # Choose the colors using http://colorblender.com
ggplot(Seattle, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=cut(..count.., c(0,1000,1500,2000, Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + labs(title="Incidents by time of day",x="Time of day", y = "No. of incidents")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = m) + geom_hline(yintercept = 2000, color="azure4") + geom_hline(yintercept = 1500, color="azure4") + geom_hline(yintercept = 1000, color="azure4")

 
############################################################################
# 2) Which incidents are most common in the evening?
# Evening definition: 6pm to 8pm (18 <= Occured.Time < 20)
# Select data only for evening

Seattle.Evening <- Seattle[which(Seattle$Occurred.Time>= 18 & Seattle$Occurred.Time < 20),]

    # Bar plot basic
library(MASS)
Inc.Evening <- Seattle.Evening$Summarized.Offense.Description
Freq.Evening <- table(Inc.Evening)
Freq.Evening <- Freq.Evening[order(Freq.Evening)] # Order the table per frequency
colores_grey = rep("azure", each= 42) # create a vector with 42 times the word azure
colores_high = rep("coral1", each=6)
colores=c(colores_grey, colores_high)
Evening.Plot <- barplot(Freq.Evening,horiz=T, main="Incidents in the evening", col=colores,xlab="Frequency of incidents in the evening", cex.axis=0.7, cex.names = 0.7, yaxt='n')

Evening_Perc <- Freq.Evening/sum(Freq.Evening)
Evening_Common <- as.data.frame(Evening_Perc)
Evening_Common <- subset(Evening_Common, Freq>0.05)

barplot(Evening_Common$Freq,names.arg=Evening_Common$Inc.Evening, horiz=T, main="Most common incidents in the evening", col= "coral1", xlab="Percentage of total incidents in the evening", xlim = c(0,.25), cex.axis=0.7, cex.names = 0.7, las=1) # las is for style of access labels: 0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical

# Barplot --> GGPLOT2
# Seattle.Evening is a data.frame --> ok

    # Order the bars by frequency:
Seattle.Evening$Summarized.Offense.Description<- factor(Seattle.Evening$Summarized.Offense.Descriptio, levels=rev(names(sort(table(Seattle.Evening$Summarized.Offense.Descriptio), decreasing = TRUE))))
print(summary(Seattle.Evening))

    # A barplot of frequencies or counts is straightforward:
ggplot(data=Seattle.Evening, aes(x=Summarized.Offense.Description)) + geom_bar(fill="coral1") + coord_flip() + labs(title="Incidents in the evening",y="Frequency of incidents in the evening", x = "Incidents")


############################################################################
# 3) During what periods of the day are robberies most common?
# Select data only for robberies

Seattle.Robbery <- Seattle[which(Seattle$Summarized.Offense.Description=='ROBBERY'),]

#Histogram --> Basic
Hours_Robbery.Hist <- hist(Seattle.Robbery$Occurred.Time, breaks=24, cex.axis=0.7, freq=T, right=F,col="coral", border="coral3", main="Robberies by time of day", xlab = "Time of day", ylab="No. of robberies", xlim=c(0,24), xaxt='n')
axis(side=1,at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), cex.axis=0.7, labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24), tick=T, line=1)
mrob <- max(Hours_Robbery.Hist$counts)
abline(h=mrob, col="darkblue") # Horizontal line for the max frequency

Hours_Robbery.Hist # Details about how did R built the histogram

# ggplo2 histogram with automatic color coding (hue)
# Seattle.Robbery is a data.frame --> ok

colours <- c("#92D900", "#FFF56A", "#FF8D15", "#EB0A0A","#EB0A0A") # Choose the colors using http://colorblender.com
ggplot(Seattle.Robbery, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=cut(..count.., c(0,10,30,50,Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + labs(title="Robberies by time of day",x="Time of day", y = "No. of robberies")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = mrob) + geom_hline(yintercept = 50, color="azure4") + geom_hline(yintercept = 30, color="azure4") + geom_hline(yintercept = 10, color="azure4")

############################################################################
# Add. 1: Which are the most common incidents

# Barplot --> GGPLOT2
# Seattle.Evening is a data.frame --> ok

# Order the bars by frequency:
Seattle$Summarized.Offense.Description<- factor(Seattle$Summarized.Offense.Descriptio, levels=rev(names(sort(table(Seattle$Summarized.Offense.Descriptio), decreasing = TRUE))))
print(summary(Seattle))

# A barplot of frequencies or counts is straightforward:
ggplot(data=Seattle, aes(x=Summarized.Offense.Description)) + geom_bar(fill="coral1") + coord_flip() + labs(title="Most frequent incidents",y="Frequency of incidents", x = "Incidents")





############################################################################
# Add. 2: During what periods of the day are car prowls most common?
# Select data only for car prowls

Seattle.Car_Prowl <- Seattle[which(Seattle$Summarized.Offense.Description=='CAR PROWL'),]

#Histogram --> Basic
Hours_Car_Prowl.Hist <- hist(Seattle.Car_Prowl$Occurred.Time, breaks=24, cex.axis=0.7, freq=T, right=F,col="coral", border="coral3", main="Car prowls by time of day", xlab = "Time of day", ylab="No. of car prowls", xlim=c(0,24), xaxt='n')
axis(side=1,at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), cex.axis=0.7, labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24), tick=T, line=1)
mcarp <- max(Hours_Car_Prowl.Hist$counts)
abline(h=mcarp, col="darkblue") # Horizontal line for the max frequency

Hours_Car_Prowl.Hist # Details about how did R built the histogram

# ggplo2 histogram with automatic color coding (hue)
# Seattle.Robbery is a data.frame --> ok

colours <- c("#92D900", "#FFF56A", "#FF8D15", "#EB0A0A","#EB0A0A") # Choose the colors using http://colorblender.com
ggplot(Seattle.Car_Prowl, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=cut(..count.., c(0,100,200,400,Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + labs(title="Car prowls by time of day",x="Time of day", y = "No. of car prowls")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = mcarp) + geom_hline(yintercept = 100, color="azure4") + geom_hline(yintercept = 200, color="azure4") + geom_hline(yintercept = 400, color="azure4")

############################################################################
# Add. 3: In which neighborhoods are car prowls most common?
# Select data only for car prowls

Seattle.Car_Prowl <- Seattle[which(Seattle$Summarized.Offense.Description=='CAR PROWL'),]

## District_sector
# Bar plot basic
library(MASS)
Car_Prowl.Neigh <- Seattle.Car_Prowl$District.Sector
Freq.Neigh <- table(Car_Prowl.Neigh)
Freq.Neigh <- Freq.Neigh[order(Freq.Neigh)] # Order the table per frequency
colores_grey = rep("azure", each= 49) # create a vector with 42 times the word azure
colores_high = rep("coral1", each=4)
colores=c(colores_grey, colores_high)
Neigh.Plot <- barplot(Freq.Neigh,horiz=T, main="Car prowls per neighborhood", col=colores,xlab="Frequency of car prowls per neighborhood", cex.axis=0.7, cex.names = 0.7, yaxt='n')

Neigh_Perc <- Freq.Neigh/sum(Freq.Neigh)
Neigh_Common <- as.data.frame(Neigh_Perc)
Neigh_Common <- subset(Neigh_Common, Freq>0.04)

barplot(Neigh_Common$Freq,names.arg=Neigh_Common$Car_Prowl.Neigh, horiz=T, main="Car prowls per neighborhood", col= "coral1", xlab="Percentage of total car prowls per neighborhood", xlim = c(0,0.175), cex.axis=0.7, cex.names = 0.7, las=1) # las is for style of access labels: 0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical

# Barplot --> GGPLOT2
# Seattle.Car_Prowl is a data.frame --> ok

# Order the bars by frequency:
Seattle.Car_Prowl$District.Sector<- factor(Seattle.Car_Prowl$District.Sector, levels=rev(names(sort(table(Seattle.Car_Prowl$District.Sector), decreasing = TRUE))))
print(summary(Seattle.Car_Prowl))

mdis <- max(Freq.Neigh)

# A barplot of frequencies or counts is straightforward:
ggplot(data=Seattle.Car_Prowl, aes(x=District.Sector)) + geom_bar(aes(fill=cut(..count.., c(0,200,350,500,Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + coord_flip() + labs(title="Car prowls per neighborhood",y="Frequency of car prowls per neighborhood", x = "Neighborhoods") + geom_hline(yintercept = 200, color="azure4") + geom_hline(yintercept = 350, color="azure4") + geom_hline(yintercept = 500, color="azure4") + geom_hline(yintercept = mdis, color="azure4") + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + scale_y_continuous(breaks=c(200, 350, 500, mdis), labels= c("200","350","500", mdis))











############################################################################
############################################################################
############################################################################
############################################################################

### COMPLETE TEXT INCL. GRAPHS NOT USED IN THE ASSIGNMENT

############################################################################
# Preliminary
library(ggplot2)
rm(list=ls(all=TRUE))
############################################################################
# 1. Load data
# Seattle data
Seattle=read.table('Seattle.csv', header=T, sep=',')

# Cases and variables
dim(Seattle)

# Variables' characteristics
str(Seattle)

# Correct the data type
Seattle$RMS.CDW.ID <- as.character(Seattle$RMS.CDW.ID)
Seattle$General.Offense.Number <- as.factor(Seattle$General.Offense.Number)
Seattle$Offense.Code.Extension <- as.factor(Seattle$Offense.Code.Extension)
Seattle$Census.Tract.2000 <- as.factor(Seattle$Census.Tract.2000)

Seattle$Date.Reported <- strptime(Seattle$Date.Reported,"%m/%d/%Y %I:%M:%S %p") # "%m/%d/%Y %H:%M:%S" if there is no AM/PM
Seattle$Occurred.Date.or.Date.Range.Start <- strptime(Seattle$Occurred.Date.or.Date.Range.Start, "%m/%d/%Y %I:%M:%S %p")
Seattle$Occurred.Date.Range.End <- strptime(Seattle$Occurred.Date.Range.End, "%m/%d/%Y %I:%M:%S %p") 
############################################################################
# 1) How do incidents vary by time of day?

# Separate days from hours
Hours <- format(Seattle$Occurred.Date.or.Date.Range.Start,format = "%H")
Minutes <- format(Seattle$Occurred.Date.or.Date.Range.Start,format = "%M")
Dates <- format(Seattle$Occurred.Date.or.Date.Range.Start,format = "%m/%d/%Y")

Seattle$Occurred.Hours <- Hours
Seattle$Occurred.Minutes <- Minutes
Seattle$Occurred.Dates <- Dates

# Convert Seattle$Occurred.Hours and Seattle$Occurred.Minutes into numeric variables; combine them
Seattle$Occurred.Hours <- as.numeric(Seattle$Occurred.Hours)
Seattle$Occurred.Minutes <- as.numeric(Seattle$Occurred.Minutes)
Seattle$Occurred.Time <- Seattle$Occurred.Hours + Seattle$Occurred.Minutes*(1/60)

# Histogram --> GGPLOT
# Seattle is a data frame --> ok

# Basic histogram --> this was not used in the assignment
Hours_Hist <- hist(Seattle$Occurred.Time, breaks=24, cex.axis=0.7, freq=T, right=F,col="coral", border="coral3", main="Incidents by time of day", xlab = "Time of day", ylab="No. of incidents", xlim=c(0,24), xaxt='n')
axis(side=1,at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), cex.axis=0.7, labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24), tick=T, line=1)
m <- max(Hours_Hist$counts)
abline(h=m, col="darkblue") # Horizontal line for the max frequency
Hours_Hist# Details about how did R built the histogram

# ggplo2 histogram with manual color coding --> this was not used in the assignment
colorgg_1 = rep("olivedrab2", each= 7)
colorgg_2 = rep("gold", each= 4)
colorgg_3 = rep("darkorange", each= 9)
colorgg = c("orangered", colorgg_1, colorgg_2, "orangered", "darkorange", "gold", colorgg_3)

linegg_1 = rep("olivedrab", each= 7)
linegg_2 = rep("goldenrod4", each= 4)
linegg_3 = rep("darkorange4", each= 9)
linegg = c("orangered4", linegg_1,linegg_2, "orangered4", "darkorange4","goldenrod4", linegg_3)

ggplot(Seattle, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', fill=colorgg, colour=linegg) + labs(title="Incidents by time of day",x="Time of day", y = "No. of incidents")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = m) + geom_hline(yintercept = 2000, color="azure4") + geom_hline(yintercept = 1500, color="azure4") + geom_hline(yintercept = 1000, color="azure4")


# ggplo2 histogram with automatic color coding (gradient)
ggplot(Seattle, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=..count..), colour=linegg) + scale_fill_gradient("Count", low = "green", high = "red") + labs(title="Incidents by time of day",x="Time of day", y = "No. of incidents")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = m) + geom_hline(yintercept = 2000, color="azure4") + geom_hline(yintercept = 1500, color="azure4") + geom_hline(yintercept = 1000, color="azure4")


# ggplo2 histogram with automatic color coding (hue)
colours <- c("#92D900", "#FFF56A", "#FF8D15", "#EB0A0A","#EB0A0A") # Choose the colors using http://colorblender.com
ggplot(Seattle, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=cut(..count.., c(0,1000,1500,2000, Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + labs(title="Incidents by time of day",x="Time of day", y = "No. of incidents")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = m) + geom_hline(yintercept = 2000, color="azure4") + geom_hline(yintercept = 1500, color="azure4") + geom_hline(yintercept = 1000, color="azure4")


############################################################################
# 2) Which incidents are most common in the evening?
# Evening definition: 6pm to 8pm (18 <= Occured.Time < 20)
# Select data only for evening

Seattle.Evening <- Seattle[which(Seattle$Occurred.Time>= 18 & Seattle$Occurred.Time < 20),]

# Bar plot basic
library(MASS)
Inc.Evening <- Seattle.Evening$Summarized.Offense.Description
Freq.Evening <- table(Inc.Evening)
Freq.Evening <- Freq.Evening[order(Freq.Evening)] # Order the table per frequency
colores_grey = rep("azure", each= 42) # create a vector with 42 times the word azure
colores_high = rep("coral1", each=6)
colores=c(colores_grey, colores_high)
Evening.Plot <- barplot(Freq.Evening,horiz=T, main="Incidents in the evening", col=colores,xlab="Frequency of incidents in the evening", cex.axis=0.7, cex.names = 0.7, yaxt='n')

Evening_Perc <- Freq.Evening/sum(Freq.Evening)
Evening_Common <- as.data.frame(Evening_Perc)
Evening_Common <- subset(Evening_Common, Freq>0.05)

barplot(Evening_Common$Freq,names.arg=Evening_Common$Inc.Evening, horiz=T, main="Most common incidents in the evening", col= "coral1", xlab="Percentage of total incidents in the evening", xlim = c(0,.25), cex.axis=0.7, cex.names = 0.7, las=1) # las is for style of access labels: 0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical

# Barplot --> GGPLOT2
# Seattle.Evening is a data.frame --> ok

# Order the bars by frequency:
Seattle.Evening$Summarized.Offense.Description<- factor(Seattle.Evening$Summarized.Offense.Descriptio, levels=rev(names(sort(table(Seattle.Evening$Summarized.Offense.Descriptio), decreasing = TRUE))))
print(summary(Seattle.Evening))

# A barplot of frequencies or counts is straightforward:
ggplot(data=Seattle.Evening, aes(x=Summarized.Offense.Description)) + geom_bar(fill="coral1") + coord_flip() + labs(title="Incidents in the evening",y="Frequency of incidents in the evening", x = "Incidents")


############################################################################
# 3) During what periods of the day are robberies most common?
# Select data only for robberies

Seattle.Robbery <- Seattle[which(Seattle$Summarized.Offense.Description=='ROBBERY'),]

#Histogram --> Basic
Hours_Robbery.Hist <- hist(Seattle.Robbery$Occurred.Time, breaks=24, cex.axis=0.7, freq=T, right=F,col="coral", border="coral3", main="Robberies by time of day", xlab = "Time of day", ylab="No. of robberies", xlim=c(0,24), xaxt='n')
axis(side=1,at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), cex.axis=0.7, labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24), tick=T, line=1)
mrob <- max(Hours_Robbery.Hist$counts)
abline(h=mrob, col="darkblue") # Horizontal line for the max frequency

Hours_Robbery.Hist # Details about how did R built the histogram

# ggplo2 histogram with automatic color coding (hue)
# Seattle.Robbery is a data.frame --> ok

colours <- c("#92D900", "#FFF56A", "#FF8D15", "#EB0A0A","#EB0A0A") # Choose the colors using http://colorblender.com
ggplot(Seattle.Robbery, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=cut(..count.., c(0,10,30,50,Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + labs(title="Robberies by time of day",x="Time of day", y = "No. of robberies")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = mrob) + geom_hline(yintercept = 50, color="azure4") + geom_hline(yintercept = 30, color="azure4") + geom_hline(yintercept = 10, color="azure4")

############################################################################
# Add. 1) During what periods of the day are car prowls most common?
# Select data only for car prowls

Seattle.Car_Prowl <- Seattle[which(Seattle$Summarized.Offense.Description=='CAR PROWL'),]

#Histogram --> Basic
Hours_Car_Prowl.Hist <- hist(Seattle.Car_Prowl$Occurred.Time, breaks=24, cex.axis=0.7, freq=T, right=F,col="coral", border="coral3", main="Car prowls by time of day", xlab = "Time of day", ylab="No. of car prowls", xlim=c(0,24), xaxt='n')
axis(side=1,at=c(0,2,4,6,8,10,12,14,16,18,20,22,24), cex.axis=0.7, labels=c(0,2,4,6,8,10,12,14,16,18,20,22,24), tick=T, line=1)
mcarp <- max(Hours_Car_Prowl.Hist$counts)
abline(h=mcarp, col="darkblue") # Horizontal line for the max frequency

Hours_Car_Prowl.Hist # Details about how did R built the histogram

# ggplo2 histogram with automatic color coding (hue)
# Seattle.Robbery is a data.frame --> ok

colours <- c("#92D900", "#FFF56A", "#FF8D15", "#EB0A0A","#EB0A0A") # Choose the colors using http://colorblender.com
ggplot(Seattle.Car_Prowl, aes(x = Occurred.Hours)) + geom_histogram(bins = 24, closed='left', aes(fill=cut(..count.., c(0,100,200,400,Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + labs(title="Car prowls by time of day",x="Time of day", y = "No. of car prowls")+ scale_x_continuous(breaks=seq(0, 23, 1)) +   theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + geom_hline(yintercept = mcarp) + geom_hline(yintercept = 100, color="azure4") + geom_hline(yintercept = 200, color="azure4") + geom_hline(yintercept = 400, color="azure4")

############################################################################
# Add. 2) In which neighborhoods are car prowls most common?
# Select data only for car prowls

Seattle.Car_Prowl <- Seattle[which(Seattle$Summarized.Offense.Description=='CAR PROWL'),]

## District_sector
# Bar plot basic
library(MASS)
Car_Prowl.Neigh <- Seattle.Car_Prowl$District.Sector
Freq.Neigh <- table(Car_Prowl.Neigh)
Freq.Neigh <- Freq.Neigh[order(Freq.Neigh)] # Order the table per frequency
colores_grey = rep("azure", each= 49) # create a vector with 42 times the word azure
colores_high = rep("coral1", each=4)
colores=c(colores_grey, colores_high)
Neigh.Plot <- barplot(Freq.Neigh,horiz=T, main="Car prowls per neighborhood", col=colores,xlab="Frequency of car prowls per neighborhood", cex.axis=0.7, cex.names = 0.7, yaxt='n')

Neigh_Perc <- Freq.Neigh/sum(Freq.Neigh)
Neigh_Common <- as.data.frame(Neigh_Perc)
Neigh_Common <- subset(Neigh_Common, Freq>0.04)

barplot(Neigh_Common$Freq,names.arg=Neigh_Common$Car_Prowl.Neigh, horiz=T, main="Car prowls per neighborhood", col= "coral1", xlab="Percentage of total car prowls per neighborhood", xlim = c(0,0.175), cex.axis=0.7, cex.names = 0.7, las=1) # las is for style of access labels: 0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical

# Barplot --> GGPLOT2
# Seattle.Car_Prowl is a data.frame --> ok

# Order the bars by frequency:
Seattle.Car_Prowl$District.Sector<- factor(Seattle.Car_Prowl$District.Sector, levels=rev(names(sort(table(Seattle.Car_Prowl$District.Sector), decreasing = TRUE))))
print(summary(Seattle.Car_Prowl))

mdis <- max(Freq.Neigh)

# A barplot of frequencies or counts is straightforward:
ggplot(data=Seattle.Car_Prowl, aes(x=District.Sector)) + geom_bar(aes(fill=cut(..count.., c(0,200,350,500,Inf))), colour="black", show.legend = FALSE) + scale_fill_manual(values=colours) + coord_flip() + labs(title="Car prowls per neighborhood",y="Frequency of car prowls per neighborhood", x = "Neighborhoods") + geom_hline(yintercept = 200, color="azure4") + geom_hline(yintercept = 350, color="azure4") + geom_hline(yintercept = 500, color="azure4") + geom_hline(yintercept = mdis, color="azure4") + theme(axis.line = element_line(colour = "black"), panel.grid.major = element_blank(), panel.border = element_blank(), panel.background = element_blank()) + scale_y_continuous(breaks=c(200, 350, 500, mdis), labels= c("200","350","500", mdis))











############################################################################
############################################################################
############################################################################
############################################################################

# Extra 1)

# Histogram with density plot
ggplot(Seattle, aes(x=Occurred.Hours)) + 
  geom_histogram(aes(y=..density..), bins = 24, colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")


############################################################################

# Extra car prowls per zone beat

## Zone.beat
# Bar plot basic
library(MASS)
Car_Prowl.Neigh <- Seattle.Car_Prowl$Zone.Beat
Freq.Neigh <- table(Car_Prowl.Neigh)
Freq.Neigh <- Freq.Neigh[order(Freq.Neigh)] # Order the table per frequency
colores_grey = rep("azure", each= 49) # create a vector with 42 times the word azure
colores_high = rep("coral1", each=4)
colores=c(colores_grey, colores_high)
Neigh.Plot <- barplot(Freq.Neigh,horiz=T, main="Car prowls per neighborhood", col=colores,xlab="Frequency of car prowls per neighborhood", cex.axis=0.7, cex.names = 0.7, yaxt='n')

Neigh_Perc <- Freq.Neigh/sum(Freq.Neigh)
Neigh_Common <- as.data.frame(Neigh_Perc)
Neigh_Common <- subset(Neigh_Common, Freq>0.04)

barplot(Neigh_Common$Freq,names.arg=Neigh_Common$Car_Prowl.Neigh, horiz=T, main="Car prowls per neighborhood", col= "coral1", xlab="Percentage of total car prowls per neighborhood", xlim = c(0,.075), cex.axis=0.7, cex.names = 0.7, las=1) # las is for style of access labels: 0=parallel, 1=all horizontal, 2=all perpendicular to axis, 3=all vertical

# Barplot --> GGPLOT2
# Seattle.Car_Prowl is a data.frame --> ok

# Order the bars by frequency:
Seattle.Car_Prowl$Zone.Beat<- factor(Seattle.Car_Prowl$Zone.Beat, levels=rev(names(sort(table(Seattle.Car_Prowl$Zone.Beat), decreasing = TRUE))))
print(summary(Seattle.Car_Prowl))

# A barplot of frequencies or counts is straightforward:
ggplot(data=Seattle.Car_Prowl, aes(x=Zone.Beat)) + geom_bar(fill="coral1") + coord_flip() + labs(title="Car prowls per neighborhood",y="Frequency of car prowls per neighborhood", x = "Neighborhoods")

