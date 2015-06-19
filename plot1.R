# Have total emissions from PM2.5 decreased 
# in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot 
# showing the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.

# download the data -------------------------------------------------------

data.download <- function(data.dir=char(0)) {
      if (!file.exists(data.dir))
            dir.create(data.dir)
      url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
      destfile <- file.path(data.dir, "NEIdata.zip")
      if (!file.exists(destfile)) {
            message("Downloading the data...")
            download.file(url, destfile, mode="wb")
            timestamp.file <- file.path(data.dir,"timestamp_NEIdata")
            if (!file.exists(timestamp.file))
                  file.create(timestamp.file)
            cat(paste("Downloaded: ", date(), collapse=" "),
                file=timestamp.file)
      }
      unzip(zipfile=destfile,exdir = data.dir)
}


# read the data -----------------------------------------------------------

data.read <- function(data.dir=file.path(".","data")) {
      nei.file <- file.path(data.dir, "summarySCC_PM25.rds")
      if (file.exists(nei.file))
            data.download(data.dir)
      message(paste("Reading ", nei.file, "...", collapse=""))
      nei <<- readRDS(nei.file)
}


# prepare the data for plotting -------------------------------------------
data.read()
## in order to plot total emissions, group
## the 'nei' dataset by 'year' and summarize the
## 'Emissions'
library(dplyr)
nei.for.plot <- nei %>% 
      group_by(year) %>%
      summarize(total_Emissions=sum(Emissions))
nei.for.plot$year <- as.numeric(nei.for.plot$year)

# plot the data -----------------------------------------------------------

png(filename="./plot1.png")
## do not draw axes, draw dots and lines
plot(x=nei.for.plot$year, 
     y=nei.for.plot$total_Emissions,
     main="Total Emissions of PM2.5 in the USA",
     xlab="Years", ylab="Total Emissions of PM2.5 (tons)",
     type='o',pch=19, axes=FALSE, frame=TRUE)
x <- nei.for.plot$year
y <- nei.for.plot$total_Emissions
## label values near the dots
sub <- c(F,T,F,T)
text(x=subset(x,sub),
     y=subset(y,sub),
     labels = subset(format(y,nsmall=0,big.mark=' '),sub),
     adj=c(1.1,0.7), cex=.75)
sub <- !sub
text(x=subset(x,sub),
     y=subset(y,sub),
     labels = subset(format(y,nsmall=0, big.mark=' '),sub),
     adj=c(-0.2,0.1), cex=.75)
par(cex.axis=0.8)
## draw the x axis
axis(1,at=x)
## draw the y axis
y_at = seq(from = floor(min(y)/10^6)*10^6, 
           to = ceiling(max(y)/10^6)*10^6, by=10^6)
axis(2, at=y_at,
     labels=c(format(y_at,scientific=FALSE,big.mark=' ')))
dev.off()

