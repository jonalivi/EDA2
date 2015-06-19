# Have total emissions from PM2.5 decreased
# in the Baltimore City, Maryland (`fips == "24510"`) 
# from 1999 to 2008? Use the base plotting system 
# to make a plot answering this question.

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
      nei <- readRDS(nei.file)
}

# prepare the data for plotting -------------------------------------------
data.read()
## in order to plot total emissions, group
## the 'nei' dataset by 'year', select only
## observations regarding Baltimore City 
## and summarize the 'Emissions'
library(dplyr)
nei.for.plot <- nei %>%
      group_by(year) %>%
      filter(fips == "24510") %>%
      summarize(total_Emissions=sum(Emissions))
nei.for.plot$year <- as.numeric(nei.for.plot$year)

# plot the data -----------------------------------------------------------

png(filename="./plot2.png")
## do not draw axes, draw dots and lines
plot(x=nei.for.plot$year, 
     y=nei.for.plot$total_Emissions,
     main="Total Emissions of PM2.5 in Baltimore",
     xlab="Years", ylab="Total Emissions of PM2.5 (tons)",
     type='o', axes=FALSE, pch=19, frame=TRUE)
x <- nei.for.plot$year
y <- nei.for.plot$total_Emissions
## label values near the dots
sub <- c(F,T,F,T)
text(x=subset(x,sub),
     y=subset(y,sub),
     labels = subset(format(y,nsmall=0, digits=4),sub),
     adj=c(1.2,0.7), cex=.75)
sub <- !sub
text(x=subset(x,sub),
     y=subset(y,sub),
     labels = subset(format(y,nsmall=0, digits=4),sub),
     adj=c(-0.2,0.1), cex=.75)
par(cex.axis=0.8)
## draw the x axis
axis(1,at=x)
## draw the y axis
y_at = seq(from = floor(min(y)/500)*500, 
           to = ceiling(max(y)/500)*500, by=500)
axis(2, at=y_at, labels=y_at)
dev.off()