# Of the four types of sources indicated by the `type` 
# (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases 
# in emissions from 1999-2008 for Baltimore City? 
# Which have seen increases in emissions from 1999-2008? 
# Use the ggplot2 plotting system to make a plot answer 
# this question.

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
## the 'nei' dataset by 'year' and 'type',
## select only observations regarding Baltimore 
## and summarize the 'Emissions'
library(dplyr)
nei.for.plot <- nei %>%
      group_by(year, type) %>%
      filter(fips == "24510") %>%
      summarize(emissions=sum(Emissions))
nei.for.plot$year <- as.numeric(nei.for.plot$year)

# plotting ----------------------------------------------------------------

library(ggplot2)
g <- ggplot(aes(year, emissions), data=nei.for.plot)
g <- g + geom_point() + geom_line(lty=2)
g <- g + facet_wrap(~type, nrow=2)
years <- unique(nei.for.plot$year)
g <- g + scale_x_continuous(breaks=years)
g <- g + labs(title="PM2.5 Emissions for Baltimore City in 1999-2008")
g <- g + labs(x = "Years", y = "PM2.5 Emissions (tons)")
library(grid)
g <- g + theme(panel.margin = unit(10,"points"))
ggsave("./plot3.png")
