# Compare emissions from motor vehicle sources in Baltimore City
# with emissions from motor vehicle sources in Los Angeles County,
# California (fips == "06037"). Which city has seen greater changes
# over time in motor vehicle emissions?

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
      if (!file.exists(nei.file))
            data.download(data.dir)
      message(paste("Reading ", nei.file, "...", collapse=""))
      nei <<- readRDS(nei.file)
}

# prepare the data for plotting -------------------------------------------
data.read()
library(dplyr)
library(tidyr)
nei.for.plot <- nei %>%
      filter(fips == "24510"|fips == "06037", type=="ON-ROAD") %>%
      group_by(year, fips) %>%
      summarize(vehicleRelated=sum(Emissions))
nei.for.plot$fips <- factor(nei.for.plot$fips, 
      labels=c("Los Angeles County", "Baltimore City"))
nei.for.plot <- group_by(nei.for.plot, fips)

# plotting ----------------------------------------------------------------

library(ggplot2)
g <- ggplot(aes(year, vehicleRelated), 
            data=nei.for.plot)
g <- g + geom_point(aes(color=fips))
years <- unique(nei.for.plot$year)
g <- g + scale_x_continuous(breaks=years)
g <- g + scale_y_continuous(label=function(x) {
      lab <- format(x, digits=0)
})
g <- g + labs(
      title="Emissions of PM2.5\nfrom Motor Vehicle Sources\nin Baltimore City\nand Los Angeles County\nin 1999-2008")
g <- g + labs(x = "Years", y = "Emissions of PM2.5 (tons)")

g <- g + geom_hline(aes(yintercept=vehicleRelated[1]),color="red")
g <- g + geom_hline(aes(yintercept=vehicleRelated[2]),color="blue")
g <- g + geom_hline(aes(yintercept=vehicleRelated[7]),color="red")
g <- g + geom_hline(aes(yintercept=vehicleRelated[8]),color="blue")
g <- g + geom_text(aes(
      label=format(vehicleRelated, digits=1, scientific=FALSE)),
      size=2, vjust=-0.5, hjust=.5)
ggsave("./plot6.png", width=5, height=5, dpi=120)
