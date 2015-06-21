# How have emissions from motor vehicle sources 
# changed from 1999-2008 in Baltimore City?

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
nei.for.plot <- nei %>%
      filter(fips == "24510", type=="ON-ROAD") %>%
      group_by(year) %>%
      summarize(vehicleRelated=sum(Emissions))

# plotting ----------------------------------------------------------------

library(ggplot2)
g <- ggplot(aes(year, vehicleRelated), data=nei.for.plot)
g <- g + geom_point()
years <- unique(nei.for.plot$year)
g <- g + scale_x_continuous(breaks=years)
g <- g + scale_y_continuous(label=function(x) {
      lab <- format(x, digits=0)
})
g <- g + labs(
      title="Emissions of PM2.5 from Motor Vehicles\nin Baltimore City in 1999-2008")
g <- g + labs(x = "Years", y = "Emissions of PM2.5 (tons)")
g <- g + geom_text(aes(
      label=format(vehicleRelated, digits=0, scientific=FALSE)),
      size=3, vjust=1.5, hjust=1)
g <- g + geom_hline(aes(yintercept=vehicleRelated[1]),color="blue")
g <- g + geom_hline(aes(yintercept=vehicleRelated[4]),color="blue")
ggsave("./plot5.png", width=5, height=5, dpi=120)
