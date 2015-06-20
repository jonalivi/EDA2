# Across the United States, how have emissions 
# from coal combustion-related sources changed from 1999-2008?

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
      scc.file <- file.path(data.dir, "Source_Classification_Code.rds")
      if (sum(file.exists(nei.file), file.exists(scc.file))<2)
            data.download(data.dir)
      message(paste("Reading ", nei.file, "...", collapse=""))
      nei <<- readRDS(nei.file)
      message(paste("Reading ", scc.file, "...", collapse=""))
      scc <<- readRDS(scc.file)
}

# prepare the data for plotting -------------------------------------------
data.read()
library(dplyr)
## here "coal combustion-related" sources means
## containing "Fuel Comb" and "Coal" 
## in EI.Sector
scc.index <- scc %>%
      filter(grepl("Fuel Comb", EI.Sector)) %>%
      filter(grepl("Coal", EI.Sector)) %>%
      select(SCC)
scc.index <- as.character(scc.index$SCC)
nei.for.plot <- nei %>%
      filter(SCC %in% scc.index) %>%
      group_by(year) %>%
      summarize(totalCoalRelated=sum(Emissions))


# plotting ----------------------------------------------------------------

library(ggplot2)
g <- ggplot(aes(year, totalCoalRelated), data=nei.for.plot)
g <- g + geom_point() + geom_line(lty=2)
years <- unique(nei.for.plot$year)
g <- g + scale_x_continuous(breaks=years)
g <- g + scale_y_continuous(label=function(x) {
            lab <- format(x, digits=0, 
                          scientific=FALSE, big.mark=" ")
      })
g <- g + labs(
      title="PM2.5 Emissions from Coal Combustion-Related Sources\nin the USA in 1999-2008")
g <- g + labs(x = "Years", y = "PM2.5 Emissions (tons)")
g <- g + geom_text(aes(
      label=format(totalCoalRelated, digits=0, 
                   scientific=FALSE, big.mark=" ")),
      size=3, vjust=1.5)
ggsave("./plot4.png")
