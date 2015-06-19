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
      scc.file <- file.path(data.dir, "Source_Classification_Code.rds")
      if (sum(file.exists(nei.file, scc.file)) < 2)
            data.download(data.dir)
      message(paste("Reading ", nei.file, "...", collapse=""))
      nei <- readRDS(nei.file)
      message(paste("Reading ", scc.file, "...", collapse=""))
      scc <- readRDS(scc.file)
}