
### ALNAP legacy data log file analysis 18-04-2018  

########    Ryan Nazareth ###############

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)
library(plyr)

rm(list = ls()) # clearing workspace
file_path_work <- "M:/PROJECTS/ALNAP website and CRM - 526/Data/Analytics/Old website analytics/Data and scripts/All previous years logfiles"
file_path_home <- "/Volumes/Files-3/PROJECTS/ALNAP website and CRM - 526/Data/Analytics/Old website analytics/Data and scripts/All previous years logfiles"
setwd(file_path_work)

# If you already have an .Rda file, then uncomment the following code to load it in
#all_downloads <- readRDS("M:/PROJECTS/ALNAP website and CRM - 526/Data/Analytics/Old website analytics/Data and scripts/2009-2018 results/data.Rda")

#Otherwise run the chunk below to create a data frame with event label and sessions

years <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017")

all_downloads = data.frame(Event_Label = factor(), Sessions = integer())

for(i in years) {
  file.list <-list.files(path = i , pattern = ".log$")
  #df.list <- lapply(file.list, read.table)
  time_1 <- proc.time()
  for (k in 1:length(file.list)) {
    data <- file.path(i, file.list[k])
    df.list = read.table(data, fill = TRUE)
    df.list <- subset(df.list, select = c(V1, V2, V5))
    colnames(df.list) <- c("Date", "Time", "Event_Label")
    head(df.list, 5)
    doc.list <-df.list[grep(".*?pdf|docx|ppt|pptx|xlsx|xls$", df.list$Event_Label), ]
    doc.frame <- as.data.frame(table(doc.list$Event_Label))
    doc.frame <- doc.frame[!doc.frame$Freq == 0, ]
    colnames(doc.frame) <- c("Event_Label", "Sessions")
    all_downloads <- rbind(all_downloads, doc.frame)
  }

}


time_2 <- proc.time()
time = time_2 - time_1

print(paste('processing the datasets took',round(time[3]/3600, 2), 'hours'))

colnames(all_downloads) <- c("Event_Label", "Sessions")
all_downloads = all_downloads[all_downloads$Sessions!= 0,] 

##Uncomment code below if required to combine old data with new data from analytics. 
#Nov17_Apr18 <- read.csv('Analytics_Nov17_17April18.csv', header = TRUE)
#colnames(Nov17_Apr18) <- c("Event_Label", "Sessions")
#all_downloads <- rbind(all_downloads, Nov17_Apr18)


# Stripping the path to only include the tail 

all_downloads$Resource_Title <- gsub("(.*\\/)", "", all_downloads$Event_Label)
# Alternative is to use baseline function below but this gives problems in windows OS
### if file paths exceed a given length
## all_downloads$Resource_Title <- basename(as.character(all_downloads$Event_Label)) 

# Reordering the columns so that the sessions column is at the end and renaming 'Sessions' column to 'Number of downloads' "
all_downloads <- all_downloads[, c(1,3,2)] 
colnames(all_downloads)[3] <-  "Number_of_downloads"


## Replacing hyphens with spaces

all_downloads$Resource_Title <- gsub("-|%20", " ", all_downloads$Resource_Title)


# Removing irrelvant filenames (e.g. .gif)
all_downloads <- all_downloads[grepl(".gif|.aspx", all_downloads$Resource_Title) == FALSE, ]
head(all_downloads, 10)


# Sum the similar paths to get aggreate session count and Sorting in descending order

all_downloads <- aggregate(Number_of_downloads ~ Resource_Title , data = all_downloads, FUN = sum)
all_downloads <- all_downloads[order(-all_downloads$Number_of_downloads), ] 
rownames(all_downloads) <- 1:nrow(all_downloads)

# Creating another datatframe with only files which contain 'ALNAP'
# in the path
alnap_downloads <- all_downloads[grepl("alnap|ALNAP", all_downloads$Resource_Title), ]
head(alnap_downloads, 10)
ggplot(data= alnap_downloads[1:10,], aes(x= Resource_Title, y = Number_of_downloads)) + geom_bar(stat="identity") + xlab('Resource Title') + ylab('Number of downloads') + coord_flip()


### Save the all_downloads data frame 

saveRDS(all_downloads, file = "datatest.Rda" )


## All downloads from 2009 to April 2018
write.csv(all_downloads, 'M:/PROJECTS/ALNAP website and CRM - 526/Data/Analytics/Old website analytics/Data and scripts/2009-2018 results/all_files_2009-2018.csv', row.names = FALSE)  # only data from old site

## All ALNAP downloads only from 2009 to April 2018
write.csv(alnap_downloads, 'M:/PROJECTS/ALNAP website and CRM - 526/Data/Analytics/Old website analytics/Data and scripts/2009-2018 results/alnap_files_2009-2018.csv', row.names = FALSE)  # only data from old site

