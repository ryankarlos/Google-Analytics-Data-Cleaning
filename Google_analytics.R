library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(stringr)
library(plyr)

rm(list = ls()) # clearing workspace
df = read.table('u_ex170101.log', fill = TRUE)
df <- subset(df, select = c(V1, V2, V5))
colnames(df) <- c("Date", "Time", "Request")
head(df, 5)
pdf <- df[grep(".*?pdf$", df$Request),]

#head(sort(table(pdf$Request),decreasing=T),5)
#head(count(pdf, Request))

## list all log files from current directory
file.list <- list.files(pattern = ".log$")
#df.list <- lapply(file.list, read.table)
reqs = data.frame(Requests = factor(), Frequency = integer())

for(k in 1:length(file.list)){
  df.list = read.table(file.list[k], fill = TRUE)
  df.list <- subset(df.list, select = c(V1, V2, V5))
  colnames(df.list) <- c("Date", "Time", "Request")
  head(df.list, 5)
  pdf.list <- df.list[grep(".*?pdf$", df.list$Request),]
  reqs <- rbind(reqs, as.data.frame(sort(table(pdf.list$Request), decreasing = T)))
}

colnames(reqs) <- c("Requests", "Frequency")
ggplot(data= reqs[1:10,], aes(x=Requests, y = Frequency)) + geom_bar(stat="identity") + xlab('PDF Downloaded') + ylab('Number of requests') + coord_flip()
write.csv(reqs, 'data.csv')