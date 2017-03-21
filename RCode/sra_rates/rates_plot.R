library(data.table)
setwd("sra_rates/")
files <- c("res2010.tsv", "res2011.tsv", "res2012.tsv", "res2013.tsv", "res2014.tsv", "res2015.tsv", "res2016.tsv", "res2017.tsv")
dd <- read.table(files[1], sep = '\t', quote = "\"", skip = 2)
for(f in files[2:length(files)]){
  print(paste(f))
  dd <- rbind(dd, read.table(f, sep = '\t', quote = "\"", skip = 2))
}
dd$V1 <- as.Date(dd$V1)
names(dd) <- c("submission_date", "samples", "reads", "bases")
#
library(ggplot2)
library(dplyr)
library(zoo)
#
dd %>% mutate(month = as.yearmon(submission_date)) %>%
  count(month, wt=bases) -> monthly_bases
monthly_bases$cumsum <- cumsum(monthly_bases$n)

p <- ggplot(monthly_bases, aes(x = month, y = cumsum)) + geom_line() +
  labs(y = "Cumulative sum of bases", x = "Months") +
  ggtitle("Cumulative submission to SRA for taxon 408169 (metagenome)")
p
