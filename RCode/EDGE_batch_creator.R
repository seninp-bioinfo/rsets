#
#
# read all the _1 and all the _2 files
library(plyr)
library(dplyr)
library(stringr)
#
current_folder <- getwd()
ones <- data.frame(file = unlist(list.files(path = ".", pattern = "*_1.fastq.gz", all.files = T,
                   full.names = F, recursive = F, ignore.case = T, include.dirs = F)),
                   stringsAsFactors = F)
ones$mtime <- aaply(ones, 1, function(x){
  as.numeric(difftime(Sys.time(), file.info(x$file)$mtime, unit = "hours" )) }, .expand = F)
ones <- filter(ones, mtime > 1.0)
#
twos <- data.frame(file = unlist(list.files(path = ".", pattern = "*_2.fastq.gz", all.files = T,
                   full.names = F, recursive = F, ignore.case = T, include.dirs = F)),
                   stringsAsFactors = F)
twos$mtime <- aaply(twos, 1, function(x){
  as.numeric(difftime(Sys.time(), file.info(x$file)$mtime, unit = "hours" )) }, .expand = F)
twos <- filter(twos, mtime > 1.0)
#
# manipulate names to extract the mask
#
library(stringr)
ones$mask <- str_extract(ones$file, "[^_]*")
twos$mask <- str_extract(twos$file, "[^_]*")
#
# merge
merged <- base::merge(ones, twos, by = c("mask"), all = T)
merged$tdiff <- aaply(merged, 1, function(x){ max(x$mtime.x, x$mtime.y, na.rm = T) }, .expand = F)
merged <- arrange(select(merged, one_of(c("mask", "file.x", "file.y", "tdiff"))), tdiff)
#
# project description ..
library(SRAdb)
sqlfile <- "/users/222935/data/SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
#
merged$experiment <- aaply(merged, .margins = 1, function(x) {
  rs <- dbGetQuery(sra_con, paste( "select * from RUN where run_accession = ",
             shQuote(x$mask, type = "sh"), sep = "" ))
  rs$experiment_accession }, .expand = F)
merged$sample <- aaply(merged, .margins = 1, function(x) {
  rs <- dbGetQuery(sra_con, paste( "select * from EXPERIMENT where experiment_accession = ",
                                   shQuote(x$experiment, type = "sh"), sep = "" ))
  rs$sample_accession }, .expand = F)
#
#
library(data.table)
hmp_data <- fread("/users/222935/data/HMASM.csv")
#
merged <- base::merge(merged, hmp_data, by.x = c("sample"), by.y = c("SRS ID"))
merged <- merged[complete.cases(merged), ]
res <- data.frame(run = merged$mask, Q1 = merged$file.x, Q2 = merged$file.y,
                  site = merged$`Body Site`, sample = merged$sample,
                  experiment = merged$experiment, stringsAsFactors = F)
#
#
res$project_name <- paste(res$site, res$run, sep = "_")
#
res$project_description <- paste("site:", res$site, ", sample:", res$sample,
                                 ", experiment:", res$experiment, ", run:", res$run, sep = "")
#
#
res <- select(res, one_of(c("project_name", "Q1", "Q2", "project_description")))
colnames(res) <- c("Project", "Q1", "Q2", "Description")
#
res$Q1 <- paste(current_folder, "/", res$Q1, sep = "")
res$Q2 <- paste(current_folder, "/", res$Q2, sep = "")
write.table(res, file = "batch_01.tsv", quote = F, sep = "\t", col.names = T, row.names = F)
#
#


