#
setwd("/temp/SRA-data/HMP/")
library(data.table)
#
processed <- fread("/users/222935/data/cleanup.csv", header = F)
dd <- data.frame(files = list.files(pattern = "*.sra"))
#
str(data)
#
library(plyr)
library(dplyr)
library(stringr)
#
data$V2 <- gsub(pattern = "/panfs/biopan01/scratch-222935/data/HMP",
            replacement = "/temp/SRA-data/HMP", data$V2)
data$V3 <- gsub(pattern = "/panfs/biopan01/scratch-222935/data/HMP",
                replacement = "/temp/SRA-data/HMP", data$V3)
#
data$folder <- paste("/temp/SRA-data/HMP/", "clean/", data$V1, sep = "")
#
#
data$batch <- paste("fastq-dump --gzip --skip-technical --readids --dumpbase --split-files --clip -L 5 -v ",
                    data$mask, sep = "")
#
data$batch <- paste("/panfs/biopan01/edge-dev-master/edge-dev-test/scripts/illumina_fastq_QC.pl -p ",
                    data$V2, " ", data$V3, " -d ", data$folder, " -t 4", sep = "")
#
data$batch[1:2]
write.table(data$batch, file = "batch-qc.sh", quote = F, row.names = F, col.names = F)
#
dr$mask <- str_match(dr$folders, ".*_([\\d\\w]+$)")[,2]


ex_rerun$batch <- paste("fastq-dump --gzip --skip-technical --readids --dumpbase --split-files --clip -L 5 -v /temp/SRA-data/HMP/",
                        ex_rerun$run_id, ".sra", sep = "")
