#
#
#
#
library(SRAdb)
sra_con <- dbConnect(dbDriver("SQLite"), "../SRAmetadb.sqlite")
#
#
library(data.table)
hmp_data <- fread("../data/HMASM.csv")
rs = listSRAfile( c(hmp_data$`SRS ID`), sra_con, fileType = 'sra' )
cmd <- cbind("wget", rs$ftp)
write.table(cmd, "batch.sh", quote=F,sep=" ", row.names=F, col.names=F)
#
#

rs <- getSRA(search_terms = 'metagenome', sra_con = sra_con)
rs_HMP <- getSRA(search_terms = 'microbiome', sra_con = sra_con)

table(rs$library_strategy)
#
library(plyr)
library(dplyr)
#
rs_wgs <- filter(rs, library_strategy == "WGS")
str(rs_wgs)
#
table(rs_wgs$platform)
#
grep("gut", rs_wgs)
#
rs_i <- filter(rs_wgs, platform == "ILLUMINA")
#
hist(rs_i[rs_i$spots > 1000, ]$spots)
rs_meta <- filter(rs_i, study_type == "Metagenomics")
table(rs_meta$study_title)

