#
setwd("/temp/SRA-data/HMP/clean")
#
dd <- data.frame(folders = gsub("./", "", list.dirs(recursive = F)), stringsAsFactors = F)
#
library(stringr)
dd$run_id <- str_match(dd$folders, ".*(SRR.*)")[,2]
#
#
library(SRAdb)
sqlfile <- "/users/222935/data/SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
#
trim <- function(x) { gsub("^\\s+|\\s+$", "", x) }
revise_name <- function(run_accession) {
  rs <- dbGetQuery(sra_con, paste( "select * from RUN where run_accession = ",
                                   shQuote(run_accession, type = "sh"), sep = "" ))
  rs <- dbGetQuery(sra_con, paste( "select * from EXPERIMENT where experiment_accession = ",
                                   shQuote(rs$experiment_accession, type = "sh"), sep = "" ))
  rs <- dbGetQuery(sra_con, paste( "select * from SAMPLE where sample_accession = ",
                                   shQuote(rs$sample_accession, type = "sh"), sep = "" ))
  body_site <- str_match(rs$sample_attribute, ".*(body site: )([\\w\\s/]+).*")
  sex <- str_match(rs$sample_attribute, ".*(sex: )([\\w\\s]+).*")
  gsub("\\s+", "_", paste(trim(body_site[3]), "_", trim(sex[3]), sep = ""))
}
#
library(plyr)
dd$new_name <- aaply(dd, 1, .expand = F, function(x) { paste(gsub("G_DNA_|G_DNA_Attached/", "",
                             revise_name(x$run_id)), "_", x$run_id, sep = "") })
#
#
#
dd$batch <- paste("mv ", dd$folders, " ", dd$new_name, sep = "")
write.table(dd$batch, file = "rename1.sh", col.names = F, row.names = F, quote = F)
#
#
#
dd <- data.frame(folders = list.dirs(recursive = T), stringsAsFactors = F)
dd <- data.frame(folders = dd[grep("speDB|BWA", dd$folders), ], stringsAsFactors = F)
dd$run_id <- str_match(dd$folders, ".*(SRR.*)_.*")[,2]
dd$pre <- str_match(dd$folders, "(.*)/.*")[,2]
dd$post <- str_match(dd$folders, ".*/(.*)")[,2]
#
revise_name <- function(run_accession) {
  rs <- dbGetQuery(sra_con, paste( "select * from RUN where run_accession = ",
                                   shQuote(run_accession, type = "sh"), sep = "" ))
  rs <- dbGetQuery(sra_con, paste( "select * from EXPERIMENT where experiment_accession = ",
                                   shQuote(rs$experiment_accession, type = "sh"), sep = "" ))
  rs <- dbGetQuery(sra_con, paste( "select * from SAMPLE where sample_accession = ",
                                   shQuote(rs$sample_accession, type = "sh"), sep = "" ))
  body_site <- str_match(rs$sample_attribute, ".*(body site: )([\\w\\s/]+).*")
  sex <- str_match(rs$sample_attribute, ".*(sex: )([\\w\\s]+).*")
  gsub("\\s+", "_", paste(trim(body_site[3]), "_", trim(sex[3]), sep = ""))
}
#
library(plyr)
dd$new_name <- aaply(dd, 1, .expand = F, function(x) { paste(gsub("G_DNA_|G_DNA_Attached/", "",
                                                                  revise_name(x$run_id)), sep = "") })
#
dd$new_post <- str_match(dd$post, ".*(_SRR.*)")[,2]
#
dd$batch <- paste("mv ", dd$folders, " ", dd$pre, "/", dd$new_name, dd$new_post, sep = "")
write.table(dd$batch, file = "rename2.sh", col.names = F, row.names = F, quote = F)

