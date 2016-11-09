library(plyr)
library(dplyr)
library(stringr)
# allReads.summary.tsv
setwd("/scratch-222935/HMP/clean")
g_refseq_res <- list.files(pattern = "allReads.summary.tsv", recursive = TRUE)
#
g_refseq_res <- g_refseq_res[grep("_gottcha2-speDB-refseq", g_refseq_res) ]
#
g_refseq_res <- data.frame(path = g_refseq_res, stringsAsFactors = F)
g_refseq_res$run_id <- str_match(g_refseq_res$path, ".*(SRR\\d+)")[,2]
#
library(SRAdb)
sqlfile <- "/users/222935/data/SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
trim <- function(x) { gsub("^\\s+|\\s+$", "", x) }
revise_name <- function(run_accession) {
  rs <- dbGetQuery(sra_con, paste( "select * from RUN where run_accession = ",
                                   shQuote(run_accession, type = "sh"), sep = "" ))
  rs <- dbGetQuery(sra_con, paste( "select * from EXPERIMENT where experiment_accession = ",
                                   shQuote(rs$experiment_accession, type = "sh"), sep = "" ))
  rs <- dbGetQuery(sra_con, paste( "select * from SAMPLE where sample_accession = ",
                                   shQuote(rs$sample_accession, type = "sh"), sep = "" ))
  body_site <- str_match(rs$sample_attribute, ".*(body site: )([\\w\\s]+).*")
  gsub("\\s+", "_", trim(body_site[3]) )
}
g_refseq_res$new_names <- aaply(g_refseq_res, 1, .expand = FALSE, function(x)
  { paste(gsub("G_DNA_", "", revise_name(x$run_id)), "_", x$run_id, sep = "") })
#
g_refseq_res$batch <- paste("cp ", g_refseq_res$path, " ", g_refseq_res$new_names,
                            "_gottcha2-speDB-refseq.tsv", sep = "")
#
write.table(g_refseq_res$batch, file = "collect_refseq.sh", col.names = F, row.names = F, quote = F)
