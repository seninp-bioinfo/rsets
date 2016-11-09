library(plyr)
library(dplyr)
library(stringr)
#
setwd("/scratch-222935/HMP/fastq")
#
dd <- data.frame(files = list.files(pattern = "*fastq.gz$"), stringsAsFactors = F)
dd$mask <- str_match(dd$files, "(.*)_[\\d\\w.]+$")[,2]
dr <- data.frame(run_id = unique(dd$mask), stringsAsFactors = F)
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
  body_site <- str_match(rs$sample_attribute, ".*(body site: )([\\w\\s]+).*")
  sex <- str_match(rs$sample_attribute, ".*(sex: )([\\w\\s]+).*")
  gsub("\\s+", "_", paste(trim(body_site[3]), "_", trim(sex[3]), sep = ""))
}
dr$prj_name <- aaply(dr, 1, .expand = FALSE, function(x)
{ paste(gsub("G_DNA_", "", revise_name(x$run_id)), "_", x$run_id, sep = "") })
#
# check for pairing in reads
#
check_paired <- function(files, mask) {
  if ( is.element(paste(mask, "_1.fastq.gz", sep = ""), files) &&
           is.element(paste(mask, "_2.fastq.gz", sep = ""), files) ) {
    1
  } else if ( is.element(paste(mask, "_1.fastq.gz", sep = ""), files)) {
    0
  } else {
  -1
  }
}
#
files <- list.files(path = "/panfs/biopan01/scratch-222935/HMP/fastq/")
#
#
dr$paired <- aaply(dr, 1, .expand = FALSE, function(x){ check_paired(files, x$run_id) } )
dr <- dr[dr$paired == 1, ]
#
#
#
setwd("/scratch-222935/HMP/clean")
g_refseq_res <- list.dirs(recursive = F)
setwd("/scratch-222935/HMP/clean2")
g_refseq_res2 <- list.dirs(recursive = F)
g_refseq_res <- data.frame(path = g_refseq_res, stringsAsFactors = F)
g_refseq_res$run_id <- str_match(g_refseq_res$path, ".*(SRR\\d+)")[,2]
#
pre_batch <- subset(dr, !(dr$run_id %in% g_refseq_res$run_id))
#
#
#
#
#
setwd("/scratch-222935/HMP/fastq")
pre_batch$batch <- paste("mkdir -p /scratch-222935/HMP/clean3/", pre_batch$prj_name, sep = "")
write.table(pre_batch$batch, file = "../make_folders.sh", col.names = F, row.names = F, quote = F)
#
pre_batch$batch <- paste("/panfs/biopan01/edge-dev-master/edge-dev-test/scripts/illumina_fastq_QC.pl -p ",
                  "/panfs/biopan01/scratch-222935/HMP/fastq/", pre_batch$run_id, "_1.fastq.gz ",
                  "/panfs/biopan01/scratch-222935/HMP/fastq/", pre_batch$run_id, "_2.fastq.gz ",
                  " -d /scratch-222935/HMP/clean3/", pre_batch$prj_name, " -t 4", sep = "")
#
#
write.table(pre_batch$batch, file = "../cleanup_p3t.sh", col.names = F, row.names = F, quote = F)
#
#
#
#

