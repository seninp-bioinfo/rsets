library(plyr)
library(dplyr)
library(stringr)
#
dd <- data.frame(folders = list.files())
#
dd$folders <- paste(getwd(), "/", dd$folders, sep = "")
#
ft <- dd$folders[1]
#
# #2. checking if the cleanup job finished
#
check_cleanup <- function(ff) {
  files <- list.files(path = ff)
  if (is.element("QC.1.trimmed.fastq.gz", files) && is.element("QC.2.trimmed.fastq.gz", files)) {
    TRUE
  } else {
    is.element("QC.unpaired.trimmed.fastq.gz", files)
  }
}
#
dd$finished <- aaply(dd, 1, function(x){ check_cleanup(x$folders) } )
#
dr <- dd[dd$finished == TRUE,]
dr$mask <- str_match(dr$folders, ".*_([\\d\\w]+$)")[,2]
dr$prj_mask <- str_match(dr$folders, ".*/([\\d\\w]+$)")[,2]
#
gottcha_cmd <- function(ff, prj_mask, cnt) {
  files <- list.files(path = ff)
  gcmd <- "zcat "
  if (is.element("QC.1.trimmed.fastq.gz", files) && is.element("QC.2.trimmed.fastq.gz", files)) {
    gcmd <- paste(gcmd, ff, "/QC.1.trimmed.fastq.gz ", sep = "")
    gcmd <- paste(gcmd, ff, "/QC.2.trimmed.fastq.gz ", sep = "")
  }
  if (is.element("QC.unpaired.trimmed.fastq.gz", files)) {
    gcmd <- paste(gcmd, ff, "/QC.unpaired.trimmed.fastq.gz ", sep = "")
  }
  gcmd <-
    paste(gcmd, "| /panfs/biopan01/edge-dev-master/edge-dev-test/scripts/runReadsToContig.pl ",
          " -u /dev/stdin ",
  "-ref /panfs/biopan01/edge-dev-master/edge-dev-test/database/bwa_index/NCBI-Bacteria-Virus.fna ",
              "-pre SRRxxxToNCBIRef -d ", ff, "/", prj_mask, "_", "BWA ", sep = "")
  gcmd
}
dr$batch <- aaply(dr, 1, .expand = F, function(x){ gottcha_cmd(x$folders, x$prj_mask, x$count) } )
#
write.table(dr$batch, file = "../BWA-test1210.sh", col.names = F, row.names = F, quote = F)
#
# SRRxxxToNCBIRef_coverage.table
#
setwd("/scratch-222935/HMP/clean")
g_refseq_res <- list.files(pattern = "SRRxxxToNCBIRef_coverage.table", recursive = TRUE)
