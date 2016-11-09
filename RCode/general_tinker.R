#
# #1. listing sub-folders
#
setwd("/scratch-222935/HMP/clean3")
dd <- data.frame(folders = list.files())
#
dd$folders <- paste(getwd(), "/", dd$folders, sep = "")
library(plyr)
library(stringr)
#
ft <- dd$folders[1]
#
# #2. chcking if the cleanup job finished
#
check_cleanup <- function(ff) {
  files <- list.files(path = ff)
  if (is.element("QC.1.trimmed.fastq", files) && is.element("QC.2.trimmed.fastq", files)) {
    TRUE
  } else {
    is.element("QC.unpaired.trimmed.fastq", files)
  }
}
#
library(plyr)
dd$finished <- aaply(dd, 1, function(x){ check_cleanup(x$folders) } )
#
# #3 creating a batch to clean-up un-finished and failed jobs
#
dr <- dd[dd$finished == FALSE,]
dr$rm <- aaply(dr, 1, function(x){ paste("rm -rf ", x$folders, sep = "") })
write.table(dr$rm, file = "../rmfolders.sh", col.names = F, row.names = F, quote = F)
#
# #4 creating a batch to re-run cleanup
#
library(stringr)
dr$outfolder
dr$mask <- str_match(dr$folders, ".*_([\\d\\w]+$)")[,2]
#
dr$batch <- paste("/panfs/biopan01/edge-dev-master/edge-dev-test/scripts/illumina_fastq_QC.pl -p ",
                  "/temp/SRA-data/HMP/", dr$mask, "_1.fastq.gz ",
                  "/temp/SRA-data/HMP/", dr$mask, "_2.fastq.gz ",
                  " -d ", dr$folders, " -t 4", sep = "")
write.table(dr$batch, file = "../cleanup.sh", col.names = F, row.names = F, quote = F)
#
# #5 attach the counts
#
library(data.table)
reads_sum <- function(ff) {
  files <- list.files(path = ff)
  if (is.element("fastqCount.txt", files)) {
    tt <- fread( paste(ff, "/", "fastqCount.txt", sep = "") )
    unlist(as.numeric(sum(tt$V2)))
  } else {
    -1
  }
}
dr$count <- aaply(dr, 1, .expand = F, function(x){ reads_sum(x$folders) } )


library(stringr)
dr <- dd[dd$finished == TRUE,]
gzip_cmd <- function(ff) {
  files <- list.files(path = ff)
  gcmd <- "cat "
  if (is.element("QC.1.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.1.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.2.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.2.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.unpaired.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.unpaired.trimmed.fastq ", sep = "")
  }
  gcmd <- paste(gcmd, "| gottcha.py -i /dev/stdin --database ",
                "/panfs/biopan01/edge-dev-master/edge-dev-test/database/GOTTCHA2/Virus_VIPR_HIVdb_IRD_NCBI_xHuBaAr_noEngStv.species.fna ",
                "--outdir ", ff, "/", prj_mask, "_", "gottcha2-speDB-v ",
                "-r READ_COUNT ",
                "-p allReads ", " -t 8",
                sep = "")
  gcmd
}
dr$batch <- aaply(dr, 1, .expand = F, function(x){ gottcha_cmd(x$folders, x$prj_mask, x$count) } )
#
write.table(dr$batch, file = "../gottcha2-virus.sh", col.names = F, row.names = F, quote = F)

#
# create the GOTTCHA batches
#
library(stringr)
dr <- dd[dd$finished == TRUE,]
dr$mask <- str_match(dr$folders, ".*_([\\d\\w]+$)")[,2]
dr$prj_mask <- str_match(dr$folders, ".*/([\\d\\w]+$)")[,2]
#
#
gottcha_cmd <- function(ff, prj_mask, cnt) {
  files <- list.files(path = ff)
  gcmd <- "cat "
  if (is.element("QC.1.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.1.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.2.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.2.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.unpaired.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.unpaired.trimmed.fastq ", sep = "")
  }
  gcmd <- paste(gcmd, "| gottcha.py -i /dev/stdin --database ",
                "/panfs/biopan01/edge-dev-master/edge-dev-test/database/GOTTCHA2/Virus_VIPR_HIVdb_IRD_NCBI_xHuBaAr_noEngStv.species.fna ",
                "--outdir ", ff, "/", prj_mask, "_", "gottcha2-speDB-v ",
                "-r READ_COUNT ",
                "-p allReads ", " -t 8",
                sep = "")
  gcmd
}
dr$batch <- aaply(dr, 1, .expand = F, function(x){ gottcha_cmd(x$folders, x$prj_mask, x$count) } )
#
write.table(dr$batch, file = "../gottcha2-virus.sh", col.names = F, row.names = F, quote = F)
#
#
#
gottcha_cmd <- function(ff, prj_mask, cnt) {
  files <- list.files(path = ff)
  gcmd <- "cat "
  if (is.element("QC.1.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.1.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.2.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.2.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.unpaired.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.unpaired.trimmed.fastq ", sep = "")
  }
  gcmd <- paste(gcmd, "| gottcha.py -i /dev/stdin --database ",
                "/panfs/biopan01/edge-dev-master/edge-dev-test/database/GOTTCHA2/RefSeq-Release75.Bacteria.species.fna ",
                "--outdir ", ff, "/", prj_mask, "_", "gottcha2-speDB-refseq ",
                "-r READ_COUNT ",
                "-p allReads ", " -t 8",
                sep = "")
  gcmd
}
dr$batch <- aaply(dr, 1, .expand = F, function(x){ gottcha_cmd(x$folders, x$prj_mask, x$count) } )
#
write.table(dr$batch, file = "../gottcha2-refseq.sh", col.names = F, row.names = F, quote = F)

#
# create the MASH pre-processing batches
#
library(stringr)
dr <- dd[dd$finished == TRUE,]
dr$mask <- str_match(dr$folders, ".*_([\\d\\w]+$)")[,2]
dr$prj_mask <- str_match(dr$folders, ".*/([\\d\\w]+$)")[,2]
#
#
gottcha_cmd <- function(ff, prj_mask, cnt) {
  files <- list.files(path = ff)
  gcmd <- "cat "
  if (is.element("QC.1.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.1.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.2.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.2.trimmed.fastq ", sep = "")
  }
  if (is.element("QC.unpaired.trimmed.fastq", files)) {
    gcmd <- paste(gcmd, ff, "/QC.unpaired.trimmed.fastq ", sep = "")
  }
  gcmd <- paste(gcmd, "| seqtk fq2fa - >", ff, "/",
                prj_mask, ".fa",
                sep = "")
  gcmd
}
dr$batch <- aaply(dr, 1, .expand = F, function(x){ gottcha_cmd(x$folders, x$prj_mask, x$count) } )
#
write.table(dr$batch, file = "../dump_fasta.sh", col.names = F, row.names = F, quote = F)
#


