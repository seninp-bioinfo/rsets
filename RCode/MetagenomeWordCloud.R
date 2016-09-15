#
# the general tinkering script
#
library(SRAdb)
sqlfile <- "/users/222935/data/SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
#
#
library(data.table)
hmp_data <- fread("/users/222935/data/HMASM.csv")
rs = listSRAfile( c(hmp_data$`SRS ID`), sra_con, fileType = 'sra', srcType = "fasp" )
#
ascpCMD <-
  "/users/222935/.aspera/connect/bin/ascp -i /users/222935/.aspera/connect/etc/asperaweb_id_dsa.openssh -QT -l 500m"
#
for (f in rs$run) {
  print(paste(
    "Run ",f,"; current time: ",format(Sys.time(), "%D %H:%M:%S"),sep = ""
  ))

  if (0 == length(list.files(path = ".", pattern = paste("^",f,".*sra$",sep =
                                                         "")))) {
    res <- try(getFASTQfile(f, srcType = "fasp", ascpCMD = ascpCMD))

    if ("try-error" %in% class(res)) {
      print(paste(res))
      res <-
        try(getSRAfile(f, sra_con = sra_con, destDir = getwd(), fileType = "sra"))

      if ("try-error" %in% class(res)) {
        print(paste(res))
      }else{
        system(paste("fastq-dump --split-files --gzip ",f,".sra",sep = ""))
      }
    }

    Sys.sleep(10)

  }else{
    print(paste("Run ",f," found, iterating further", sep = ""))
  }

}

# the batch for running HMP download...
# cmd <- cbind("wget", rs$ftp)
# write.table(cmd, "batch.sh", quote = F, sep = " ", row.names = F, col.names = F)
#
library(plyr)
library(dplyr)
#
# intial filter looking for anything containing metagenom
rs <- getSRA(search_terms = '"metagenom*" OR "Metagenom*"', sra_con = sra_con)
# dim(rs)
# [1] 514781     70
#
rs_wgs <- filter(rs, library_strategy == "WGS")
# 76928
rs_i <- filter(rs_wgs, platform == "ILLUMINA")
# 68045
rs_meta <- filter(rs_i, study_type == "Metagenomics")
# 47874
rs_meta$read_length <- rs_meta$bases / rs_meta$spots
rs_meta[is.na(rs_meta$read_length), ]$read_length <- -1
rs_meta <- rs_meta[rs_meta$read_length >= 100 & rs_meta$read_length <= 300, ]
# 68045
#
rsf <- getSRA(search_terms = '"fosmid"', sra_con = sra_con)
rs_meta <- dplyr::filter( rs_meta, !(run %in% dplyr::intersect( rsf$run, rs_meta$run)))
# 41564
#
library(tm)
library(SnowballC)
library(wordcloud)
#
#
experiment_title_corpus <- Corpus(VectorSource(rs_meta$experiment_title))
#getTransformations()
experiment_title_corpus <- tm_map(experiment_title_corpus, removePunctuation)
experiment_title_corpus <- tm_map(experiment_title_corpus, stripWhitespace)
experiment_title_corpus <- tm_map(experiment_title_corpus, removeNumbers)
experiment_title_corpus <- tm_map(experiment_title_corpus, removeWords, stopwords('english'))
#
experiment_title_corpus <- tm_map(experiment_title_corpus, stemDocument, mc.cores = 4)
#
wordcloud(experiment_title_corpus, scale = c(6, 1), max.words = 500, random.order = FALSE)
#
grep("infant", rs_meta)
#


#
library(tm)
experiment_title_corpus <- Corpus(VectorSource(rs$study_title))
#getTransformations()
experiment_title_corpus <- tm_map(experiment_title_corpus, removePunctuation)
experiment_title_corpus <- tm_map(experiment_title_corpus, stripWhitespace)
experiment_title_corpus <- tm_map(experiment_title_corpus, removeNumbers)
experiment_title_corpus <- tm_map(experiment_title_corpus, removeWords, stopwords('english'))
library(SnowballC)
experiment_title_corpus <- tm_map(experiment_title_corpus, stemDocument, mc.cores = 1)
library(wordcloud)
wordcloud(experiment_title_corpus, max.words = 100, random.order = FALSE)
#
dbListFields(sra_con, "study")
rs <- dbGetQuery(sra_con, paste( "SELECT study_type AS StudyType,
                                 count( * ) AS Number FROM `study` GROUP BY study_type order
                                 by Number DESC ", sep = ""))
#
dbListFields(sra_con, "run")
dbListFields(sra_con, "experiment")
#
rs <- dbGetQuery(sra_con, paste( "SELECT study_type AS StudyType,
                                 count( * ) AS Number FROM `study` GROUP BY study_type order
                                 by Number DESC ", sep = ""))
#


rsf <- getSRA(search_terms = '"fosmid"', sra_con = sra_con)
