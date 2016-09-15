#
# the general tinkering script
#
library(SRAdb)
sqlfile <- "../SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
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
library(plyr)
library(dplyr)
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
