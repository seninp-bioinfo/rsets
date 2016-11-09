#
# the general tinkering script
#
library(devtools)
library(SRAdb)
#library(ShinySRAdb)
#ShinySRAdb()

install_github("seandavi/SRAdb-app")
sqlfile <- "../SRAmetadb.sqlite"
sqlfile <- "/Users/psenin/workspace/rsets/SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
#
#
library(data.table)
hmp_data <- fread("/users/222935/data/HMASM.csv")
hmp_data <- fread("../data/HMASM.csv")
# get runs id for SRS, i.e. experiments
hmp_runs = listSRAfile( c(hmp_data$`SRS ID`), sra_con, fileType = 'sra', srcType = "fasp" )

#
get_library_strategy <- function(run_accession) {
 rs <- dbGetQuery(sra_con, paste( "select * from RUN where run_accession = ",
                                  shQuote("SRR060400", type = "sh"), sep = "" ))
 rs <- dbGetQuery(sra_con, paste( "select * from EXPERIMENT where experiment_accession = ",
                                  shQuote(rs$experiment_accession, type = "sh"), sep = "" ))
 rs <- dbGetQuery(sra_con, paste( "select * from SAMPLE where sample_accession = ",
                                  shQuote(rs$sample_accession, type = "sh"), sep = "" ))
 rs$library_strategy
}

library(plyr)
hmp_runs$library_strategy <- daply(hmp_runs, .(run), function(x) { print(paste(x$run)); get_library_strategy(x$run)})

table(hmp_runs$library_strategy)

get_library_strategy("SRR061450")
run_accession = "SRR061450"

rs <- dbGetQuery(sra_con, paste( "select * from STUDY where study_accession = ",
                                 shQuote("SRP002163", type = "sh"), sep = "" ))
str(rs)

dbListFields(sra_con,"experiment")

ascpCMD <-
  "/users/222935/.aspera/connect/bin/ascp -i /users/222935/.aspera/connect/etc/asperaweb_id_dsa.openssh -QT -l 300m"
#
for (f in rs$run) {
  print(paste(
    "Run ",f,"; current time: ",format(Sys.time(), "%D %H:%M:%S"),sep = ""
  ))

  if (0 == length(list.files(path = ".", pattern = paste("^",f,".*sra$",sep =
                                                         "")))) {
    res <- try(getFASTQfile(f, sra_con, srcType = "fasp", ascpCMD = ascpCMD))

    if ("try-error" %in% class(res)) {
      print(paste(res))
      res <-
        try(getSRAfile(f, sra_con = sra_con, destDir = getwd(),
                       method = "wget", fileType = "sra"))

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


#
res = as.data.frame( listSRAfile( "SRR062326", sra_con, fileType = 'sra', srcType = "fasp" ) )

for (f in rs$run) {
  print(paste(
    "Run ",f,"; current time: ",format(Sys.time(), "%D %H:%M:%S"),sep = ""
  ))

  if (0 != length(list.files(path = ".", pattern = paste("^",f,".*sra$",sep =
                                                         "")))) {

    if (0 == length(list.files(path = ".", pattern = paste("^",f,".*fastq.gz$",sep =
                                                           "")))) {
        system(paste("fastq-dump --split-files --gzip ",f,".sra",sep = ""))
    }

    info = listSRAfile( f, sra_con, fileType = 'sra', srcType = "fasp" )

    res = rbind( res, info )
  }

}

