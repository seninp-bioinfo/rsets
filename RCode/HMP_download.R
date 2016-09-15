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
