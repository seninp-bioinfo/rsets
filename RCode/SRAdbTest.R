#
# the general tinkering script
#
library(SRAdb)
sqlfile <- "../SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)
#
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
