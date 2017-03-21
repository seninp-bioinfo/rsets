library(ENAbrowseR)

# get counts in read_run for offset
ena_taxonomy("metagenomes")
#metagenomes, Taxid:408169
#direct bases subtree subsize
#read_experiment     3746  3 Tb  537467  249 Tb
#read_run            3758  3 Tb  600236  249 Tb
#read_study            57  3 Tb   11382  249 Tb
#sample              3165     -  549037       -


  fields <- c("study_accession", "sample_accession", "run_accession", "library_strategy", "library_selection",
              "library_source",  "base_count", "first_public")

m1 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000)
m2 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000, offset=100001 )
m3 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000, offset=200001 )
m4 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000, offset=300001 )
m5 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000, offset=400001 )
m6 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000, offset=500001 )
m7 <- ena_search("tax_tree(408169)", "read_run", fields, drop=FALSE, limit=100000, offset=600001 )

# combine ( I can send a copy of this table if needed)
mg <- rbind(m1, m2, m3, m4, m5, m6, m7)

save(mg, file="~/mg.rda")

## Identify  WGS random genomic,  Amplicon and other

n <- mg$library_strategy=="WGS" & grepl("^RANDOM", mg$library_selection)  & grepl("GENOMIC$", mg$library_source)

mg$wgs<- ""
mg$wgs[n] <- "WGS"
mg$wgs[mg$library_strategy=="AMPLICON"] <- "Amplicon"
mg$wgs[mg$wgs==""] <- "Other"

table(mg$wgs)

#Amplicon    Other      WGS
#293218    71351    46244   # june 2016
#410044   132197    57995   # march 20, 2107

# add month
mg$month <- as.Date( paste0(substr(mg$first_public, 1,7), "-15"))

library(dplyr)
x <-   group_by(mg, wgs, month)  %>%
  summarize( bases= sum(base_count, na.rm=TRUE), samples= n_distinct(sample_accession), runs=n() )

# drop current month
x <- filter(x, month !="2017-03-15")

# rename for key
names(x)[1] <- "Library"

library(ggplot2)
ggplot(data=subset(x, Library!="Other"), aes(x=month, y=log10(bases), group=Library, colour=Library, shape=Library)) +
  geom_line() + geom_point() +xlab("Released") + ylab("Total bases (log10) / month") +  geom_smooth()


ggplot(data=subset(x, Library!="Other"), aes(x=month, y=runs, group=Library, colour=Library, shape=Library)) +
  geom_line() + geom_point() +xlab("Released") + ylab("Total runs / month") + scale_y_log10() + geom_smooth()
