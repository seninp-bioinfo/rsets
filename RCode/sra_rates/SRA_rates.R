library(stringr)
library(RCurl)
library(XML)
library(plyr)
library(dplyr)
library(SRAdb)
sqlfile <- "/Users/psenin/git/SRAmetadb.sqlite"
sra_con <- dbConnect(dbDriver("SQLite"), sqlfile)

#
# configure runtime
options(echo = TRUE)
args <- commandArgs(trailingOnly = TRUE)
#
# print provided args
print(paste("provided args: ", args))
year <- args[1]
#year <- 2017
#

valid_el <- function(ll) {
	"IDENTIFIERS" %in% names(ll)
}

parse_tag <- function(tags, tag) {
	f = tags[tags$tag == tag,]$value
	as.numeric(levels(f))[f]
}



ex_data <- function(x) {
	id = x$IDENTIFIERS$PRIMARY_ID
	tags = ldply(
		x$RUN_ATTRIBUTES[which(ldply(x$RUN_ATTRIBUTES, function(z){ "VALUE" %in% names(z) &&
		    "TAG" %in% names(z) && !is.null(z$TAG) && !is.null(z$VALUE)})$V1)],
		function(z){ data.frame(tag=z$TAG, value=z$VALUE)})
	reads = 0
	if("ENA-SPOT-COUNT" %in% tags$tag) {
		reads = parse_tag(tags, "ENA-SPOT-COUNT")
	}
	bases = 0
	if("ENA-BASE-COUNT" %in% tags$tag) {
		bases = parse_tag(tags, "ENA-BASE-COUNT")
	}
	data.frame(runid = id, reads = reads, bases = bases)
}

parse_date <- function(date) {

	url = paste("http://www.ebi.ac.uk/ena/data/warehouse/search?query=",
	"tax_tree(408169) AND first_public=", date,
	"&fields=library_strategy,library_selection,library_source",
	"&result=read_run&display=xml&download=xml", sep = "")

	res <- xmlParse(URLencode(url))

	xml_data <- xmlToList(res)

	if (!(is.null(xml_data$text)) && str_detect(xml_data[[1]],
		"display type is either not supported or entry is not found")) {
		data.frame(date = date, samples = NA, reads = NA, bases = NA)
	} else {
		dd <- ldply( xml_data[which(laply(xml_data, valid_el))], ex_data)
		data.frame(date = date, samples = dim(dd)[1], reads = sum(dd$reads), bases = sum(dd$bases))
	}

}

start <- paste(year, "/1/1", sep = "")
end <- paste(year, "/12/31", sep = "")

yearSeq <- as.character(seq(as.Date(start), as.Date(end), "days"))

res <- data.frame(date = "", samples = NA, reads = NA, bases = NA)
cTime = Sys.time();

for(i in 1:length(yearSeq)) {
	nTime = Sys.time()
	print(paste("processing ", yearSeq[i], ", prev day took ",
		as.numeric(difftime(nTime, cTime, units = "min")),
		" minutes...", sep = ""))
	dd <- parse_date(yearSeq[i])
	res <- rbind(res, dd)
	write.table(res, paste("res", year, ".tsv", sep = ""), sep = "\t", col.names = T, row.names = F)
	print(paste(dd))
}
