library(data.table)
tt <- fread("~/set61_k27_s6k.tsv", header = T, stringsAsFactors = FALSE, data.table = FALSE)
tt <- fread("~/set61_k21_s3k.tsv", header = T, stringsAsFactors = FALSE, data.table = FALSE)
#
str(tt)
#
library(stringr)
colnames(tt) <- str_match(colnames(tt), "(.*)\\.fa")[, 2]
tt[, 1] <- str_match(tt[, 1], "(.*)\\.fa")[, 2]
rownames(tt) <- tt[, 1]
tt <- tt[, -1]
#
cols <- colnames(tt)
rows <- rownames(tt)
runs <- str_match(cols, ".*(SRR.*)")[,2]
#
library(SRAdb)
sqlfile <- "/Users/psenin/workspace/rsets/SRAmetadb.sqlite"
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
  body_site <- str_match(rs$sample_attribute, ".*(body site: )([\\w\\s/]+).*")
  sex <- str_match(rs$sample_attribute, ".*(sex: )([\\w\\s]+).*")
  gsub("\\s+", "_", paste(trim(body_site[3]), "_", trim(sex[3]), sep = ""))
}
#
library(plyr)
new_names <- aaply(runs, 1, function(x) { paste(gsub("G_DNA_|G_DNA_Attached/", "", revise_name(x)), "_", x, sep = "") })
colnames(tt) <- new_names
rownames(tt) <- new_names
#
as.dist(tt)
hc <- hclust(as.dist(tt))
#
library(ggplot2)
library(ggdendro)
#
#convert cluster object to use with ggplot
dendr <- dendro_data(hc, type = "rectangle")

#your own labels (now rownames) are supplied in geom_text() and label=label
ggplot() +
  geom_segment(data = segment(dendr), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = label(dendr), aes(x = x, y = y, label = label, hjust = 0), size = 3) +
  coord_flip() + scale_y_reverse(expand = c(0.2, 0)) +
  ggtitle("Mash-based distance, set61_k27_s6k.tsv") +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.grid = element_blank())

par(mar = c(3, 1, 1, 5))
plot(as.dendrogram(hc), horiz = T)
