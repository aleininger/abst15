#

# Source of Shapefile
# http://www.arcgis.com/home/item.html?id=a5067fb3b0b74b188d7b650fa5c64b39

library(foreign)
library(gdata)
library(maptools)
library(rgeos)
library(rgdal)
library(ggplot2)
library(xtable)
library("lubridate")

setwd('/home/arndt/Dropbox/01 PhD/02 projects/abst15')

# URL <- c('http://www.bfs.admin.ch/bfs/portal/de/index/themen/17/03/blank/key/2015/012.Document.191327.xls', 'http://www.bfs.admin.ch/bfs/portal/de/index/themen/17/03/blank/key/2015/011.Document.191333.xls')

# http://www.bfs.admin.ch/bfs/portal/de/index/themen/17/03/blank/key/2015/012.html
# http://www.bfs.admin.ch/bfs/portal/de/index/themen/17/03/blank/key/2015/011.html


################################################################################
# Energie- statt Mehrwertsteuer
################################################################################

map <- readOGR(dsn = ".", "Kantone")

map <- fortify(map, region = 'Name')
map <- map[order(map$id),]
map$idnum <- as.numeric(factor(map$id))


################################################################################
# Energie- statt Mehrwertsteuer
################################################################################


# Volksinitiative "Energie- statt Mehrwertsteuer"

e <- read.csv('energieinitiative.csv')

e <- e[order(e$canton),]
e$id <- 1:nrow(e)

map.e <- merge(map, e, by.x = 'idnum', by.y = 'id', all.x = TRUE)

e.f <- ggplot(map.e, aes(long, lat, group = group, fill = yesperc)) + geom_polygon(colour='white') +
  coord_equal() + labs(x = NULL, y = NULL, fill = "% Ja") +
  ggtitle("Volksinitiative 'Energie- statt Mehrwertsteuer'") +
  scale_fill_gradient(low = "linen", high = "red2") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(lineheight=.8, size = 12))


################################################################################
# Steuerfreie Familienzulagen
################################################################################

# Volksinitiative "Familien stärken! Steuerfreie Kinder- und Ausbildungszulagen"


f <- read.csv('familieninitiative.csv')

f <- f[order(f$canton),]
f$id <- 1:nrow(f)

map.f <- merge(map, f, by.x = 'idnum', by.y = 'id', all.x = TRUE)

f.f <-
  ggplot(map.f, aes(long, lat, group = group, fill = yesperc)) + geom_polygon(colour='white') +
  coord_equal() + labs(x = NULL, y = NULL, fill = "% Ja") +
  ggtitle("Volksinitiative 'Familien stärken! Steuerfreie Kinder- und Ausbildungszulagen'") +
  scale_fill_gradient(low = "linen", high = "red2") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  theme(plot.title = element_text(lineheight=.8, size = 12))


################################################################################
# Liste der Volksabstimmungen
################################################################################

abst <- read.csv('win_datasheet.csv', sep = ';')

# abstbackup <- abst

abst$titel <- as.character(abst$tit)

abst$typ <- factor(abst$rechtsform, levels = unique(abst$rechtsform),
                   labels = c('Obligatorisches Referendum',
                              'Fakultatives Referendum',
                              'Volksinitiative',
                              'Gegenvoschlag'))

tab <- head(abst[order(abst$volkja.proz), c('datum', 'typ', 'titel', 'volkja.proz')], 9)

tab <- rbind(tab,
             data.frame(datum='08.03.2015',
                        typ = 'Volksinitiative',
                        titel = 'Volksinitiative «Energie- statt Mehrwertsteuer»',
                        volkja.proz = 8))

tab <- tab[order(tab$volkja.proz),]

################################################################################
# Abstimmungen seit 2010
################################################################################

abst$datum <- as.character(abst$datum)

abst$jahr <- as.numeric(substr(abst$datum, nchar(abst$datum)-3, nchar(abst$datum)))

abst2010 <- abst[which(abst$jahr >= 2010),]

table(abst2010$volk, abst2010$typ)

prop.table(table(abst2010$volk, abst2010$typ), margin = 2) * 100
