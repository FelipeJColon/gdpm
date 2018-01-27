## ----include=F-----------------------------------------------------------
knitr::knit_hooks$set(margin = function(before,options,envir) {
if(before) par(mgp = c(1.5, 0.5, 0), bty = "n",plt = c(.105, .97, .13, .97)) else NULL })

knitr::opts_chunk$set(margin = T,prompt = T,comment = "", collapse = T, cache = F,
dev.args = list(pointsize = 11), fig.height = 3.5,
fig.width = 4.24725, fig.retina = 2, fig.align = "center")

## ----eval=F--------------------------------------------------------------
#  devtools::install_github("choisy/gdpm")

## ------------------------------------------------------------------------
library(gdpm)

## ----eval=F--------------------------------------------------------------
#  write.table(gdpm::getid(ili), "ili.csv", quote = F, sep = ",", row.names = F)
#  write.table(gdpm::diseases, "diseases.csv", quote = F, sep = ",", row.names = F)

## ------------------------------------------------------------------------
chickenpox <-  getid(chickenpox)
head(chickenpox)

## ------------------------------------------------------------------------
str(chickenpox)

## ------------------------------------------------------------------------
levels(chickenpox$month)

## ------------------------------------------------------------------------
head(chickenpox)
tail(chickenpox)

## ------------------------------------------------------------------------
diseases

## ------------------------------------------------------------------------
str(diseases)

## ---- eval = FALSE-------------------------------------------------------
#  ?getid

## ------------------------------------------------------------------------
merged_chickenpox <- getid(chickenpox) 
head(merged_chickenpox)
# By default, the time range selected will be: "1980 - 2015", time range of the data (see diseases)
range(merged_chickenpox$year)
# The province will be merged accordingly to the history of Vietnam to return the data ordered by the forty provinces of 1980 (except "Ha Son Binh which is merged with Ha Noi")
unique(merged_chickenpox$province)

## ------------------------------------------------------------------------
range(getid(dysenteria, malaria, rubella)$year)
# equivalent to :
#range(getid_("dysenteria", "malaria", "rubella")$year)

## ------------------------------------------------------------------------
# if shortest = FALSE
# equivalent : getid(dysenteria, malaria, rubella)$year
range(getid(malaria, dysenteria, rubella, shortest = FALSE)$year)
# if shortest = TRUE
range(getid(malaria, dysenteria, rubella, shortest = TRUE)$year)


## ------------------------------------------------------------------------
merged_diseases <- getid(malaria, dysenteria, rubella, from = "1980-01-01", to = "2006-12-31") 
# equivalent to :
#merged_diseases <- getid_("malaria", "dysenteria", "rubella", from = "1980-01-01", to = "2006-12-31")
#merged_diseases <- getid_("malaria", "dysenteria", "rubella", from = "1980", to = "2006")
#merged_diseases <- getid_("malaria", "dysenteria", "rubella", from = 1980, to = 2006)
#merged_diseases <- getid(malaria, dysenteria, rubella, from = "1980", to = "2006")
#merged_diseases <- getid(malaria, dysenteria, rubella, from = 1980, to = 2006)
# time range of the output
range(merged_diseases$year)
# Visualisation
head(merged_diseases)
tail(merged_diseases)

## ------------------------------------------------------------------------
range(getid(malaria, dysenteria, rubella, from = 2004, shortest = TRUE)$year)
range(getid(malaria, dysenteria, rubella, to = 2006, shortest = FALSE)$year)

## ------------------------------------------------------------------------
df1 <- getid(dysenteria, malaria, rubella, from = 1990)
length(unique(df1$province))
df2 <- getid(dysenteria, hepatitis, malaria, rubella, from = 1990)
length(unique(df2$province))

