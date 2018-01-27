## ----include=F-----------------------------------------------------------
knitr::knit_hooks$set(margin = function(before,options,envir) {
if(before) par(mgp=c(1,0.35,0),bty="n",plt=c(0,.99,.13,.99), mar = c(2,2,3,2), xpd = TRUE) else NULL })

knitr::opts_chunk$set(margin=T,prompt=T,comment="",collapse=T,cache=F, bty="n",
dev.args=list(pointsize=11),fig.height= 4,
fig.width=6.24725,fig.retina=2,fig.align="center")

## ---- eval = FALSE-------------------------------------------------------
#  devtools::install_github("choisy/gdpm")
#  devtools::install_github("choisy/poseid")
#  install.packages("animation")

## ----warning=FALSE, message=FALSE----------------------------------------
library(gdpm)
library(poseid)
library(animation)

## ---- warning=FALSE, message=FALSE---------------------------------------
# Important packages
library(dplyr) # for 'select', 'filter', 'mutate'....
library(gso) # for pop_size

## ------------------------------------------------------------------------
# Load the dataset from the gdpm package
dengue <- getid(dengue, from = 1995)
dim(dengue)
range(dengue$year)
head(dengue)

## ------------------------------------------------------------------------
?merge_prov
?pop_size

## ------------------------------------------------------------------------
pop <-  gso::pop_size
head(pop)

## ------------------------------------------------------------------------
pop <- merge_prov(pop, from = "1995-01-01")
head(pop)

## ------------------------------------------------------------------------
identical(unique(pop$province), unique(dengue$province))

## ------------------------------------------------------------------------
pop <- filter(pop, key == "total")
pop <- select(pop, province, year, population = value)
head(pop)

## ------------------------------------------------------------------------
pop <- mutate(pop, population = population * 1000)
head(pop)

## ---- warning=FALSE, message=FALSE---------------------------------------
# join the population dataframe to the dengue dataset and calculate the incidence rate
dengue <- left_join(dengue, pop, by = c("province", "year"))
dengue <- mutate(dengue, incidence_rate =  (incidence_dengue / population)*10000)
head(dengue)

## ---- eval = FALSE-------------------------------------------------------
#  ?breaks # information on the breaks function
#  ?classInt::classIntervals # information on the style used in the breaks function

## ------------------------------------------------------------------------
sample_dengue <- sample_frac(dengue, 0.5)

## ------------------------------------------------------------------------
quant <- breaks(sample_dengue, "incidence_rate", n = 6, style = "quantile", pal = rev(heat.colors(6)), distribution = TRUE)

## ------------------------------------------------------------------------
attr(quant, "breaks")

## ------------------------------------------------------------------------
fisher <- breaks(sample_dengue, "incidence_rate", n = 6, style = "fisher", pal = rev(heat.colors(6)), distribution = TRUE)

## ------------------------------------------------------------------------
attr(fisher, "breaks")

## ------------------------------------------------------------------------
# Create a consensus:
breaks_den <- c(0, 0.1, 1, 4, 8, 22, max(na.omit(dengue$incidence_rate)))
# Look at the distribution of the data:
dengue_breaks <- breaks(dengue, "incidence_rate", n = 6, style = "fixed", fixedBreaks = breaks_den, pal = rev(heat.colors(6)), distribution = TRUE) 

## ------------------------------------------------------------------------
# Create a palette
pal <- colorRampPalette(rev(heat.colors(6))) 
pal_breaks <- pal(6)[cut(na.omit(dengue_breaks$incidence_rate), 
                         breaks = attr(dengue_breaks, "breaks"))]
# Plot the distribution of the value
plot(na.omit(dengue_breaks$incidence_rate), xlab = "time", pch = 20, col = pal_breaks)

## ---- message=FALSE------------------------------------------------------
library(magrittr) # `%>%` 
library(gadmVN)   # for 'gadm'
map <- gadmVN::gadm(date = 1995, merge_hanoi = TRUE)

## ------------------------------------------------------------------------
?choromap

## ------------------------------------------------------------------------
filter(dengue, year == 2009, month == "January") %>% 
  select(province, contains("rate")) %>%             
  choromap(map, col = rev(heat.colors(6)), fixedBreaks = breaks_den) %>% 
  legend2(legend = ., col = attr(., "colors"), col_na = "grey", n_round = 2)

## ------------------------------------------------------------------------
?animation::saveGIF

## ---- message=FALSE------------------------------------------------------
months <- month.name # vector of month name

saveGIF({
    monthly_loop <- for(i in seq(months)){ # loop printing a map for each months
    filter(dengue, year == 2009, month == months[i]) %>% 
      select(province, contains("rate")) %>% 
      choromap(map, col = rev(heat.colors(6)), fixedBreaks = breaks_den) %>% 
      legend2(legend = ., col = attr(., "colors"), col_na = "grey", n_round = 1)
    text(x = par("usr")[2], y = par("usr")[4], labels = paste0(months[i], " ", 2009), adj = c(1, 1))
    }
}, movie.name = "animation_monthly.gif", interval = 0.5, ani.width = 580)

## ---- message=FALSE------------------------------------------------------
saveGIF({
  yearly_loop <- for(j in seq(range(dengue$year)[1], range(dengue$year)[2])){ # loop for each year
    monthly_loop <- for(i in seq(months)){ # monthly loop
    filter(dengue, year == j, month == months[i]) %>% 
      select(province, contains("rate")) %>% 
      choromap(map, col = rev(heat.colors(6)), fixedBreaks = breaks_den) %>% 
      legend2(legend = ., col = attr(., "colors"), col_na = "grey", n_round = 2)
    text(x = par("usr")[2], y = par("usr")[4], labels = paste0(months[i], " ", j), adj = c(1, 1))
    }
  }
}, movie.name = "animation_dengue.gif", interval = 0.1, ani.width = 580)

