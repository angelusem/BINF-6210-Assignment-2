##***************************
## SOFTWARE TOOLS 2024 - BINF*6210: ASSIGNMENT_01 
##
## Maureen Angelus
##
## 2025-10-13
##
##***************************

####INTRODUCTION----

### Research Question: "How well-sampled is Canidae across countries, and what can the relationship between BINs and location tell us about species diversity and data coverage?”
## Broken down:
#Part 1: how well sampled is Canidae across countries
#Part 2: what can the relationship between BINs and location tell us about species diversity and data coverage?

##packages used----
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
library(viridis)
theme_set(theme_light())
library("vegan")
library(dplyr)

####AQUISITION OF DATA-----

# Obtain the Canidae data download from BOLD, in tsv format
dfBOLD <- read_tsv(file = "https://portal.boldsystems.org/api/documents/eAErSaywSkvMzcyptHJOzMtMSUy1zsnMzSxJTQEAhwsKDA==/download?format=tsv")

# written to disk
write_tsv(dfBOLD, "Canidae_BOLD_data.tsv")
#or from file submitted
getwd()
dfBOLD <- read_tsv(file = "../data/Canidae_BOLD_data.tsv")

### 1. Exploratory analysis
# Quick look at my dataset:
names(dfBOLD)
summary(dfBOLD)
length(dfBOLD)
# Since I am interested in the relationship between location metadata and BIN data: I am exploring three columns -> "coord", "bin_uri" and "country/ocean"

#coordinate data
names(dfBOLD)
head(dfBOLD$coord) 
sum(!is.na(dfBOLD$coord))
length(dfBOLD$coord)
table(is.na(dfBOLD$coord))
percent_coord_usable <- 1258 / (1258 + 3577) * 100
percent_coord_usable
# country/ocean data
sum(!is.na(dfBOLD$`country/ocean`))
percent_country_usable <- 2367 / (2367 + 3577) * 100
percent_country_usable
# BIN data 
sum(!is.na(dfBOLD$bin_uri))
# Same as # of country/ocean but does that mean every country has a BIN?
dfBOLD.expl <- dfBOLD[, c("bin_uri", "country/ocean", "coord")]
# Finding: not the case.


# Since there are more data points for country/ocean than there is for coord, proceeding  with exploration based off bin_uri and country/ocean.

####VISUALIZATION----

##2. Analysis aimed at answering Part 1 of Research Question: 
dfBOLD.sub <- dfBOLD %>%
  select(bin_uri, `country/ocean`) %>%
  filter(!is.na(`country/ocean`), !is.na(bin_uri)) # subsetted my columns of interest

dfBOLD.sub
dfBOLD_no_unrec <- subset(dfBOLD.sub, `country/ocean` != "Unrecoverable") #removed the entries that corresponded to "unrecoverable" in my dataset since they would not contribute to analyses
dfBOLD_no_unrec #check

# looking at count of BIN records per country
my.table <- table(dfBOLD_no_unrec$"country/ocean")
my.table
mean(my.table)
median(my.table)
max(my.table)
nrow(my.table)

#Plotting graphs where counts of records is along the x axis and along the y-axis, we have the frequency (i.e. number of countries) with that amount of records.

hist(x = my.table, xlab = "Count of BOLD Records per Country", ylab = "Frequency (No. Countries", breaks = c(seq(0, 300, by = 50)))#showing many countries have a count of Canidae records on BOLD in the bin 0-50 
hist(x = my.table, xlab = "Count of BOLD Records per Country", ylab = "Frequency (No. Countries", breaks = c(seq(0, 250, by = 10))) #showing many countries have a count of Canidae records on BOLD in the bin 0-10

#there seems to be many countries with a low count of records (in the zero to 50 range)for Canidae-> sampling bias? warrants further investigation.

#trying to visualize the counts of BOLD records for each country more: 

sort(table(dfBOLD.sub$"country/ocean"), decreasing = TRUE)
sort(table(dfBOLD.sub$"country/ocean"), decreasing = TRUE)[1:10]
plot(sort(table(dfBOLD.sub$"country/ocean"), decreasing = TRUE)[1:5]) #gives a line plot of the top 5 countries with the most BOLD records

# to visualize the top 10 countries, in a more appealing manner
country_rec_counts <- dfBOLD.sub %>%
  count(`country/ocean`, sort = TRUE)

country_rec_counts %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(`country/ocean`, n), y = n)) +
  geom_col(fill = "hotpink") +
  coord_flip() +
  labs(
    title = "Top 10 Countries for Canidae Records in BOLD",
    x = "Country", y = "Record count"
  )

##Addressing Part 2 of Research Question: Using the BINs as a molecular proxy for species to explore the completeness of sampling in the genus Canidae for the DNA barcoding campaign;
# a.) rarefaction curve----
#b.) species accumulation curve----

# a.) examines sampling effort vs. observed diversity
# Grouping the data by BIN and counting the number of records in each BIN
dfCount.by.BIN <- dfBOLD.sub %>%
  group_by(bin_uri) %>%
  count(bin_uri)
# getting the data into the community data object format
dfBINs.spread <- pivot_wider(data = dfCount.by.BIN, names_from = bin_uri, values_from = n)
# plotting a rarefaction curve by randomly sampling the individuals available in the total data set, at various levels of completeness (shown on the x-axis).
x <- rarecurve(dfBINs.spread, xlab = "Individuals Barcoded", ylab = "BIN Richness")

# b.) "As we add countries, do we add a lot of new BINs? i.e. Do unique BINs tend to be added as we sample additional countries?"

dfBINs.by.country <- dfBOLD_no_unrec %>%
  rename(country_ocean = `country/ocean`) |> # also renaming country/ocean for ease of use
  group_by(country_ocean, bin_uri) %>%
  count(bin_uri)
# removing the rows where country is NA, and BIN is NA as such records do not contribute to answering our question.
dfBINs.by.country.na.rm <- dfBINs.by.country %>%
  filter(!is.na(country_ocean)) %>%
  filter(!is.na(bin_uri))
# Reshaping data to match desired data type and formatting once more
dfBINs.spread.by.country <- pivot_wider(data = dfBINs.by.country.na.rm, names_from = bin_uri, values_from = n)
# turn nas to 0 but are there any nas?
dfBINs.spread.by.country[is.na(dfBINs.spread.by.country)] <- 0

# setting the rownames as country, rather than having country as a data column. 
dfBINs.spread.by.country <- dfBINs.spread.by.country %>%
  remove_rownames() %>%
  column_to_rownames(var = "country_ocean")
# plotting curve
AccumCurve <- specaccum(dfBINs.spread.by.country)

plot(AccumCurve, xlab = "Countries Sampled", ylab = "BIN Richness", ci.type = "poly", col = "red", lwd = 2, ci.lty = 0, ci.col = "yellow")

AccumCurveboxpl <- specaccum(dfBINs.spread.by.country, "random")

boxplot(AccumCurveboxpl, col = "orange", add = TRUE, pch = "+")

##Estimating the percentage of Canidae diversity that has been sampled in the dataset

pool_estimate <- specpool(dfBINs.spread.by.country)
pool_estimate
completeness <- pool_estimate$Species / pool_estimate$chao * 100
completeness
####END