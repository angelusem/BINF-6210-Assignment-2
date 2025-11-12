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
#Part 2: How does the relationship between BINs and location reveal patterns of species diversity and data coverage?

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
dfBOLD <- read_tsv(file = "../BINF-6210-Assignment-2/Canidae_BOLD_data.tsv")

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
### If you want to mean % of number of non-missing coordinates, the calculation should be different. 3577 is the number of total rows. So we can try this code instead:
percent_coord_usable2 <- 1258 / 3577 * 100
percent_coord_usable2
#==> It will returns 35% instead of 26%

# country/ocean data
sum(!is.na(dfBOLD$`country/ocean`))
percent_country_usable <- 2367 / (2367 + 3577) * 100
percent_country_usable
# Same comment as above
percent_country_usable2 <- 2367 / 3577 * 100
percent_country_usable2
#==> This returns 66% instead of 40%

# BIN data 
sum(!is.na(dfBOLD$bin_uri))
# Same as # of country/ocean but does that mean every country has a BIN?

dfBOLD.expl <- dfBOLD[, c("bin_uri", "country/ocean", "coord")]
# Finding: not the case.

### I think this code create a new data frame with only 3 columns (bin_uri, country/ocean, and coors). To answer your question if each of the 2367 country/ocean has a BIN, we can try these 4 following codes:

# Let's first change country/ocean to country_ocean
dfBOLD <- dfBOLD %>% 
  rename(country_ocean = `country/ocean`)

# 1) Make a data frame of only 2 columns (country_ocean and bin_uri), because we already knew that 2367 non-missinng values and our question is focused on 2 variables with same lenght.
dfBOLD.expl2 <- dfBOLD[, c("country_ocean", "bin_uri")]

#2) Let's check how many rows are complete for both two viriables
sum(complete.cases(dfBOLD.expl2))
#==> This returns 1356 different from 2367

# 3) We can go further make a clean data frame after removing rows with any missing values
dfBOLD.expl2.clean <- dfBOLD.expl2[complete.cases(dfBOLD.expl2), ]
dfBOLD.expl2.clean
#==> This returns a data frame of 1356 complete rows of 2 variables (bin_uri and country_ocean)
# Check whether any remaining missing values in this new dataset
any(is.na(dfBOLD.expl2.clean))
#==> Returns FALSE

# 4) We can finally check if all rows, in our clean data frame, are complete. all() will check if every row in the data frame is complete and return TRUE or FALSE. This is different from sum() which return the number (how many?).
all(complete.cases(dfBOLD.expl2.clean))
#==> TRUE
#==> To answer if the 2367 country_ocean have BIN is not. Only 1356 country_oceas have BINs.

# Before we make the next conclusion, we can check the number of complete cases for the 3 variables (bin_uri, country_ocean, and coors)
sum(complete.cases(dfBOLD.expl))
#==> 1160 complete rows of 3 variables (bin_uri, country_ocean, and coors)


# Since there are more data points for country/ocean than there is for coord, proceeding  with exploration based off bin_uri and country/ocean.

### The new lines of code I added demonstrated that coordinate variables can't be neglected because it contains 1160 rows with non-missing variables. So there is no big diffeferent when considering only country_ocean and bin_uri, which have 1356 complete observations.

####VISUALIZATION----

##2. Analysis aimed at answering Part 1 of Research Question: 
dfBOLD.sub <- dfBOLD %>%
  select(bin_uri, country_ocean) %>%
  filter(!is.na(country_ocean), !is.na(bin_uri)) # subsetted my columns of interest

#removed the entries that corresponded to "unrecoverable" in my dataset since they would not contribute to analyses
dfBOLD.sub
dfBOLD_no_unrec <- subset(dfBOLD.sub, country_ocean != "Unrecoverable") 
dfBOLD_no_unrec #check

# looking at count of BIN records per country
my.table <- table(dfBOLD_no_unrec$"country_ocean")
my.table
mean(my.table)
median(my.table)
max(my.table)
nrow(my.table)
#==> Mean = 19.3, meadian = 4.5, max = 227. These statistics indicate that many countries have a low number of BINs. So with no additoon tests, we can conclude that that these data are not normally distributed (mean != median).

#Plotting graphs where counts of BIN records is along the x axis and along the y-axis, we have the number of countries with that amount of records. This shows how data are strongly skewed with most countries having very low number of BINS, while very few contries have a larger number of BINs.

hist(x = my.table, 
     main = "BIN distribution across countries",
     xlab = "Number of BINs per Country", 
     ylab = "Number Countries", 
     breaks = c(seq(0, 300, by = 50)))#showing many countries have a count of Canidae records on BOLD in the bin 0-50 
### This histogram gives an important picture of BIN distribution across countries. We can edit labels to make it more informative. Let maybe call the x-axis number of BINs; y-axis, number of countries; and  
hist(x = my.table, 
     main = "BIN distribution across countries", 
     xlab = "Number of BINs per Country", 
     ylab = "Number Countries", 
     breaks = c(seq(0, 250, by = 10))) #showing many countries have a count of Canidae records on BOLD in the bin 0-10

# There seems to be many countries with a low count of BIN records (in the zero to 50 range)for Canidae-> sampling bias? warrants further investigation.

# Trying to visualize the counts of BOLD BIN records for each country more: 

sort(table(dfBOLD.sub$country_ocean), decreasing = TRUE)
sort(table(dfBOLD.sub$country_ocean), decreasing = TRUE)[1:10]
plot(sort(table(dfBOLD.sub$country_ocean), decreasing = TRUE)[1:5], 
     main = "BIN distribution across the top 5 countries", 
     xlab = "Countries", 
     ylab = "Number of BINs") #gives a line plot of the top 5 countries with the most BOLD records. I think that ggplot could be better for a clear viaualization.

# to visualize the top 10 countries, in a more appealing manner using ggplot. 
country_rec_counts <- dfBOLD.sub %>%
  count(country_ocean, sort = TRUE)

country_rec_counts %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(country_ocean, n), y = n)) +
  geom_col(fill = "hotpink") +
  coord_flip() +
  labs(
    title = "Top 10 Countries for Canidae Records in BOLD",
    x = "Country", y = "Number of BINs"
  )

##Addressing Part 2 of Research Question: Using the BINs as a molecular proxy for species to explore the completeness of sampling in the genus Canidae for the DNA barcoding campaign;

### This question seems to be different from the one asked in the introduction. Let's see if we can get responses to both questions.

# a) Rarefaction curve----
# b) Species accumulation curve----

# a) Examines sampling effort vs. observed diversity

# Grouping the data by BIN and counting the number of records in each BIN
dfCount.by.BIN <- dfBOLD.sub %>%
  group_by(bin_uri) %>%
  count(bin_uri)

# Getting the data into the community data object format. 
dfBINs.spread <- pivot_wider(data = dfCount.by.BIN, names_from = bin_uri, values_from = n)

# plotting a rarefaction curve by randomly sampling the individuals available in the total data set, at various levels of completeness (shown on the x-axis).
x <- rarecurve(dfBINs.spread, 
               main = "Rarefaction curve illustrating BIN richnes in Canidae", 
               xlab = "Individuals Barcoded", 
               ylab = "BIN Richness")
### The curve shows a steep initial increase in BIN richness with increasing individual sampling. Importantly, the continued upward trend suggests that additional sampling may reveal some unsampled BINs.

# b) "As we add countries, do we add a lot of new BINs? i.e. Do unique BINs tend to be added as we sample additional countries?"

dfBINs.by.country <- dfBOLD_no_unrec %>%
  group_by(country_ocean, bin_uri) %>%
  count(bin_uri)
# removing the rows where country is NA, and BIN is NA as such records do not contribute to answering our question.

dfBINs.by.country.na.rm <- dfBINs.by.country %>%
  filter(!is.na(country_ocean)) %>%
  filter(!is.na(bin_uri))

# Reshaping data to match desired data type and formatting once more
dfBINs.spread.by.country <- pivot_wider(data = dfBINs.by.country.na.rm, names_from = bin_uri, values_from = n)

# turn nas to 0 but are there any nas?
# You can first check if there is any missing value
any(is.na(dfBINs.spread.by.country)) #==> This returns TRUE

# Turns NAs to 0
dfBINs.spread.by.country[is.na(dfBINs.spread.by.country)] <- 0

# setting the rownames as country, rather than having country as a data column. 
dfBINs.spread.by.country <- dfBINs.spread.by.country %>%
  remove_rownames() %>%
  column_to_rownames(var = "country_ocean")

# Let's build the Species Acculmulation Curve
AccumCurve <- specaccum(dfBINs.spread.by.country)

# plotting the curve
plot(AccumCurve, xlab = "Countries Sampled", ylab = "BIN Richness", ci.type = "poly", col = "red", lwd = 2, ci.lty = 0, ci.col = "yellow")

AccumCurveboxpl <- specaccum(dfBINs.spread.by.country, "random")

boxplot(AccumCurveboxpl, col = "orange", add = TRUE, pch = "+")

##Estimating the percentage of Canidae diversity that has been sampled in the dataset

pool_estimate <- specpool(dfBINs.spread.by.country)
pool_estimate
#==> Chao estimate predicts 39 BINs, while observed BINs are 24. So, approximately 15 more BINs still need to be detected.

completeness <- pool_estimate$Species / pool_estimate$chao * 100
completeness
#==> This returns a moderate coverage ~62%. 

### Let's visualize these findings
# Create data to be visualized
df.BIN.richness <- data_frame(
  Metric = c("Observed BINs", "Estimated BINs"),
  Value = c(pool_estimate$Species, pool_estimate$chao)
)

# Let's make a plot
ggplot(df.BIN.richness, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", width = 0.6) +
  labs(
    title = "Observed vs. Estimated BIN richness in Canidae samples",
    y = "BIN richness",
    x = NULL
  )


####END