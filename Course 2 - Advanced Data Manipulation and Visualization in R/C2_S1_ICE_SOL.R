############################################
# Data Science Training
# Solution to the In-Class Exercises of
# Course 2 Session 1
############################################

setwd("Course 2 - Advanced Data Manipulation and Visualization in R")

#****************************
### In-Class Exercise 1 -----
#****************************

# Read an aggregated version of a social accounting matrix, with random errors introduced before distributing it in the framework of this course. 
SAM <- readxl::read_xlsx("data/Ag_SAM_modified.xlsx") 
# Lets create a matrix from the SAM:
SAM_mat <- as.matrix(SAM[-(1:2)])
rownames(SAM_mat) <- SAM$Index

library(matrixStats)

# (a) using rowSums2(), compute the total intermediate use of each commodity
res <- rowSums2(SAM_mat, 
                rows = startsWith(rownames(SAM_mat), "C_"),
                cols = startsWith(colnames(SAM_mat), "A_"))

names(res) <- grep("^C_", rownames(SAM_mat), value = TRUE)
res

# (b) repeat step (a) usig rowSums() where you subset the matrix beforehand
rowSums(SAM_mat[startsWith(rownames(SAM_mat), "C_"),
                startsWith(colnames(SAM_mat), "A_")])

# (c) compute the taxes and transport margins paid on commodities
# Either: (more efficient for large matrices)
res <- colSums2(SAM_mat, 
                rows = grep("TAX|TM", rownames(SAM_mat)),
                cols = startsWith(colnames(SAM_mat), "C_"))

names(res) <- grep("^C_", colnames(SAM_mat), value = TRUE)
res
# Or: (less efficient but easier)
colSums(SAM_mat[grep("TAX|TM", rownames(SAM_mat)),
                startsWith(colnames(SAM_mat), "C_")])


# (d) compute the total expenditure of different households on all commodities
# Either:
res <- colSums2(SAM_mat, 
                rows = startsWith(rownames(SAM_mat), "C_"),
                cols = startsWith(colnames(SAM_mat), "HH_"))

names(res) <- grep("^HH_", colnames(SAM_mat), value = TRUE)
res
# Or:
colSums(SAM_mat[startsWith(rownames(SAM_mat), "C_"),
                startsWith(colnames(SAM_mat), "HH_")])



#****************************
### In-Class Exercise 2 -----
#****************************

# Rewrite these expressions using magrittr pipes:
library(magrittr)

# (a)
sapply(subset(transform(mtcars, mpg2 = mpg * 2),
              cyl > 4 & mpg > 20, vs:mpg2), median)

# Piped version:
mtcars %>%
  transform(mpg2 = mpg * 2) %>%
  subset(cyl > 4 & mpg > 20, vs:mpg2) %>%
  sapply(median) -> 
  res

res <- transform(mtcars, mpg2 = mpg * 2)
res <- subset(res, cyl > 4 & mpg > 20, vs:mpg2)
res <- sapply(res, median)

# (b)
str(rapply(list(airquality, mtcars), sum, how = "list"))

# Piped version:
 list(airquality, mtcars) %>% 
  rapply(sum, how = "list") %>%
  str


# (c)
do.call(rbind, tapply(iris$Sepal.Width, iris$Species, quantile)) 

# Piped version:
tapply(iris$Sepal.Width, iris$Species, quantile) %>% 
  do.call(rbind, .)



#****************************
### In-Class Exercise 3 -----
#****************************

# The collapse package comes with another dataset: wlddev 
library(collapse)
library(magrittr)
View(wlddev)
?wlddev

# In this exercise you will explore this dataset

# (a) using namlab(), see the names, variable labels and classes of variables in this data
namlab(wlddev, class = TRUE)

# (b) compute a detailed statistical summary of variables using descr(). 
descr(wlddev)

# (c) using qsu(), compute panel-data summary statistics of this data.
qsu(wlddev, pid = ~ iso3c, vlabels = TRUE)

# (d) Summarise these variables for EAC countries
EAC <- wlddev %>% fsubset(iso3c %in% c("UGA", "KEN", "TZA", "RWA", "BDI", "SSD"))
qsu(EAC, by = ~ country, cols = 9:12, vlabels = TRUE)

# (e) Compute the annualized 10-year growth rates of these variables
G(EAC, 10, by = ~ country, t = ~ year, cols = 9:12, power = 1/10)

# (f) Save the GDP, LIFE Expectancy and ODA for EAC variables in a matrix and plot it
psmat(EAC, ~ country, ~ year, cols = c(9:10, 12), 
      transpose = TRUE) %>% plot(legend = TRUE)

# (g) Repeat part (f) and plot the annualized growth rates computed in (e)
G(EAC, 10, by = ~ country, t = ~ year, cols = c(9:10, 12), power = 1/10) %>%
  psmat(~ country, ~ year) %>% plot(legend = TRUE)

# (h) We want to aggregate this data to provide regional statistics. 
# To better represent the populations in the different regions, we will do a population weighted aggregation.
# Since the data does not contain a population variable, we download it from the world bank and merge it to the data
install.packages("WDI")
wlddev %<>% merge(
  WDI::WDI(country = as.character(unique(wlddev$iso3c)), # API for the world bank development indicators:, see ?WDI::WDI. For more indicators see als install.packages("wbstats")
           indicator = "SP.POP.TOTL", extra = TRUE) %>% 
    frename(SP.POP.TOTL = POP) %>%
    fselect(iso3c, year, POP))
# Relabeling data
vlabels(wlddev) <- c(vlabels(collapse::wlddev), "Total population")
View(wlddev)
qsu(wlddev, vlabels = TRUE)

# Aggregate this data to the world region level using Population weights. See ?fmean
wldreg <- wlddev %>% fgroup_by(region, year) %>% fselect(PCGDP:POP) %>% fmean(POP)
head(wldreg)

# (i) transform this aggregated data from part (h) into a time series array and plot
wldreg %>% psmat( ~ region, ~ year) %>% plot(legend = TRUE)



#****************************
### In-Class Exercise 4 -----
#****************************

# In the data folder there is a STATA file with poverty estimates from the Uganda National Household Survey 2016/17

# (1) import this data using the haven library. Coerce all labeled variables to R factors by calling haven::as_factor on the imported dataset
library(haven)
UNHS_pov <- read_dta("data/UNHS_pov16.dta") %>% as_factor

# (2) describe the data, if possible utilize the weights 'finwalwgt' in the summary
namlab(UNHS_pov, class = TRUE)
qsu(UNHS_pov, w = ~finalwgt)
descr(UNHS_pov, w = UNHS_pov$finalwgt) # Weights only applied only to mean, SD, skewness and kurtosis

# (2) Summarize the variable 'fexp30' and 'welfare' by 'urban', and panel-decompose the variation by 'district', use the weights 'finalwgt'
qsu(UNHS_pov, fexp30 + welfare ~ urban, ~ district, ~ finalwgt)

# (3) Compute the median, 1st and 2rd quartile of 'welfare' by 'regurb'. 
# TIP: Use fsummarise and fnth, also utilize the weights 'finalwgt' 
welQ <- UNHS_pov %>% fgroup_by(regurb) %>% 
  fsummarise(Nobs = fnobs(welfare),
             Sumw = fsum(finalwgt),
             welfare_1Q = fnth(welfare, 0.25, finalwgt),
             welfare_med = fmedian(welfare, finalwgt), 
             welfare_3Q = fnth(welfare, 0.75, finalwgt))
welQ

# (4) # (e) Compute an identifier indicating whether a household is wealthier than the median household in the same 'regurb' group.
settransform(UNHS_pov, above_welfare_regurb = welfare > fmedian(welfare, regurb, finalwgt, "replace_fill"))
# another option if we want to utilize the statistics already computed
settransform(UNHS_pov, above_welfare_regurb = welfare > TRA(welfare, welQ$welfare_med, "replace", regurb))

# (5) Aggregate the data to the district level. Remove the columns 'hhid' and 'ea' beforehand,
#     utilize the weights 'finalwgt'. 

# Weighted aggregation by district, after removing household id and enumeration area
UNHS_pov %>% 
  fselect(-hhid, -ea) %>% 
  fgroup_by(district) %>% 
  collapg(fmedian, w = finalwgt) %>% 
  fdroplevels %>% 
  head


#****************************
### In-Class Exercise 5 -----
#****************************

library(data.table)
# Data Table also provides an efficient file reader for delimited data (CSV files)
?fread
# This reads CSV from the Bureau of Transporation Statistics for all the flights that departed from New York City airports in 2014 
flights <- fread("https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv")

# (a) Save this file in the data subfolder of this course under the name 'flights14.csv' using the function fwrite
?fwrite
fwrite(flights, "data/flights14.csv")

# (b) Work through the 'Introduction to data.table' vignette which is based on this data.
# make sure you get the same results and understand them.
vignette('datatable-intro')

# Bonus: Also work through the 'reference semantics' vignette
vignette('datatable-reference-semantics')


#****************************
### In-Class Exercise 6 -----
#****************************

# Join all the tables in this database together into one big table. 
# (Note: there are multiple ways to do it, any that combines all tables will suffice)

library(data.table)
library(collapse) # for frename()
library(magrittr) # for pipes
airlines <- as.data.table(nycflights13::airlines)
airports <- as.data.table(nycflights13::airports)
flights <- as.data.table(nycflights13::flights)
planes <- as.data.table(nycflights13::planes)
weather <- as.data.table(nycflights13::weather)

# Create a data.table that combines all tables in this database:
all <- flights %>% 
       merge(airlines %>% frename(name = "carrier name"), by = "carrier") %>% 
       merge(airports %>% frename(function(x) paste("origin", x), cols = -1L), 
             by.x = "origin", by.y = "faa") %>% # Joining to origin (could also join to destination)
       merge(planes %>% frename(function(x) paste("plane", x), cols = -1L), 
             by = "tailnum", all.x = TRUE) %>%  # Left join: Keeping all obs. in flights table
       merge(weather[, -(year:hour)] %>% 
             frename(function(x) paste("origin", x), cols = -c(1L, 11L)), 
             by = c("origin", "time_hour"), all.x = TRUE) # Left join: Keeping all obs. in flights table

# Look at it:
View(all)  
  

