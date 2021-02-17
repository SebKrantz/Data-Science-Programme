############################################
# Data Science Training
# Homework Assignment of Course 1 Session 2
############################################

# Problem 1: Accessing Data from the Web ---------------------------------------------------------------------

# (a) using the download.file() function, write a small program that downloads
# spreadsheets you frequently require from the web (such as BoU or UBOS websites),
# and saves them to a folder on your local hard drive. Do this using the following steps:

# I. Open a browser and navigate to the website page where your spreadsheet is located.

# II. hold your mouse over the download link of your spreadsheet, and then right click and 'Copy Link Address'. 
# You should have copied a link that ends with the file you want to download. For example the direct link to 
# the interest rates excel sheet of BoU is:
# https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/InterestRates/Interest_rates.xlsx
# Open a new browser tab and paste the link there. Make sure you get the file directly.

# III. Create a folder where you want do save the downloaded file on your disc. Set the working directory there:
setwd("C:/Users/Sebastian Krantz/Documents/Data/BoU")

# IV. Download the file using the download.file function like this:
download.file("https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/InterestRates/Interest_rates.xlsx", 
              destfile = "Interest_rates.xlsx", mode = "wb")
# In destfile you specify the name of the file you are saving, which can be same as the original or different. 
# Note that if your working directory is not set, you can alternatively specify a full file path here, 
# e.g. destfile = "C:/Users/Sebastian Krantz/Documents/Data/BoU/Interest_rates.xlsx"
# If you are working on windows, specify mode = "wb" (shorthand for 'windows binary'). This is necessary for download.file to work well on windows.

# V. go to the folder on you computer ("C:/Users/Sebastian Krantz/Documents/Data/BoU" in my case), see if this downloaded spreadsheet is there.

# VI. Now write a program the downloads all spreadsheets you frequently require like this

# (b) The read.csv and read.delim functions allow you to directly access data from the web. 
# This downloads a csv file about monthly rainfall from the UBOS website
ug_rain <- read.csv("https://www.ubos.org/wp-content/uploads/statistics/Monthly_rainfall_for_selected_centres_(mm).csv", skip = 1)
str(ug_rain)
View(ug_rain)
# This code now processes this fiile into a 3d array and generates some graphics. Try to follow this code at each line,
# what is it doing? You may want to execute only one line at a time or only a part of a line to detect this. 

ug_rain[-1L] <- lapply(ug_rain[-1L], as.numeric)
names(ug_rain)[2L] <- "Year"
nc <- nchar(ug_rain$Station)
ug_rain$Station[nc > 10L | nc < 3L] <- NA
ug_rain$Station
ug_rain$Station <- trimws(ug_rain$Station)
ug_rain$Station
nms <- which(!is.na(ug_rain$Station))
diff(nms) # There are 9 observations per group
ind <- mapply(`:`, nms + 2L, nms + 8L, SIMPLIFY = FALSE) # we only take the years, not long term average
ug_rain[ind[[1]], ] # This gets the data block for the KAMPALA station
ustat <- unique(ug_rain$Station)
names(ind) <- ustat[!is.na(ustat)]
ind
ug_rain_array <- lapply(ind, function(i) {
  res <- as.matrix(ug_rain[i, month.abb])
  row.names(res) <- 2011:2017
  res
})
rm(nc, nms, ind) # removing temporary objects not needed for the analysis
str(ug_rain_array)
# This now creates the array
ug_rain_array <- simplify2array(ug_rain_array)
str(ug_rain_array)
# Now we so some analysis
med_rain <- apply(ug_rain_array, 2:3, median, na.rm = TRUE)

library(pheatmap)
library(RColorBrewer)
display.brewer.all()
brewer.pal(10, "Blues")

pheatmap(med_rain, cluster_rows = FALSE, cluster_cols = FALSE, 
         scale = "none", border_color = NA, display_numbers = TRUE, 
         number_color = "grey50", color = brewer.pal(5, "Blues"))

med_rain <- apply(ug_rain_array, c(1L, 3L), median, na.rm = TRUE)

pheatmap(med_rain, cluster_rows = FALSE, cluster_cols = FALSE, 
         scale = "none", border_color = NA, display_numbers = TRUE, 
         number_color = "grey50", color = brewer.pal(10, "Blues"))

med_rain <- apply(ug_rain_array, 1:2, median, na.rm = TRUE)

pheatmap(med_rain, cluster_rows = FALSE, cluster_cols = FALSE, 
         scale = "none", border_color = NA, display_numbers = TRUE, 
         number_color = "grey50", color = brewer.pal(10, "Blues"))

# here we generate a time-series of total annual precipitation per district
ts_district <- apply(ug_rain_array, c(1L, 3L), sum, na.rm = TRUE)
ts_district <- ts(ts_district, start = 2011, end = 2017)
ts_district
series_colors <- rainbow(ncol(ts_district))
ts.plot(ts_district, col = series_colors, main = "Annual Rainfall, 2011-2017", 
        ylab = "mm", xlab = "Year", x.nbre)
grid()
legend("bottomleft", colnames(ts_district), lty = 1, col = series_colors, ncol = 3, bty = "n") 

ts_all <- rowSums(ts_district) 
tsp(ts_all) <- tsp(ts_district)
class(ts_all) <- "ts"
time(ts_all)

plot(ts_all, main = "Average Annual Rainfall, 2011-2017", 
     ylab = "mm", xlab = "Year")
grid()
trendline <- lm(ts_all ~ time(ts_all))
summary(trendline)
abline(trendline, col = "red")
mtext(paste0("Average decrease: ",  round(abs(coef(trendline)[2L])), " mm per year"), padj = -0.5)
legend("topright", "Linear Trend", lty = 1, col = "red", bty = "n")


# We could also download the csv file directly and save it as an excel fule using 
ug_rain <- read.csv("https://www.ubos.org/wp-content/uploads/statistics/Monthly_rainfall_for_selected_centres_(mm).csv", skip = 1)
writexl::write_xlsx(ug_rain, "C:/Users/... /ug_rain.xlsx") # set the path to save the excel file somewhere

# (Optional) further exercises:
# - Open this ug_rain.xlsx file in Excel and clean it a bit, then try to import using readxl and simplify the preprocessing code above based on your cleaned file
#   Tipp: You may want to carry forward the rtation names in the excel sheet so as to create a panel dataset, and the use the
#   split() function to split the data by station, and then turn each dataset into a matrix lapply (as shown above), and finally simplify2array() to turn that list of matrices into an array.
# - Go to the UBOS Website, download a suitable csv file from UBOS website, and do some analysis on it. 



# (c) We can also create a wrapper function around read_excel that allows us to do this 

readxl_web <- function(x, ...) {
  temp <- tempfile()
  download.file(x, temp, mode = "wb")
  on.exit(file.remove(temp))
  readxl::read_excel(temp, ...)
}

BOU_I <- readxl_web("https://www.bou.or.ug/bou/bouwebsite/bouwebsitecontent/statistics/InterestRates/Interest_rates.xlsx", sheet = 2)

# Problem 2: Accessing data from an API package ----------------------------------------
library(wbstats)
library(tradestatistics)
# Nah, not for now...

# Using the R data viewer, find out which was the most exported and imported product of Uganda in 2018

# Problem 3: Analyzing Macroeconomic Data from BoU -------------------------------------

# Using the readxl_web function above, import the Monthly macroeconomic indicators dataset from the Bank of Uganda
# Tipp: You want to first download the file manually, then 

# Create a plot...

# Problem 4: Learning R Using Swirl ----------------------------------------------------
