#############################################################
# *************** R BASED DATA SCIENCE TRAINING *************
# Course 2: Advanced Data Manipulation and Visualization in R
# -----------------------------------------------------------
# Prepared by: Sebastian Krantz, ODI Fellow, MEPD, MoFPED
# Permanent Contact: sebastian.krantz@graduateinstitute.ch
#############################################################

# Course Aim: To familiarize participants with state-of-the art R packages for 
#             statistical computing, data manipulation and visualization

# Today:
# (1) Brief Recap of Base Graphics (material of C1_S2)
# (2) Data Visualization with ggplot2


## Advanced graphics with ggplot2 -----------------------------------
library(ggplot2)
library(magrittr)
library(collapse)
library(data.table)

# Quick (q) plot:
View(mtcars)
qplot(mpg, data = mtcars)
qplot(mpg, wt, data = mtcars)
qplot(mpg, wt, data = mtcars, colour = factor(cyl))
qplot(mpg, wt, data = mtcars, size = cyl)
qplot(mpg, wt, data = mtcars, shape = factor(cyl))
qplot(mpg, wt, data = mtcars, facets = vs ~ am)
qplot(mpg, wt, data = mtcars, geom = c("point", "smooth"))
qplot(mpg, wt, data = mtcars, geom = c("point", "smooth"), method = "lm", colour = factor(cyl))
qplot(mpg, wt, data = mtcars, geom = c("point", "smooth"), method = "lm", 
      colour = factor(cyl), facets = vs ~ am)

# A neat application:
setDT(mtcars)
dat <- melt(mtcars)
qplot(value, data = dat, fill = variable) + 
  facet_wrap(~ variable, scales = "free") + 
  guides(fill = FALSE)

# Same thing but more advanced:
histplot <- function(data, max.bins = 60L) {
  
  oldopts <- options(warn = -1L)
  on.exit(options(oldopts))
  # Removing Character Variables:
  char_vars(data) <- NULL
  # Put summary statistics in the variable names:
  names(data) <- qsu(data)[, 1:3] %>% round(1L) %>% qDF("Name") %>% {
                 do.call(sprintf, c(list("%s (N: %s Mean: %s, SD: %s)"), .)) }
  # For each variable set number of Bins to the number of distinct values, if the number of distinct values is smaller than 60. otherwise, number of bins is 60
  mybins <- fNdistinct(data)
  mybins[mybins > max.bins] <- max.bins
  # Reshape the data to long form:
  dat <- melt(qDT(data))
  # make custom geom with the appropriate breaks for each histogram:
  hists <- Map(function(x, b) geom_histogram(data = x, bins = b, colour = "white"),
               rsplit(dat, ~ variable, keep.by = TRUE), mybins)
  # Render Plot:
  ggplot(dat, aes(x = value, fill = variable)) + hists +
         facet_wrap(~ variable, scales = "free") + 
         guides(fill = FALSE) +
         labs(x = NULL, x = NULL) +
         theme(axis.text = element_text(face = "bold", size = 12L),
               strip.text = element_text(size = 12L)) 
}
histplot(mtcars)

GGDC10S %>% ftransformv(6:16, `/`, SUM) %>% histplot

##### Now on to some real Applications:

# (1) HDI Trends -------------------------------------------------------------------

HDI <- haven::read_dta("data/plotdata/HDI Trends.dta")
names(HDI)
p <- ggplot(HDI, aes(x = year, y = HDI, color = regions)) +
     scale_colour_discrete(name = "Regions") + geom_smooth(se = FALSE)
p

# THEMES

# Standard themes: http://ggplot2.tidyverse.org/reference/ggtheme.html
p + theme_minimal(base_size = 16)

# ggthemes package: https://cran.r-project.org/web/packages/ggthemes/vignettes/ggthemes.html or https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
library(ggthemes)
help(package = "ggthemes")
# https://yutannihilation.github.io/allYourFigureAreBelongToUs/ggthemes/
# Also covers other visualization packages...
# https://yutannihilation.github.io/allYourFigureAreBelongToUs/
p + theme_stata() + scale_color_stata()
p + theme_economist(base_size = 12, base_family = "serif") + scale_color_economist()
p + theme_economist_white() + scale_color_economist()
library(extrafont)
p + theme_economist(base_family = "ITC Officina Sans") +
  scale_colour_economist()
p + theme_wsj(base_size = 18, color = "gray")
p + theme_hc() + scale_colour_hc()
p + theme_igray(base_size = 16) + scale_colour_tableau()
p + theme_hc(base_size = 16, bgcolor = "darkunica")
p + theme_solarized_2(base_size = 20, light = FALSE)

# saving the plot:
dev.copy(pdf,"figures/HDI Trends.pdf", width = 11.35, height = 6.6)
dev.off()

# COLORS : http://www.sthda.com/english/wiki/colors-in-r
#  https://github.com/EmilHvitfeldt/r-color-palettes !!!
install.packages("paletteer")
# ggthemes theme color palettes
p + scale_colour_tableau()

# R-Brewer Palettes:
library(RColorBrewer)
display.brewer.all()
p + theme_minimal(base_size = 16) + scale_color_brewer(palette="YlOrRd")
p + theme_minimal(base_size = 16) + scale_color_brewer(palette="Set1")
p + theme_minimal(base_size = 16) + scale_color_brewer(palette="Dark2")

# Viridis Palettes: (these 4 palettes are really great!) https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html
library(viridis)
p + theme_minimal(base_size = 16) + scale_color_viridis(discrete = TRUE, option = "A")
p + theme_minimal(base_size = 16) + scale_color_viridis(discrete = TRUE, option = "B")
p + theme_minimal(base_size = 16) + scale_color_viridis(discrete = TRUE, option = "C")
p + theme_minimal(base_size = 16) + scale_color_viridis(discrete = TRUE, option = "D")

# Color names:
colors()
View(matrix(colors(), ncol = 10L))
demo("colors") # Show all named colors and more

# Another representation:
d = data.frame(c = colors(), 
               y = seq(0L, length(colors())-1L) %% 66L, 
               x = seq(0L, length(colors())-1L) %/% 66L)
ggplot() +
  scale_x_continuous(name = NULL, breaks = NULL, expand = c(0, 0)) +
  scale_y_continuous(name = NULL, breaks = NULL, expand = c(0, 0)) +
  scale_fill_identity() +
  geom_rect(data = d, mapping = aes(xmin = x, xmax = x + 1, ymin = y, ymax = y + 1), fill = "white") +
  geom_rect(data = d, mapping = aes(xmin = x + 0.05, xmax = x + 0.95, ymin = y + 0.5, ymax = y + 1, fill = c)) +
  geom_text(data = d, mapping = aes(x = x + 0.5, y = y + 0.5, label = c), colour = "black", hjust = 0.5, vjust = 1, size = 3)

dev.copy(pdf, "figures/R colors.pdf", width = 20, height = 13.5)
dev.off()

# Manual scaling with HEX6 colours:
# Goolge: "html color picker"
p + scale_color_manual(values = c("#000099", "#0000CC", "#0000FF", "#3366CC", 
                                  "#3399CC", "#33CCFF", "#660000", "#990000", 
                                  "#CC0000", "#FF0000", "#FF3300", "#FF6600",
                                  "#FF9900", "#FFCC00"))

# FONTS: http://www.cookbook-r.com/Graphs/Fonts/

p + theme_minimal(base_size = 16, base_family = "sans")  # Helvetica
p + theme_minimal(base_size = 16, base_family = "serif") # I think Time New Roman

# GEOMS: SEE GGPLOT2 Cheatsheet
# Mainly: geom_point(), geom_bar(), geom_histogram(), geom_smooth()
# Different smoothing methods (e.g. lm, loess, gam, polynomail regression): https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/

# REACTIVITY:

# Adding Interactivity with Plotly:
library(plotly)
myplot <- p + theme_solarized_2(base_size = 20, light = FALSE)
ggplotly(myplot)

ggplotly2 <- function(p, dynticks = FALSE, hoverinfo = "x+y+name", hoverformat = ",.1f",
                      hovermode = "compare", size = 15,
                      legend = list(xanchor = "center", yanchor = "top", 
                                    x = 0.5, y = -0.3, orientation = 'h'), 
                      hoverlabel = list(bordercolor = "transparent", font = list(color = "#FFF"))) { # y = -0.2
  ggplotly(p, tooltip = "none", dynamicTicks = dynticks) %>% 
    style(hoverinfo = hoverinfo) %>%
    layout(
      title = list(font = list(family = "Arial", color = '#333333')), 
      legend = legend, 
      yaxis = list(tickformat = hoverformat, hoverformat = hoverformat, 
                   tickfont = list(family = "Arial", size = size)), 
      xaxis = list(hoverformat = "", tickfont = list(family = "Arial", size = size)), 
      hovermode = hovermode, 
      hoverlabel = hoverlabel,
      margin = list(l = 0, r = 10, b = 0, t = 50, pad = 5)
    )
}

ggplotly2(myplot + guides(colour = FALSE), hoverinfo = "y+name", 
          hoverformat = ".3f")

ggplotly2(myplot, hoverinfo = "y+name", hovermode = "x unified", 
          hoverformat = ".3f", hoverlabel = list(bgcolor = 'rgba(255, 255, 255, 0.8)', bordercolor = "transparent"))


# # Adding Animation with gganimate: https://github.com/dgrtwo/gganimate
# For Some reason however it didn't work properly for me!
# devtools::install_github("dgrtwo/gganimate")
# library("gganimate")
#
# library(installr)
# install.ImageMagick()
# library(gapminder)
# library(animation)
# p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent, frame = year)) +
#   geom_point() +
#   scale_x_log10()
# gganimate(p)




# (2) World Growth and Human Development --------------------------------------------------------------

dataset <- read.csv("data/plotdata/ltgraphGEOdetailed.csv")
# Basic plot
p1 <- qplot(GMRealPcGDP, NIHDI, colour = GEOdetailed, geom = c("point", "path"), 
            data = dataset, main = "Wealth and Human Development",
            xlab = "GDP per Capita, PPP$ inflation-adjusted", ylab = "Non-Income HDI")
# Add text
p2 <- geom_text(aes(label = quinade), size = 3, nudge_y = 0.015, 
                check_overlap = TRUE, show.legend = FALSE, data = dataset)
# Add Lowess fit
p3 <- geom_smooth(aes(x = GMRealPcGDP, y = NIHDI), data = dataset, 
                  method = "loess", span = 1, inherit.aes = FALSE, size = 0.4, 
                  se = TRUE, linetype = "dashed", alpha = 0.15)
# Customize axis breaks
p4 <- c(scale_x_continuous(breaks = seq(0, 60000, 5000)),
        scale_y_continuous(breaks = seq(0, 1, 0.125)))
# Customize appearance
p5.1 <- theme_minimal() # theme_solarized_2(light = FALSE)
p5.2 <- theme(axis.text.x = element_text(face = "bold", size = 10, margin = margin(0, 0 ,5, 0)),
              axis.text.y = element_text(face = "bold", size = 10, margin = margin(0, 0, 0, 5)),
              axis.title.x = element_text(size = 14, margin = margin(7, 0, 7, 0)),
              axis.title.y = element_text(size = 14, margin = margin(0, 7, 0, 7)),
              title = element_text(size = 16, margin = margin(10, 0, 0, 0)),
              plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size = 12))
# Make Lgend 1 column
p5.3 <- guides(col = guide_legend(ncol = 1, byrow = TRUE, title = "Regional Aggregate"))


p1
p1 + p2
p1 + p2 + p3
p1 + p2 + p3 + geom_smooth(method = "loess", span = 1, size = 0.4, alpha = 0.15) # this we don't want
p1 + p2 + p3 + p4
p1 + p2 + p3 + p4 + p5.1 + p5.2 + p5.3 

# We can make out own rainbow scale
rainbow_colours <- discrete_scale(c("colour", "fill"), "gradientn", rainbow, na.value = "grey50")
p1 + p2 + p3 + p4 + p5.1 + p5.2 + p5.3 + rainbow_colours

# Save plot:
dev.copy(pdf,"figures/World Wealth and Human Development.pdf", width = 16, height = 9)
dev.off()



# (3) Governance Data, Uganda and the World ----------------------------------------------------------------

library(viridis)
GOVI <- haven::read_dta("data/plotdata/Governance, Uganda + Regional Aggregates, 2010-2016 Average.dta")
names(GOVI) <- vlabels(GOVI)
settransform(GOVI, Region = paste0(Region, "     "))
GOVI %<>% qDT %>% melt(id.vars = "Region", variable.name = "Variable")

pretty_plot2 <- theme_minimal() +
  theme(axis.text.y = element_text(face = "bold", size = 10),
        axis.title.y = element_text(size = 14, margin = margin(r = 10, l = 5)),
        title = element_text(size = 14),
        plot.title = element_text(hjust = 0.5, margin=margin(b = 15, t = 5)),
        legend.text = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 10),
        legend.title = element_blank(),
        legend.position = "top")

# Bar Charts:
ggplot(GOVI, aes(x = Region, y = value, fill = Region)) + 
  geom_bar(stat = "identity", alpha = 0.8) + pretty_plot2 +  # + geom_col()
  scale_y_continuous(breaks = scales::pretty_breaks(n = 7), expand = c(0.2, 0)) + 
  scale_x_discrete(breaks = NULL) +
  geom_text(inherit.aes = TRUE, mapping = aes(label = round(value, 1)), 
            vjust = ifelse(GOVI$value > 0, -0.3, 1.3), size = 3.5) + # 1.6, color="white" ifelse(GOVI$value>0,1.5,-0.8)
  facet_wrap( ~ Variable, scales = "free", ncol = 3) + 
  labs(y = "Index Value", x = NULL) + 
  scale_fill_viridis(discrete = TRUE, option = "B")

dev.copy(pdf, "figures/Uganda Governance Statistics.pdf", width = 11.69, height = 8.27)
dev.off()

# Ok, now that you know the result, Lets start simple:
View(GOVI)
mp <- ggplot(GOVI, aes(x = Region, y = value, fill = Region)) + 
      geom_bar(stat = "identity", alpha = 0.8) +
      facet_wrap( ~ Variable, scales = "free", ncol = 3)
mp
# adding labels and a viridis color palette:
mp <- mp + labs(y = "Index Value", x = NULL) + scale_fill_viridis(discrete = TRUE, option = "B")
mp
# Adding text:
mp + geom_text(inherit.aes = TRUE, mapping = aes(label = round(value, 1)))
# This does not look good
# properly situating text:
mp <- mp + geom_text(inherit.aes = TRUE, mapping = aes(label = round(value, 1)), 
                     vjust = ifelse(GOVI$value > 0, -0.3, 1.3), size = 3.5)
mp
# Adjusting y-scale and setting x-ticks to 0:
mp <- mp + scale_y_continuous(breaks = scales::pretty_breaks(n = 7), 
                              expand = c(0.2, 0)) + scale_x_discrete(breaks = NULL)
mp
# Adding custom theme styling:
mp <- mp + pretty_plot2
mp
# and we are done!!




# (4) Trends in secondary education in Uganda. Source: World Bank EdStats Database -----------------------------------------

EDUC <- read_excel("data/plotdata/Data_Extract_From_Education_Statistics_-_All_Indicators.xlsx")
View(EDUC)
# Throw away information at the bottom of excel sheet and coercing Time to integer
EDUC %<>% ss(1:377) %>% ftransform(Time = as.integer(Time)) 

# This part selects the income aggregates
names(EDUC)[2L] <- "Aggregate"
unique(EDUC$Aggregate)
EDUCGI <- fsubset(EDUC, Aggregate %in% unique(Aggregate)[c(1:2, 10:13)])
unique(EDUCGI$Aggregate)
settransform(EDUCGI, Aggregate = paste0(Aggregate, "     ")) # we need space at the end of each label for the legend to look nice
table(EDUCGI[[2L]])

# Some cleaning on variable names
names(EDUCGI) %<>% 
  sub(", both sexes", "", .) %>%
  sub("secondary general education", "secondary education", .) %>%
  sub("gender parity index", "GPI", .) %>%
  sub(" to the last grade of lower secondary education", "", .) %>%
  sub(" \\(GPI\\)", "", .) %>% 
  sub(", all grades", "", .) %>%
  sub("Rate of out", "Out", .) %>%
  sub("Effective transition rate from primary to lower secondary education",
      "Effective transition rate, primary to lower secondary", .)



# Plot 1:
vr <- c("Time", "Aggregate",
        "Gross enrolment ratio, secondary (%)",
        "Gross enrolment ratio, secondary, GPI",
        "Lower secondary completion rate (%)",
        "Lower secondary completion rate, GPI",
        "Percentage of repeaters in lower secondary education (%)",
        "Percentage of repeaters in lower secondary education, GPI",
        "School life expectancy, primary and secondary (years)",
        "School life expectancy, primary and secondary, GPI")

data <- EDUCGI %>% get_vars(vr) %>% qDT %>% 
        melt(id.vars = c("Time", "Aggregate"), variable.name = "Variable")

pretty_plot <- theme_minimal() +
  theme(axis.text = element_text(size = 10),
        axis.title.x = element_text(size = 14, margin = margin(t = 10, b = 5)),
        axis.title.y = element_text(size = 14, margin = margin(r = 10, l = 5)),
        title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.position = "top")

ggplot(data, aes(x = as.integer(Time), y = value, color = Aggregate)) +
  # geom_point(alpha = 0.6) +
  geom_smooth(span = 1, size = 0.5, se = FALSE) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(1990, 2015)) +
  facet_wrap( ~ Variable, scales = "free", ncol = 2) + 
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Set1") + 
  pretty_plot

dev.copy(pdf, "figures/Uganda Education Trends 1.pdf", width = 8.27, height = 11.69)
dev.off()


# Plot 2:

vr <- c("Time", "Aggregate",
        "Out-of-school youth of upper secondary school age (%)",
        "Effective transition rate, primary to lower secondary (%)",
        "Cumulative drop-out rate (%)",
        "School life expectancy, secondary (years)",
        "School life expectancy, secondary, GPI")

data <- EDUCGI %>% get_vars(vr) %>% qDT %>% 
        melt(id.vars = c("Time", "Aggregate"), variable.name = "Variable")

# Feeding in 2 extra series:
A <- haven::read_dta("data/plotdata/2 Extra attainment indicators from WDI.dta")
names(A) <- vlabels(A)
names(A)[1L] <- "Time"
names(A)[3L] <- "Aggregate" 
A %<>% get_vars(-c(2L, 4:7)) %>% qDT %>%
       melt(id.vars = c("Time", "Aggregate"), variable.name = "Variable") %>%
       ftransform(Aggregate = paste0(Aggregate, "     "))

data <- rbind(A, data)

ggplot(data, aes(x = as.integer(Time), y = value, color = Aggregate)) +
  # geom_point(alpha = 0.6) +
  geom_smooth(span = 1, size = 0.5, se = FALSE) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6), limits = c(1990, 2015)) +
  facet_wrap( ~ Variable, scales = "free", ncol = 2) + 
  labs(x = NULL, y = NULL) +
  scale_color_brewer(palette = "Dark2") + 
  pretty_plot


dev.copy(pdf, "figures/Uganda Education Trends 2.pdf", width = 8.27, height = 9.09)
dev.off()

# (5) Area Plots: Tanzania Output, employment shares and labour productivities. Source: Groningen 10 - Sector Growth ------------------------------------------------

data(GGDC10S)

## World Regions Structural Change Plot

GGDC10S %>% 
  ftransformv(6:15, `/`, SUM) %>% 
  replace_outliers(0, NA, "min") %>%
  ftransform(Variable = fifelse(Variable == "VA", 
                                "Value Added Share", 
                                "Employment Share")) %>% 
  fgroup_by(Variable, Region, Year) %>% 
  fselect(AGR:OTH) %>% 
  fmedian %>% qDT %>% 
  melt(1:3, variable.name = "Sector", na.rm = TRUE) %>% 
  
ggplot(aes(x = Year, y = value, fill = Sector)) +
  geom_area(position = "fill", alpha = 0.9) + 
  theme_linedraw(base_size = 14) + 
  facet_grid(Variable ~ Region, scales = "free_x") +
  scale_fill_manual(values = sub("#00FF66FF", "#00CC66", rainbow(10))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7), expand = c(0, 0))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10), expand = c(0, 0),
                     labels = scales::percent) +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 315, hjust = 0, margin = ggplot2::margin(t = 0)),
        strip.background = element_rect(colour = "grey30", fill = "grey30"))

# A function to plot the structural change of an arbitrary country

plotGGDC <- function(ctry) {
  # Select and subset
  fsubset(GGDC10S, Country == ctry, Variable, Year, AGR:SUM) %>%
    # Convert to shares and replace negative values with NA
    ftransform(fselect(., AGR:OTH) %>% 
                 lapply(`*`, 1 / SUM) %>% 
                 replace_outliers(0, NA, "min")) %>%
    # Remove totals column and make proper variable labels
    ftransform(Variable = recode_char(Variable, 
                                      VA = "Value Added Share",
                                      EMP = "Employment Share"),
               SUM = NULL) %>% 
    # Fast conversion to data.table
    qDT %>% 
    # data.table's melt function
    melt(1:2, variable.name = "Sector", na.rm = TRUE) %>%
    # ggplot with some scales provided by the 'scales' package
    ggplot(aes(x = Year, y = value, fill = Sector)) +
    geom_area(position = "fill", alpha = 0.9) + labs(x = NULL, y = NULL) +
    theme_linedraw(base_size = 14L) + facet_wrap( ~ Variable) +
    scale_fill_manual(values = sub("#00FF66", "#00CC66", rainbow(10L))) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7L), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10L), expand = c(0, 0),
                       labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 315, hjust = 0, margin = ggplot2::margin(t = 0)),
          strip.background = element_rect(colour = "grey20", fill = "grey20"),
          strip.text = element_text(face = "bold"))
}

plotGGDC("TZA")

# Now reshaping and computing labour productivity as well

data <- GGDC10S %>% qDT %>% 
  fselect(-Regioncode, -Region) %>%
  melt(1:3, variable.name = "Sector") %>%
  dcast(Country + Sector + Year ~ Variable, data = .) %>% 
  ftransform(LP = VA / EMP) %>% 
  melt(1:3, variable.name = "Variable", na.rm = TRUE) 

# Shares Plot
data[Country == "TZA" & Sector != "SUM"] %>%
  ggplot(aes(x = Year, y = value, fill = Sector)) +
    geom_area(position = "fill", alpha = 0.9) + labs(x = NULL, y = NULL) +
    theme_linedraw(base_size = 14L) + facet_wrap( ~ Variable) +
    scale_fill_manual(values = sub("#00FF66", "#00CC66", rainbow(10L))) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 7L), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10L), expand = c(0, 0),
                       labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 315, hjust = 0, margin = ggplot2::margin(t = 0)),
          strip.background = element_rect(colour = "grey20", fill = "grey20"),
          strip.text = element_text(face = "bold"))

data[Country == "TZA" & Sector != "SUM"] %>%
  ggplot(aes(x = Year, y = value, colour = Sector)) +
  geom_xspline(spline_shape = 1, size = 1) + 
  facet_wrap( ~ Variable, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_bw(base_size = 14) + guides(color = FALSE) +
  scale_color_manual(values = sub("#00FF66", "#00CC66", rainbow(10L))) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 7), expand = c(0, 0))+
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x)), expand = c(0, 0)) +
  theme(axis.text.x = element_text(angle = 315, colour = "black", hjust = 0, margin=ggplot2::margin(t = 0)),
        axis.text.y = element_text(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        strip.background = element_rect(colour = "grey20", fill = "grey20"),
        strip.text = element_text(face = "bold", colour = "white")) +
  annotation_logticks(sides = "lr")



# (6) An Intro to GEO-Computation ------------------------------------------------

# Clear All Data objects but keep functions:
rm(list = setdiff(ls(), lsf.str()))

# Loading Shapefile and Data: -----------------------------------------------------------------------------
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)     # for fortifying shapefiles

# First read in the shapefile, using the path to the shapefile and the shapefile name minus the extension as arguments
shapefile <- readOGR("data/plotdata/Uganda Districts Shapefile", "Districts_2014_UBOS")

# to change to correct projection:
shapefile <- spTransform(shapefile, CRS("+proj=longlat +datum=WGS84"))

# Next the shapefile has to be converted to a dataframe for use in ggplot2
shapefile_df <- fortify(shapefile)

# Now the shapefile can be plotted as either a geom_path or a geom_polygon. Paths handle clipping better. Polygons can be filled. You need the aesthetics long, lat, and group.
map <- ggplot()+
  geom_polygon(data = shapefile_df,
               aes(x = long, y = lat, group = group))+
  theme_void() +
  coord_map() # Important for representation to have correct proportions.
print(map)


# Loading Schools data:
library(readxl)
CIM <- read_xlsx("data/plotdata/Schools.xlsx")

# Quick Map Plot (ggmap) -------------------------------------------------------------------------

library(ggmap)
library(RColorBrewer)
library(viridis)

# With Stamen toner map and ggmap:
qmplot(x = gps1longitude, y = gps1latitude, data = CIM, maptype = "toner-lite",
       color = gen_nstudents, legend = "top") + # guides(color=FALSE) +
  geom_polygon(aes(x = long, y = lat, group = id), data = shapefile, 
               color ="white", fill ="black", alpha = .1, size = .2) +
  scale_color_viridis(trans = "log", option = "C", name = "Number of Students:   ", 
                      breaks = c(50, 100, 500, 1000, 2000, 4000), 
                      guide = guide_legend(nrow = 1)) +
  theme_void() + theme(legend.position = "top", legend.margin = ggplot2::margin(b = -0.2, unit = "cm"))

dev.copy(pdf, "figures/Schools by Size Map.pdf", width = 5.4, height = 5.8)
dev.off()


# Map plot with ggmap and Google Maps Static API -----------------------------------------------------------------
# https://stackoverflow.com/questions/36367335/ggmap-removing-country-names-from-googles-terrain-map
# https://developers.google.com/maps/documentation/maps-static/styling#features

long <- mean(range(CIM$gps1longitude, na.rm = TRUE))
lat <- mean(range(CIM$gps1latitude, na.rm = TRUE))

CIM$i <- rep(1, nrow(CIM))
p11 <- ggmap(get_googlemap(c(long, lat), zoom = 7, color = "color", 
                           style = 'feature:administrative.country|element:geometry.stroke|visibility:off&style=feature:all|element:labels|visibility:simplified&style=feature:administrative.country|element:labels|visibility:off&style=feature:road|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off'), padding = 0.01) + # feature:all|element:labels|visibility:off
  geom_polygon(aes(x = long, y = lat, group = id), 
               data = shapefile, color = "grey35", fill = "white", alpha = .5, size = .05) +
  coord_fixed(xlim = c(29.5, 35.1), ylim = c(-1.5, 4.2)) +
  stat_density2d(aes(x = gps1longitude, y = gps1latitude, fill = ..level.., alpha = 0.8, size = 3), 
                 geom = "polygon", data = CIM) +
  guides(alpha = FALSE, size = FALSE) +
  scale_fill_viridis(trans = "log", option = "C", name = "School Density:   ", breaks = c(0.1, 0.3, 0.5)) +
  theme_void() + theme(legend.position = "top", legend.title = element_text(size = 12), 
                       legend.margin = ggplot2::margin(b = -0.2, unit = "cm"))

p12 <- ggmap(get_googlemap(c(long, lat), zoom = 7, color = "color", 
                           style = 'feature:all|element:labels|visibility:simplified&style=feature:administrative.country|element:labels|visibility:off&style=feature:road|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off'), padding = 0.01) +
  geom_point(aes(x = gps1longitude, y = gps1latitude, color = gen_nstudents), data = CIM) +
  coord_fixed(xlim = c(29.5, 35.1), ylim = c(-1.5, 4.2)) +
  scale_color_viridis(trans = "log", option = "C", name = "Number of Students:   ",
                      breaks = c(50, 100, 500, 1000, 2000, 4000), guide = guide_legend(nrow = 1)) +
  theme_void() + theme(legend.position = "top", 
                       legend.title = element_text(size = 12), 
                       legend.margin = ggplot2::margin(b = -0.1, unit = "cm"))

library(grid)
grid.newpage()
grid.draw(cbind(ggplotGrob(p12), ggplotGrob(p11), size = "last"))

dev.copy(pdf, "figures/Schools by Size Map FINAL.pdf", width = 9.6, height = 5.4)
dev.off()

# Population and School Density Choropleth: ----------------------------------------------------------

library(sp)        # spatial package
library(rgdal)     # R wrapper around GDAL/OGR
library(ggplot2)   # for general plotting
library(ggmap)     # for fortifying shapefiles

data2 <- data.frame(id = as.character(0:111),
                    pop = as.numeric(shapefile@data$POP2014 / 1000),
                    district_string = as.character(shapefile@data$DNAME2014))
View(shapefile_df)
View(shapefile@data)

shapefile_df %<>% qDT %>% merge(data2, all.x = TRUE)
# Aggregate by District:
D <- data.frame(district_string = CIM$district_string , num = 1) %>% 
     ftransform(district_string = as.character(district_string)) %>%
     fgroup_by(district_string) %>% fsum

shapefile_df %<>% merge(D, all.x = TRUE, by = "district_string") %>%
                  ftransform(num = replace_NA(num, 0))
table(shapefile_df$num)
head(shapefile_df)


# Plots:
p1 <- ggplot() + # Population
  geom_polygon(data = shapefile_df, 
               aes(fill = pop, x = long, y = lat, group = group), size = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", name = "Population 2014 (thousands)", 
                     breaks = c(50, 100, 200, 500, 1000, 1500, 2000), 
                     guide = guide_legend(keyheight = unit(3, units = "mm"), 
                                          keywidth = unit(12, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'top', nrow = 1)) +
  theme(legend.position = c(0.6, 0.05)) + coord_map()

p2 <- ggplot() + # Schools
  geom_polygon(data = shapefile_df, 
               aes(fill = num, x = long, y = lat, group = group), size = 0, alpha = 0.9) +
  theme_void() +
  scale_fill_viridis(trans = "log", name = "Schools", 
                     breaks=c(1, 2, 5, 10, 20, 50), 
                     guide = guide_legend(keyheight = unit(3, units = "mm"),
                                          keywidth = unit(12, units = "mm"), 
                                          label.position = "bottom", 
                                          title.position = 'top', nrow = 1)) +
  theme(legend.position = c(0.6, 0.05)) + coord_map()

# Combine the Plots:
library(grid)
grid.newpage()
grid.draw(cbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

dev.copy(pdf, "figures/Schools and Population Choropleth.pdf", width = 10, height = 5)
dev.off()


# For more on Interactive maps: see
library(leaflet)
# https://rstudio.github.io/leaflet/
# Cheat Sheet: https://ugoproto.github.io/ugo_r_doc/leaflet-cheat-sheet.pdf





## Resources for R Graphics-----------------------------------------------

## General:

# Shiny App for learning GGplot2:
library(ggplotgui)
ggplot_shiny()

# Courses / Books: 
# https://clauswilke.com/dataviz/
# https://github.com/uo-datasci-specialization/c2-dataviz-2021
# https://wilkelab.org/SDS375/
# https://yutannihilation.github.io/allYourFigureAreBelongToUs

# ggplot2 Cheat sheet (Provided)
# ggplot2 Reference Page: http://ggplot2.tidyverse.org/reference/index.html
# ggplot2 cultom heme styling reference: http://ggplot2.tidyverse.org/reference/theme.html
# The R-Graph Gallery: Short reference for all kinds of graphs + code: https://www.r-graph-gallery.com/
# Smoothers: https://stats.idre.ucla.edu/r/faq/how-can-i-explore-different-smooths-in-ggplot2/
# More on Smoothers and Plotting options: https://ben-williams.github.io/updated_ggplot_figures.html
# Great hands on courses on data vidualization. See the two courses on ggplot2: https://www.datacamp.com/courses/topic:data_visualization
# https://uc-r.github.io/ggplot

## Specific ggplot2 topics:

# axes: ticks and labels: http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
# barplots: http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
# Pie charts: http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
# more on colors: http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually#default-colors

## Inspiration:

# top ggplot plots: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html
# R graph gallery: https://www.r-graph-gallery.com/

# for more on interactive plots, see: https://www.htmlwidgets.org/index.html
# Also look up the plotly library, read the provided book on developing data products in R, and check out a package called d3r, which provides a wrapper for the powerful D3 interactive web graphics language: https://d3js.org/

