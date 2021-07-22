##############################
# Uganda 2014 Census Map
##############################

suppressPackageStartupMessages({
library(shiny)
library(shinyBS)
library(collapse)
library(leaflet)
library(sf)
library(scales)
library(htmltools)
library(RColorBrewer)
})

# rm(list = ls())
load("app_DATA.RData")

idvars <- c("Region", "District",  "County",  "Subcounty", "Parish")

names(cens_vars_list)[1L] <- "Composite Indices"

agglabs <- setNames(idvars, paste0(idvars, " (", fndistinct(.subset(DATA, idvars)),")"))

genpopup <- function(data, colvar = "POP_M", oth = c("POP", "HDI"), 
                     idvars = c("Region", "District",  "County",  "Subcounty", "Parish")) {
  ids <- idvars[idvars %in% names(data)]
  if(colvar %!in% oth) oth <- c(oth, colvar)
  vars <- c(ids, oth) 
  lhs <- paste0("<br/><strong>", vars, ":</strong>") 
  lhs[1L] <- substr(lhs[1L], 6L, 10000L)
  pop <- c(as.vector(lhs, "list"), .subset(data, vars))
  if(!all(int <- vapply(pop[oth], is.integer, TRUE))) 
    pop[oth][!int] <- lapply(pop[oth][!int], round, 3L)
  o <- seq(1L, 2L*length(vars), 2L)
  o <- order(c(o, o + 1L))
  do.call(paste, pop[o]) 
}


ui <- source(file.path("geoUI.R"), local = TRUE)$value 

server <- function(input, output, session) {
  source(file.path("geoSERVER.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)

