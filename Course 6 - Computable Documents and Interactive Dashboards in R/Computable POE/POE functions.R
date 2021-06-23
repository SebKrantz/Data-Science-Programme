library(collapse)
library(magrittr)
library(data.table)
library(plotly)
library(lubridate)
library(ggplot2)
library(scales)
library(ggrepel)
library(d3.format) # remotes::install_github("dreamRs/d3.format")

# library(ggalt) geom_xspline

# Set plot theme
pretty_plot <- function(text.size = 12, 
                        x.labels.right = TRUE, 
                        plot.margin = margin(t = 5.5, r = 35, b = 5.5, l = 15), 
                        legend.position = "bottom") {
  theme_minimal() +
  theme(axis.text = element_text(size = text.size),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # axis.line.x = element_line(colour = "grey20"),
        plot.title = element_text(hjust = 0.5, colour = "grey20"),
        # axis.ticks = element_line(),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = if(x.labels.right) 315 else 45, 
                                   hjust = 1 - x.labels.right, 
                                   margin = margin(t = 0)),
        legend.position = legend.position,
        legend.spacing.x = unit(0.5, "cm"), # "lines"
        # panel.grid = element_blank(),
        strip.text = element_text(face = "bold"),
        plot.margin = plot.margin,
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(size = text.size, colour = "grey30"))
}


crbpal <- gradient_n_pal(recode_char(rainbow(26L),
                                     "#00FF14" = "#00CC66",  # Dark Green
                                     "#FF003B" = "#FF0000")) # Red / Magenta

rainbow_colours <- discrete_scale(c("colour", "fill"), "gradientn",
                                  function(x) crbpal(seq(0, 1, 1/x)), na.value = "grey50")
# rainbow_colours <- discrete_scale(c("colour", "fill"), "hue", rainbow, na.value = "grey50")

read_poe_oth <- function(path = "POE-other-data.xlsx") {
  x <- readxl::read_xlsx(path)
  lab <- c("Month", x[[2L]])
  x <- transpose(get_vars(x, -2L), keep.names = "Date", make.names = 1)
  x[[1L]] <- as.Date(as.double(x[[1L]]), origin = "1899-12-30")
  settransformv(x, -1L, as.numeric) 
  vlabels(x) <- lab
  return(qDT(x))
}

ggplotly2 <- function(p, 
                      dynticks = FALSE, 
                      hoverinfo = "x+y+name", 
                      hoverformat = ",.1f",
                      hovermode = "compare", 
                      size = 15,
                      legend = list(xanchor = "center", yanchor = "top", 
                                    x = 0.5, y = -0.3, orientation = 'h'), 
                      hoverlabel = list(bordercolor = "transparent", 
                                        font = list(color = "#FFF")),
                      origtext = FALSE) { # y = -0.2
  if(origtext) hoverinfo <- sub("y", "text", hoverinfo)
  ipl <- ggplotly(p, tooltip = if(origtext) "y" else "none", dynamicTicks = dynticks) %>% 
    config(displayModeBar = FALSE) %>%
    style(hoverinfo = hoverinfo) %>%
    layout(
      title = list(font = list(family = "Arial", color = '#333333')), 
      legend = legend, 
      yaxis = list(tickformat = hoverformat, 
                   hoverformat = hoverformat, 
                   tickfont = list(family = "Arial", size = size),
                   fixedrange = TRUE), 
      xaxis = list(hoverformat = "", 
                   tickfont = list(family = "Arial", size = size),
                   fixedrange = TRUE), 
      hovermode = hovermode, 
      hoverlabel = hoverlabel,
      margin = list(l = 0, r = 10, b = 0, t = 50, pad = 5)
    )
  if(origtext) {
    ipl$x$data <- lapply(ipl$x$data, function(x) {
      if(any(nzchar(x$text))) 
      x$text <- d3_format(hoverformat)(as.numeric(sub("value: ", "", x$text))) 
      x
    })
  }
  return(ipl)
}

prep_data <- function(data, day, series, labels) {
  if(!is.null(day)) lubridate::day(data$Date) <- as.integer(day)
  if(!is.null(series)) data <- get_vars(data, if(is.numeric(series)) c(1L, series + 1L) else c("Date", series))
  if(!is.null(labels)) names(data)[-1L] <- if(isTRUE(labels)) vlabels(data)[-1L] else labels
  melt(qDT(data), "Date", na.rm = TRUE)
}


# Here defining 3 plotmethods for use in Rmarkdown
lineplot <- function(data, 
                     title = NULL, 
                     series = NULL, 
                     labels = NULL, 
                     colours = "YlOrRd", 
                     line.shapes = FALSE, # gives different line shapes
                     hline = NULL, 
                     day = NULL, 
                     text.size = 11, 
                     legend.position = "bottom", 
                     legend.ncol = 2, 
                     x.labels.right = TRUE,
                     y.nbreaks = 5, 
                     y.labels.format = formatC, 
                     point.labels = TRUE, 
                     point.labels.lastm1 = TRUE,
                     point.labels.size = text.size,
                     point.labels.format = NULL, 
                     point.labels.direction = "both", 
                     point.labels.seed = NA, 
                     plot.margin = margin(t = 5.5, r = 35, b = 5.5, l = 15),
                     interactive = FALSE, 
                     dynamic.ticks = FALSE,
                     hoverinfo = "x+y+name", 
                     hoverformat = ",.1f", 
                     hovermode = "compare",
                     hoverlabel = list(bordercolor = "transparent", 
                                       font = list(color = "#FFF")),
                     iact.axis.text.size = 15, 
                     iact.legend.position = list(xanchor = "center", yanchor = "top", 
                                                 x = 0.5, y = -0.3, orientation = 'h'), ...) {
  
  oldopts <- options(warn = -1L)
  on.exit(options(oldopts))
  
  data <- prep_data(data, day, series, labels)
  
  if(point.labels && !interactive) {
    i <- seq_row(data)
    last <- flast(i, data$variable, use.g.names = FALSE)
    ind <- c(ffirst(i, data$variable, use.g.names = FALSE), 
             if(point.labels.lastm1) last - 1L else NULL, last)
    data$labvalue <- NA_character_
    data$labvalue[ind] <- if(is.null(point.labels.format)) y.labels.format(data$value[ind]) else 
                                                       point.labels.format(data$value[ind])
  }  
  
  my_gg <- 
    ggplot(data, aes(x = Date, y = value, colour = variable)) + 
    (if(line.shapes) geom_line(aes(linetype = variable), size = if(interactive) 0.5 else 1) else 
                     geom_line(size = if(interactive) 0.5 else 1)) + 
    (if(!is.null(hline)) geom_hline(yintercept = hline, size = if(interactive) 0.25 else 0.5) else NULL) + 
    scale_x_date(breaks = pretty_breaks(n = min(fNdistinct(data$Date), 12L)), expand = c(if(point.labels) 0.05 else 0.03, 0)) +  
    scale_y_continuous(breaks = pretty_breaks(n = y.nbreaks), labels = y.labels.format, ...) +  
    (if(point.labels && !interactive)
      geom_text_repel(aes(label = labvalue), show.legend = FALSE, point.padding = 0.5, min.segment.length = 0.2, 
                      # https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
                      direction = point.labels.direction, seed = point.labels.seed, size = 0.35 * point.labels.size) else NULL) +
    switch(tolower(colours), rainbow = rainbow_colours, scale_color_brewer(palette = colours)) + 
    labs(title = title) + 
    guides(colour = guide_legend(ncol = legend.ncol)) +
    pretty_plot(text.size, x.labels.right, plot.margin, legend.position)
  
  if(interactive) {
    return(ggplotly2(my_gg, dynamic.ticks, hoverinfo, hoverformat, hovermode, 
              iact.axis.text.size, iact.legend.position, hoverlabel))
  } else return(my_gg)
}

barplot <- function(data, 
                    transpose = FALSE, 
                    position = "stack",
                    title = NULL, 
                    series = NULL, 
                    labels = NULL, 
                    colours = "YlOrRd", 
                    hline = NULL,
                    day = NULL,
                    text.size = 11,
                    legend.position = "bottom", 
                    legend.ncol = 2, 
                    x.labels.right = TRUE,
                    y.nbreaks = 5, 
                    y.labels.format = formatC, 
                    point.labels = TRUE, 
                    point.labels.lastm1 = TRUE,
                    point.labels.size = text.size,
                    point.labels.format = NULL, 
                    plot.margin = margin(t = 5.5, r = 35, b = 5.5, l = 15),
                    interactive = FALSE, 
                    dynamic.ticks = FALSE,
                    hoverinfo = "x+y+name", 
                    hoverformat = ",.1f", 
                    hovermode = "compare",
                    hoverlabel = list(bordercolor = "transparent", 
                                      font = list(color = "#FFF")),
                    iact.axis.text.size = 15, 
                    iact.legend.position = list(xanchor = "center", yanchor = "top", 
                                                x = 0.5, y = -0.3, orientation = 'h'), ...) {
  
  oldopts <- options(warn = -1L)
  on.exit(options(oldopts))
  
  data <- prep_data(data, day, series, labels)
  
  if(transpose) {
    settransform(data, Date = paste(month.abb[month(Date)], year(Date)) %>% factor(unique(.)))
    setrename(data, Date = variable, variable = Date)
  }
  
  if(point.labels && !interactive) {
    if(!transpose) {
      i <- seq_row(data)
      last <- flast(i, data$variable, use.g.names = FALSE)
      ind <- c(ffirst(i, data$variable, use.g.names = FALSE), 
               if(point.labels.lastm1) last - 1L else NULL, last)
      data$labvalue <- NA_character_
      data$labvalue[ind] <- if(is.null(point.labels.format)) y.labels.format(data$value[ind]) else 
                                                         point.labels.format(data$value[ind])
    } else {
      data$labvalue <- if(is.null(point.labels.format)) y.labels.format(data$value) else 
                                                    point.labels.format(data$value)
    }
  }  
  
  my_gg <- 
    ggplot(data, aes(x = Date, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position, alpha = 0.8) +
    (if(!is.null(hline)) geom_hline(yintercept = hline, size = if(interactive) 0.25 else 0.5) else NULL) + 
    (if(transpose) scale_x_discrete() else 
      scale_x_date(breaks = pretty_breaks(n = min(fNdistinct(data$Date), 12L)), expand = c(0.03, 0))) +  
    scale_y_continuous(breaks = pretty_breaks(n = y.nbreaks), labels = y.labels.format, ...) + 
    (if(point.labels && !interactive) geom_text(aes(label = labvalue), position = position, size = 0.35 * point.labels.size,
                                                vjust = fifelse(data$value > 0, -0.3, 1.3), colour = "grey30", inherit.aes = TRUE) else NULL) +
    switch(tolower(colours), rainbow = rainbow_colours, scale_fill_brewer(palette = colours)) +
    labs(title = title) + 
    guides(fill = guide_legend(ncol = legend.ncol)) + 
    pretty_plot(text.size, x.labels.right, plot.margin, legend.position)
  
  if(interactive) {
    ipl <- ggplotly2(my_gg, dynamic.ticks, hoverinfo, hoverformat, hovermode, 
                     iact.axis.text.size, iact.legend.position, hoverlabel, origtext = TRUE)
    # Previous solution: Works well for positive values (in this case set origtext = FALSE above)
    # if(is.character(position) && position %in% c("stack", "fill")) {
    #   ipl$x$data <- lapply(ipl$x$data, function(x) {
    #     x$text <- d3_format(hoverformat)(x$y) # point.labels.format(x$y)
    #     x$hoverinfo <- sub("y", "text", x$hoverinfo)
    #     x
    #   })
    # }
    return(ipl)
  } else return(my_gg)
}

areaplot <- function(data, 
                     title = NULL, 
                     series = NULL, 
                     labels = NULL, 
                     colours = "YlOrRd", 
                     hline = NULL, 
                     day = NULL, 
                     text.size = 12,
                     legend.position = "bottom", 
                     legend.ncol = 2, 
                     x.labels.right = TRUE,
                     y.nbreaks = 5, 
                     y.labels.format = formatC,
                     position = "stack", 
                     plot.margin = margin(t = 5.5, r = 35, b = 5.5, l = 15),
                     interactive = FALSE, 
                     dynamic.ticks = FALSE,
                     hoverinfo = "x+y+name", 
                     hoverformat = ",.1f", 
                     hovermode = "compare",
                     hoverlabel = list(bordercolor = "transparent", 
                                       font = list(color = "#FFF")),
                     iact.axis.text.size = 15, 
                     iact.legend.position = list(xanchor = "center", yanchor = "top", 
                                                 x = 0.5, y = -0.3, orientation = 'h'), ...) {
  
  oldopts <- options(warn = -1L)
  on.exit(options(oldopts))
  
  data <- prep_data(data, day, series, labels)
  
  my_gg <- 
    ggplot(data, aes(x = Date, y = value, fill = variable)) +
    geom_area(position = position, alpha = 0.8) +
    (if(!is.null(hline)) geom_hline(yintercept = hline, size = if(interactive) 0.25 else 0.5) else NULL) + 
    scale_x_date(breaks = pretty_breaks(n = min(fNdistinct(data$Date), 12L)), expand = c(0.03, 0)) +
    scale_y_continuous(breaks = pretty_breaks(n = y.nbreaks), labels = y.labels.format, ...) + 
    switch(tolower(colours), rainbow = rainbow_colours, scale_fill_brewer(palette = colours)) +
    labs(title = title) + 
    guides(fill = guide_legend(ncol = legend.ncol)) + 
    pretty_plot(text.size, x.labels.right, plot.margin, legend.position)
  
  if(interactive) {
    ipl <- ggplotly2(my_gg, dynamic.ticks, hoverinfo, hoverformat, hovermode, 
                     iact.axis.text.size, iact.legend.position, hoverlabel, origtext = TRUE)
    return(ipl)
  } else return(my_gg)
}