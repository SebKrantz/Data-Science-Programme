# Small functions to help process data on the server side 
selectvars <- function(x, v, exc = FALSE) {
  if(exc) v <- -ckmatch(v, names(x))
  get_vars(x, v)
}

vlabdf <- function(X) {
  res <- lapply(unattrib(X), attr, "label")
  res[vapply(res, is.null, TRUE)] <- "-"
  unlist(res, use.names = FALSE)
}

vlabclsrc <- function(X) {
  ntd <- function(x) if(length(x)) x else "-"
  ff <- function(x) c(paste0(paste(class(x), collapse = " "), " (", typeof(x),")"), ntd(attr(x, "label")), ntd(attr(x, "source")))
  qDF(setColnames(t(vapply(X, ff, character(3))), 
                  c("Class (Storage Mode)", "Label", "Source")), "Variable")
}

namlabHTML <- function(df) {
  if(is.list(df) && length(df) > 1L) names(df) <- paste0("<b>", names(df), "</b> <br> <small>", vlabdf(df), "</small>")
  df
}

mysummary <- function(df, labels = TRUE) {
  ord <- as.integer(c(3,1,4,2,5,6,7,8,9))
  sumf <- function(x) if(is.numeric(x)) c(fndistinct.default(x), fmedian.default(x), qsu.default(x, higher = TRUE))[ord] else
    c(fnobs.default(x), fndistinct.default(x), rep(NA_real_, 7L))
  res <- t(vapply(df, sumf, numeric(9)))
  colnames(res) <- c("N","Distinct","Mean","Median","SD","Min","Max","Skewness","Kurtosis")
  res <- qDF(print.qsu(res, return = TRUE), "Variable")
  if(labels) res$Label <- vlabels(df)
  return(res)
}

renameSTATA <- function(x) {
  names(x) <- gsub("\\.", "_", names(x))
  x
}
