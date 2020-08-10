
#' @method apa_print summary.traitMPT
#' @export

apa_print.summary.traitMPT <- function(
  x
  , parameters = "mean"
  , ...) {

  extract_treebugs <- function(x, ...) {
    x <- as.data.frame(x, stringsAsFactors = FALSE)
    x$Term <- gsub(rownames(x), pattern = "mean_", replacement = "", fixed = TRUE)
    x$estimate <- x$Mean
    x$conf.int <- lapply(apply(X = x[, c("2.5%", "97.5%"), drop = FALSE], MARGIN = 1, list), unlist)
    x <- x[, c("Term", "estimate", "conf.int")]
    attr(x$conf.int, "conf.level") <- .95
    rownames(x) <- NULL

    canonical_x <- canonize(x, est_label = "$M$")
    beautiful_x <- beautify(canonical_x, capitalize = FALSE, ...)
    variable_labels(beautiful_x$term) <- "Parameter"
    beautiful_x
  }

  beautiful_x <- extract_treebugs(x$groupParameters[[parameters]], digits = 3, gt1 = FALSE)

  if(parameters == "rho") {
    beautiful_x$term[] <- gsub(pattern = "rho[", replacement = "$\\rho_{", beautiful_x$term, fixed = TRUE)
    beautiful_x$term[] <- gsub(pattern = "]", replacement = "}$", beautiful_x$term, fixed = TRUE)
  }

  beautiful_x
}
