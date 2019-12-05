#' Read a `S4Vectors::DataFrame` as a data source
#'
#' Create a data source to work with `S4Vectors::DataFrame`
#' \pkg{dplyr} verbs.
#'
#' @param x A `S4Vectors::DataFrame` object
#'
#' @md
#' @export
#' @examples
#' library(S4Vectors)
#' library(dplyr)
#' 
#' DF <- as(mtcars, "DataFrame")
#' src <- src_DF(DF)
#' src
#' 
#' DF %>% src_DF() %>% mutate_at(vars(starts_with("c")), ~.^2)
#' DF %>% src_DF() %>% group_by(cyl, am) %>% tally(gear)
#' DF %>% src_DF() %>% count(gear, am, cyl)
#' DF %>% src_DF() %>% select(am, cyl)
#' DF %>% src_DF() %>% select(am, cyl) %>% rename(foo = am)
#' DF %>% src_DF() %>% arrange(desc(hp))
#' rbind(data.frame(mtcars[1, ], row.names = "MyCar"), DF) %>% src_DF %>% distinct()
#' DF %>% src_DF() %>% filter(am == 0)
src_DF <- function(x) {
  if (inherits(x, "DataFrame"))
    dplyr::src(subclass = "DF", x = x)
  else 
    x
}

#' @export
format.src_DF <- function(x, ...) {
  x
}

print.src_DF <- function(x, ...) {
  S4Vectors::show(x$x)
}

filter.src_DF <- function(.data, ..., .preserve = FALSE) {
  rn <- rownames(.data$x)
  t <- dplyr::tbl_df(.data$x)
  identifiers <- apply(.data$x, 1, digest::digest)
  tf <- dplyr::filter(t, ..., .preserve = .preserve)
  subset_identifiers <- apply(tf, 1, digest::digest)
  tDF <- as(tf, "DataFrame")
  newrn <- identifiers[identifiers %in% subset_identifiers]
  newrn <- newrn[!duplicated(newrn)]
  rownames(tDF) <- names(newrn)
  src_DF(tDF)
}

mutate.src_DF <- function(.data, ...) {
  rn <- rownames(.data$x)
  t <- dplyr::tbl_df(.data$x)
  tm <- dplyr::mutate(t, ...)
  tDF <- as(tm, "DataFrame")
  rownames(tDF) <- rn
  src_DF(tDF)
}

tbl_vars.src_DF <- function(x) {
  names(x$x)
}

select.src_DF <- function(.data, ...) {
  rn <- rownames(.data$x)
  t <- dplyr::tbl_df(.data$x)
  tm <- dplyr::select(t, ...)
  tDF <- as(tm, "DataFrame")
  rownames(tDF) <- rn
  src_DF(tDF)
}

rename.src_DF <- function(.data, ...) {
  rn <- rownames(.data$x)
  t <- dplyr::tbl_df(.data$x)
  tm <- dplyr::rename(t, ...)
  tDF <- as(tm, "DataFrame")
  rownames(tDF) <- rn
  src_DF(tDF)
}

count.src_DF <- function(.data, ...) {
  t <- convert_with_group(.data)
  dplyr::count(t, ...)
}

tally.src_DF <- function(x, wt = NULL, sort = FALSE, name = "n") {
  t <- convert_with_group(.data)
  dplyr::tally(t, wt = wt, sort = sort, name = name)
}

summarise.src_DF <- function(.data, ...) {
  rn <- rownames(.data$x)
  t <- convert_with_group(.data)
  dplyr::summarise(t, ...)
}

group_data.src_DF <- function(.data) {
  attr(.data$x, "groups")
}

group_vars.src_DF <- function(x) {
  ## dplyr:::group_vars.grouped_df (not exported)
  groups <- group_data(x)[[1]]
  if (is.character(groups)) {
    groups
  }
  else if (is.data.frame(groups)) {
    head(names(groups), -1L)
  }
  else if (is.list(groups)) {
    purrr::map_chr(groups, rlang:::as_string)
  }
}

group_by.src_DF <- function(.data, ..., add = FALSE, .drop = group_by_drop_default(.data)) {
  rn <- rownames(.data$x)
  t <- convert_with_group(.data)
  tm <- dplyr::group_by(t, ..., add = add, .drop = .drop)
  groupdata <- group_data(tm)
  tDF <- as(tm, "DataFrame")
  rownames(tDF) <- rn
  attr(tDF, "groups") <- list(groupdata)
  src_DF(tDF)
}

arrange.src_DF <- function(.data, ...) {
  rn <- rownames(.data$x)
  t <- convert_with_group(.data)
  ta <- dplyr::arrange(t, ...)
  tDF <- as(ta, "DataFrame")
  rownames(tDF) <- rn
  src_DF(tDF)
}

#' @importFrom digest digest
distinct.src_DF <- function(.data, ..., .keep_all = FALSE) {
  rn <- rownames(.data$x)
  t <- convert_with_group(.data)
  identifiers <- apply(.data$x, 1, digest::digest)
  td <- dplyr::distinct(t, ..., .keep_all = .keep_all)
  subset_identifiers <- apply(td, 1, digest::digest)
  tDF <- as(td, "DataFrame")
  newrn <- identifiers[identifiers %in% subset_identifiers]
  newrn <- newrn[!duplicated(newrn)]
  rownames(tDF) <- names(newrn)
  src_DF(tDF)
}

convert_with_group <- function(.data) {
  t <- dplyr::tbl_df(.data$x)
  if (!is.null(group_data(.data))) {
    for (gvar in group_vars(.data)) {
      t <- dplyr::group_by(t, !!sym(gvar), add = TRUE)
    }
  }
  t
}
