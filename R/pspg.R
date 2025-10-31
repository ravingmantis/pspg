pspg <- function(x, ...) UseMethod("pspg")

pspg_call <- function(tbl, args, row.names = FALSE, col.names = TRUE) {
  if (getOption("pspg.args.ignore-case", default = TRUE)) {
    args[["ignore-case"]] <- TRUE
  }
  if (isTRUE(row.names)) {
    args[["freezecols"]] <- "0"
  }
  if (isTRUE(col.names)) {
    args[["csv-header"]] <- "on"
  }

  arg_strings <- vapply(
    unique(names(args)),
    function(n) {
      v <- args[[n]]
      if (isTRUE(v)) {
        return(paste0("--", n))
      }
      return(paste0("--", n, "=", v))
    },
    character(1)
  )

  tmp_path <- tempfile(pattern = "pspg", fileext = ".csv")
  # TODO: Handling of row.names / col.names is wrong, read ?write.csv more closely
  # TODO: significant figures option somewhere
  utils::write.csv(
    tbl,
    file = tmp_path,
    row.names = row.names,
    col.names = col.names
  )
  on.exit(unlink(tmp_path), add = TRUE)
  # TODO: Fall over if pspg isn't available, with a friendly message
  system2(c(Sys.which("pspg"), "--csv", arg_strings, tmp_path))
  cat("\n\n")
}

pspg.data.frame <- function(x, ...) {
  tbl <- x
  # TODO: Toggle row.names / col.names based on content
  pspg_call(tbl, args = list(...), row.names = FALSE, col.names = TRUE)
  return(invisible(tbl))
}

pspg.list <- function(x, ...) {
  tbl <- data.frame(
    key = names(x),
    # TODO: Spread values out along columns
    value = vapply(
      x,
      function(v) paste(as.character(v), collapse = ", "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  pspg_call(tbl, args = list(...), row.names = TRUE, col.names = FALSE)
  return(invisible(tbl))
}

pspg.default <- function(x, ...) {
  # TODO: Would as.data.frame.table() be more sensible in some cases?
  tbl <- as.data.frame(x)
  pspg_call(tbl, args = list(...))
  return(invisible(tbl))
}
