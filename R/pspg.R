pspg <- function(x, ...) UseMethod("pspg")

pspg_call <- function(tbl, args, fixed_cols = NULL) {
  # Parse any pspg.args.* as a pspg command-line option
  opts <- .Options
  args <- opts[grep("^pspg\\.args\\.", names(opts))]

  if (getOption("pspg.args.ignore-case", default = TRUE)) {
    args[["ignore-case"]] <- TRUE
  }
  if (getOption("pspg.args.no-topbar", default = TRUE)) {
    args[["no-topbar"]] <- TRUE
  }

  args[["freezecols"]] <- if (is.null(fixed_cols)) {
    "1"
  } else {
    as.character(fixed_cols)
  }
  if (!is.null(colnames(tbl))) {
    args[["csv-header"]] <- "on"
  }

  arg_strings <- unlist(lapply(
    unique(names(args)),
    function(n) {
      v <- args[[n]]
      if (isTRUE(v)) {
        return(paste0("--", n))
      }
      return(paste0("--", n, "=", v))
    }
  ))

  # TODO: significant figures option somewhere
  tmp_path <- tempfile(pattern = "pspg", fileext = ".csv")
  on.exit(unlink(tmp_path), add = TRUE)
  utils::write.table(
    tbl,
    file = tmp_path,
    row.names = is.null(fixed_cols), # null fixed_cols --> show rownames column
    col.names = if (is.null(colnames(tbl))) {
      # No colnames, so don't include any header row
      FALSE
    } else if (is.null(fixed_cols)) {
      # NA means include blank corner-cell for the rownames column
      NA
    } else {
      # Include column name row
      TRUE
    },
    fileEncoding = "UTF-8",
    # Defaults from write.csv
    sep = ",",
    dec = ".",
    qmethod = "double"
  )

  pspg_path <- Sys.which("pspg")
  if (!nzchar(pspg_path)) {
    stop(paste(
      "pspg isn't available on your system. Install it with the relevant command, e.g:",
      "    * apt install pspg",
      "    * dnf install pspg",
      "    * brew install pspg",
      sep = "\n"
    ))
  }
  system2(c(pspg_path, "--csv", arg_strings, tmp_path))
  cat("\n\n")
}

pspg.data.frame <- function(x, ...) {
  tbl <- x
  pspg_call(tbl, args = list(...))
  return(invisible(x))
}

pspg.list <- function(x, ...) {
  tbl <- data.frame(
    # TODO: Spread values out along columns
    V1 = vapply(
      x,
      function(v) paste(as.character(v), collapse = ", "),
      character(1)
    ),
    stringsAsFactors = FALSE
  )
  rownames(tbl) <- names(x)
  colnames(tbl) <- NULL
  pspg_call(tbl, args = list(...))
  return(invisible(x))
}

pspg.default <- function(x, ...) {
  # Get unparsed x, but only if it was a single variable (to keep columns short)
  in_var <- sys.call(-1)[[2]]
  in_var <- if (is.symbol(in_var)) as.character(in_var) else "value"

  fixed_cols <- NULL

  x_dropped <- drop(x)
  if (length(dim(x_dropped)) > 2) {
    tbl <- as.data.frame.table(x_dropped, responseName = in_var)
    fixed_cols <- seq_along(dim(x_dropped))
  } else {
    tbl <- as.data.frame(x_dropped, optional = TRUE)
  }

  pspg_call(tbl, args = list(...), fixed_cols = fixed_cols)
  return(invisible(x))
}
