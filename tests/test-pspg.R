if (!interactive()) options(warn=2, error = function() { sink(stderr()) ; traceback(3) ; q(status = 1) })
library(unittest)

library(pspg)

do_pspg_format <- function (x, test_options=list()) {
  # Make a fake PSPG that cats the input
  fake_pspg_bin <- tempfile(pattern = "ut_pspg_", fileext = ".sh")
  test_options[["pspg.binary"]] <- fake_pspg_bin
  writeLines(text = '#!/bin/bash
echo pspg $*
cat $1
  ', con = fake_pspg_bin)
  Sys.chmod(fake_pspg_bin, mode = "0744")
  on.exit(unlink(fake_pspg_bin), add = TRUE)

  # Store old options, resetting previously unset
  old_options <- options()[grep("^pspg\\.", names(options()))]
  for (n in names(test_options)) {
    if (!(n %in% names(old_options))) old_options[n] <- list(NULL)
  }
  on.exit(do.call(options, old_options), add = TRUE)

  do.call(options, test_options)
  # TODO: Not actually capturing stdout
  stdout <- capture.output({
    x_out <- pspg::pspg(x)
  })
  ok(ut_cmp_identical(x, x_out), "pspg: Output identical to input")
  return(stdout)
}

# TODO: do_pspg_format(data.frame(x=1:10))
    