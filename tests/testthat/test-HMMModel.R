deparseCall <- function(name, args) {

  paste0(deparse(as.call(c(list(as.symbol(name)), args)),
                 control = c("niceNames", "showAttributes"), width.cutoff = 80),
         collapse = "\n")

  #  sprintf("%s(%s)", name, paste(names(args), sapply(args, deparse,
  #          control = c("niceNames", "showAttributes"), width.cutoff = 80),
  #    sep = " = ", collapse = ", "))
}


expect_checkmate <- function(fun, working.args, tryouts) {
  docopy <- function(l) {
    lapply(l, function(x) x)
  }

  funname <- as.character(substitute(fun))
  do.call(fun, docopy(working.args))

  for (tr in seq_along(tryouts)) {
    new.args <- working.args
    new.args[[names(tryouts)[[tr]]]] <- tryouts[[tr]]
    expect_error(do.call(fun, docopy(new.args)),
                 regex = "Assertion.*failed",
                 info = sprintf("\nCalling the following should have generated a checkmate assertion error but didn't:\n\n%s\n",
                                deparseCall(funname, new.args))
    )
  }
}

test_that("HMMModel", {

  expect_checkmate(HMMModel$new,list(identifier = "Assert1", states = c("1", "2", "3"),
                                     signal.set =c("a", "b")), list(identifier = 1))
})
