factory <- function(fun)
  function(...) {
    warn <- err <- NULL
    res <- withCallingHandlers(
      tryCatch(fun(...), error=function(e) {
        err <<- conditionMessage(e)
        NULL
      }), warning=function(w) {
        warn <<- append(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    list(res, warn=warn, err=err)
  }



test <- function(i)
  switch(i, "1"=stop("oops"), "2"={ warning("hmm"); i }, i)
res <- lapply(1:3, factory(test))


.has <- function(x, what)
  !sapply(lapply(x, "[[", what), is.null)
hasWarning <- function(x) .has(x, "warn")
hasError <- function(x) .has(x, "err")
isClean <- function(x) !(hasError(x) | hasWarning(x))
value <- function(x) sapply(x, "[[", 1)
cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)

hasWarning(res)
hasError(res)
isClean(res)
value(res)
cleanv(res)
