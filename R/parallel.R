# -*- coding: us-ascii-unix -*-

#' Query amount of logical CPU cores designated for use.
#' @return Amount of cores.
#' @export cores.available
cores.available = local({
    total = NULL
    function() {
        n = getOption("mc.cores")
        if (is.null(n)) {
            if (is.null(total))
                total <<- parallel::detectCores(logical=TRUE)
            n = total
        }
        return(n)
    }
})

#' Parallel version of lapply that stops after errors.
#'
#' Call \code{\link{mclapply}} and once all processes have completed,
#' check for possible errors and stop if errors found.
#' @param ... passed to \code{\link{mclapply}}.
#' @export mclapply.stop
mclapply.stop = function(...) {
    value = parallel::mclapply(...)
    if (length(value) == 0)
        return(value)
    m = which(sapply(value, inherits, what="try-error"))
    sapply(m, function(i) stop(value[[i]]))
    return(value)
}
