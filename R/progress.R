# -*- coding: us-ascii-unix -*-

#' Bump progress in progress bar.
#' @param bar progress bar object.
#' @param n amount to bump by.
#' @seealso \code{\link{setTxtProgressBar}}
#' @export bumpTxtProgressBar
bumpTxtProgressBar = function(bar, n=1)
    utils::setTxtProgressBar(bar, utils::getTxtProgressBar(bar) + n)

#' Query permission to print progress output.
#' @param interval amount of seconds between which to print.
#' @param parallel \code{TRUE} if task is being run in parallel processes.
#' @param n amount of processes used. Give only if different
#' from what is returned by \code{\link{cores.available}}.
#' @return A logical value.
#' @export progress.due
progress.due = local({
    # Cache next scheduled update time across calls.
    utime = -1
    function(interval, parallel=FALSE, n=NULL) {
        time = .Internal(Sys.time())
        if (parallel && is.null(n))
            n = cores.available()
        if (parallel && utime < 0) {
            # Assume consecutive PIDs for parallel processes
            # and print initial progress for only a few.
            utime <<- time + stats::runif(1) * n * interval
            return(.Internal(Sys.getpid()) %% n < 3)
        }
        if (time < utime)
            return(FALSE)
        if (parallel)
            interval = n * interval
        utime <<- time + interval
        return(TRUE)
    }
})

#' Print progress of running task.
#' @param time.start start time as returned by \code{\link{Sys.time}}.
#' @param n current item.
#' @param total total amount of items.
#' @param suffix additional text to include.
#' @export progress.eta
progress.eta = function(time.start, n, total, suffix="") {
    elapsed = as.numeric(difftime(Sys.time(), time.start, units="secs"))
    speed = n / elapsed
    time.rem = pmin(359999, (total - n) / speed)
    messagef("% 9d rem @% 9.2f items/s, ETA: %02d:%02d:%02d%s",
             total - n,
             speed,
             as.integer(floor((time.rem / 3600))),
             as.integer(floor((time.rem %% 3600) / 60)),
             as.integer(floor((time.rem %% 60))),
             suffix)

}

#' Print statistics of completed task.
#' @param time.start start time as returned by \code{\link{Sys.time}}.
#' @export progress.final
progress.final = function(time.start) {
    elapsed = as.numeric(difftime(Sys.time(), time.start, units="secs"))
    messagef("...completed in %d h %d min %d s.",
             floor((elapsed / 3600)),
             floor((elapsed %% 3600) / 60),
             floor((elapsed %% 60)))

}
