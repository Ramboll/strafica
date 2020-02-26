# -*- coding: us-ascii-unix -*-

#' Split data frame and calculate group-wise summaries.
#' @param df data frame to summarize.
#' @param by columns to split by, either quoted or strings.
#' @param ... one or more keyword arguments with function calls
#' of summaries to calculate, e.g. \code{xmin=min(x), xmax=max(x)}.
#' @return A data frame with one row for each unique combination of \code{by}
#' and columns \code{by} and \code{...} in sorted order of \code{by}.
#' @export fold
fold = function(df, by, ...) {
    big = nrow(df) > 1000000
    if (inherits(by, "quoted"))
        by = names(by)
    df = data.table::as.data.table(df)
    if (big) gc()
    df = df[, eval(substitute(list(...))), by]
    data.table::setorderv(df, by)
    data.table::setDF(df)
    if (big) gc()
    rownames(df) = rows.along(df)
    return(df)
}

#' Split data frame and calculate group-wise summaries.
#'
#' \code{mcddply} is a parallel reimplementation of \code{\link{ddply}}.
#' Compared to \code{ddply} it is especially suitable for data frames
#' with a large amount of groups.
#' @param df data frame to summarize.
#' @param by columns to split by, either quoted or strings.
#' @param fun function to apply to each piece.
#' @param ... arguments passed to \code{fun}.
#' @param .progress "text" or "none".
#' @param .parallel \code{TRUE} to use all cores.
#' @return A data frame.
#' @export mcddply
mcddply = function(df, by, fun, ..., .progress="text", .parallel=TRUE) {
    if (inherits(by, "quoted"))
        by = names(by)
    indices = uind(do.call(classify, df[,by,drop=FALSE]))
    do.print.progress = (.progress == "text")
    if (do.print.progress)
        messagef("Summarizing %d pieces...", length(indices))
    time.start = Sys.time()
    .apply = if (.parallel) mclapply.stop else lapply
    stat = .apply(seq_along(indices), function(i) {
        if (do.print.progress &&
            progress.due(3, parallel=.parallel))
            progress.eta(time.start, i, length(indices))
        out = fun(df[indices[[i]],,drop=FALSE], ...)
        if (is.null(out) || nrow(out) == 0)
            return(NULL)
        if (!all(by %in% colnames(out)))
            out = cbind(out, df[
                indices[[i]][1],
                setdiff(by, colnames(out)),
                drop=FALSE
            ], row.names=NULL)
        return(out)
    })
    stat = rbind_all(stat)
    if (do.print.progress)
        progress.final(time.start)
    return(stat)
}
