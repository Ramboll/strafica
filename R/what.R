# -*- coding: us-ascii-unix -*-

#' Print summary of data frame contents.
#' @param df a data frame or a vector.
#' @param ... names of columns as either symbols or strings.
#' If not given, summaries of all columns will be printed.
#' @seealso \code{\link{summary}}, \code{\link{str}}
#' @export what
what = function(df, ...) {
    if (is.vector(df))
        df = data.frame(x=df)
    names = as.character(substitute(expression(...)))[-1]
    if (length(names) > 0)
        df = df[,names,drop=FALSE]
    stopifnot(is.data.frame(df))
    .what.general(df)
    .what.columns(df)
    return(invisible())
}

# Print summary of data frame columns.
.what.columns = function(df) {
    stat = data.frame(.name=rep(NA, 11))
    stat$.name[01] = "Class:"
    stat$.name[02] = "N.uniq:"
    stat$.name[03] = "N.NA:"
    stat$.name[04] = "N.Inf:"
    stat$.name[05] = "N.0:"
    stat$.name[06] = "N.1:"
    stat$.name[07] = "Min:"
    stat$.name[08] = "Max:"
    stat$.name[09] = "Mean:"
    stat$.name[10] = "Median:"
    stat$.name[11] = "Std.dev:"
    .format = function(x) format(x, digits=3)
    for (colname in colnames(df)) {
        value = rep("\u2013", nrow(stat))
        x = df[,colname]
        xok = x[is.finite(x)]
        value[01] = class(x)
        value[02] = length(unique(x))
        value[03] = sum(is.na(x))
        if (is.numeric(x) || is.logical(x)) {
            value[04] = sum(is.infinite(x))
            if (length(xok) > 0) {
                value[05] = sum(xok == 0)
                value[06] = sum(xok == 1)
                value[07] = .format(min(xok))
                value[08] = .format(max(xok))
                value[09] = .format(mean(xok))
                value[10] = .format(stats::median(xok))
                value[11] = .format(stats::sd(xok))
            }
        }
        stat[,colname] = value
    }
    footer = data.frame(.name=" ")
    footer[,colnames(df)] = list("")
    stat = rbind(stat, footer)
    rownames(stat) = stat$.name
    stat$.name = NULL
    cat("\ndf$*:\n")
    print(stat)
    return(invisible())
}

# Print general summary of data frame.
.what.general = function(df) {
    stat = data.frame(name=rep(NA, 3), value=rep(NA, 3))
    stat$name[1] = "N.row:"
    stat$name[2] = "N.col:"
    stat$name[3] = "Size.MB:"
    stat$value[1] = nrow(df)
    stat$value[2] = ncol(df)
    stat$value[3] = round(utils::object.size(df) / (1024^2))
    rownames(stat) = stat$name
    stat$name = NULL
    cat("\ndf:\n")
    print(stat)
    return(invisible())
}
