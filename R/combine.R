# -*- coding: us-ascii-unix -*-

#' Subset one data frame to match what is not found in another.
#' @param x data frame to subset.
#' @param y reference data frame.
#' @param by names of columns to match.
#' @return Data frame \code{x} with a subset of rows that don't have
#' a \code{by}-wise match in \code{y}. Columns of \code{x} are not modified.
#' @export antijoin
antijoin = function(x, y, by=intersect(names(x), names(y))) {
    if (is.null(y)) return(x)
    if (nrow(y) == 0) return(x)
    y$.antijoin.id = TRUE
    ynames = c(by, ".antijoin.id")
    z = leftjoin(x[,by,drop=FALSE], y[,ynames,drop=FALSE], by)
    m = which(is.na(z$.antijoin.id))
    return(x[m,,drop=FALSE])
}

#' Join two data frames together.
#' @param x a data frame.
#' @param y a data frame.
#' @param by names of columns to join by.
#' @param missing value to use in new columns for rows
#' found in \code{x}, but not in \code{y} or vice versa.
#' @return A data frame with all \code{by}-wise
#' unique rows of both \code{x} and \code{y}.
#' @export fulljoin
fulljoin = function(x, y, by=intersect(names(x), names(y)), missing=NA) {
    x$.xid = rows.along(x)
    y$.yid = rows.along(y)
    xy = leftjoin(x, y, by=by, missing=missing)
    y2 = subset(y, !(.yid %in% xy$.yid))
    z = if (nrow(y2) > 0) {
        # rightjoin rows in y not found in xy.
        rbind_list(xy, leftjoin(y2, x, by=by, missing=missing))
    } else xy
    return(unpick(z, .xid, .yid))
}

#' Join two data frames together.
#' @param x a data frame.
#' @param y a data frame.
#' @param by names of columns to join by.
#' @param overwrite \code{TRUE} to replace existing columns in \code{x},
#' \code{FALSE} to only add new columns.
#' @param missing value to use in new columns for rows
#' found in \code{x}, but not in \code{y}.
#' @return A data frame with as many rows as \code{x}.
#' @export leftjoin
leftjoin = function(x, y, by=intersect(names(x), names(y)), overwrite=FALSE,
                    missing=NA) {

    ids = rbind_list(x[,by,drop=FALSE], y[,by,drop=FALSE])
    id = do.call(classify, ids)
    if (any(id > 2^.Machine$double.digits - 1))
        stop("Match key exceeds integer precision")
    yi = match(id[rows.along(x)], id[-rows.along(x)])
    names.join = setdiff(colnames(y), colnames(x))
    if (overwrite) {
        # Remove existing columns from x.
        names.join = setdiff(colnames(y), by)
        x[intersect(colnames(x), names.join)] = list(NULL)
    }
    if (length(names.join) == 0) {
        warning("No columns to join")
        return(x)
    }
    if (nrow(x) > 1000000) {
        # Use data.table to avoid excessive copying of x.
        # (Doing the actual join using data.table would be even
        # faster, but data.table mangles the row ordering, undoing
        # which makes the total time taken slower.)
        x = as.data.table(x)
        for (name in names.join)
            set(x, i=NULL, j=name, value=y[yi,name])
        data.table::setDF(x)
        post.gc(rm(y))
    } else {
        x[,names.join] = y[yi,names.join]
    }
    if (!is.na(missing) && any(is.na(yi)))
        x[which(is.na(yi)), names.join] = missing
    return(x)
}

#' Efficiently rbind multiple data frames.
#' @param dfs a list of data frames to combine.
#' @return A data frame.
#' @export rbind_all
rbind_all = function(dfs) {
    if (length(dfs) == 0) return(NULL)
    if (length(dfs) == 1) return(dfs[[1]])
    df = data.table::rbindlist(dfs, use.names=TRUE, fill=TRUE)
    data.table::setDF(df)
    return(df)
}

#' Efficiently rbind multiple data frames.
#' @param ... individual data frames to combine.
#' @return A data frame.
#' @export rbind_list
rbind_list = function(...)
    rbind_all(list(...))

#' Subset one data frame to match what is found in another.
#' @param x data frame to subset.
#' @param y reference data frame.
#' @param by names of columns to match.
#' @return Data frame \code{x} with a subset of rows that have a \code{by}-wise
#' match in \code{y}. Columns of \code{x} are not modified.
#' @export semijoin
semijoin = function(x, y, by=intersect(names(x), names(y))) {
    if (is.null(y)) return(x[FALSE,,drop=FALSE])
    if (nrow(y) == 0) return(x[FALSE,,drop=FALSE])
    y$.semijoin.id = TRUE
    ynames = c(by, ".semijoin.id")
    z = leftjoin(x[,by,drop=FALSE], y[,ynames,drop=FALSE], by)
    m = which(!is.na(z$.semijoin.id))
    return(x[m,,drop=FALSE])
}
