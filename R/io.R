# -*- coding: us-ascii-unix -*-

#' Download file, retrying in case of failure.
#' @param url URL of file to download.
#' @param destfile name where the downloaded file is saved.
#' @param max.tries maximum amount of tries.
#' @param interval interval in seconds to wait between tries.
#' @param ... passed to \code{\link{download.file}}.
#' @export download.file.retry
download.file.retry = function(url, destfile, max.tries=3, interval=5, ...) {
    retval = try(utils::download.file(url, destfile, ...))
    if (!inherits(retval, "try-error") && retval == 0)
        return(invisible())
    if (max.tries <= 1)
        stop(sprintf("Download failed, aborting", max.tries))
    message("Download failed, retrying...")
    Sys.sleep(interval)
    return(Recall(url, destfile, max.tries-1, interval, ...))
}

#' Reload one saved dataset.
#'
#' Reload a data set containing only one object and return that
#' object. Will fail if data set contains more than one object.
#' @param ... passed to \code{\link{load}}.
#' @return An object.
#' @export load1
load1 = function(...) {
    names = load(...)
    if (length(names) != 1)
        stop("Multiple objects in loaded file")
    return(get(names[1]))
}

#' Reload one saved dataset keeping values cached.
#'
#' \code{load1.memo} is a memoized version of \code{\link{load1}}.
#' Objects loaded with \code{load1.memo} will stay in cache for
#' duration of the R session, unless size of the file changes
#' or cache is explicitly cleared. Be careful.
#' @param fname name of file to load. Use \code{NULL} to clear the cache.
#' @return An object.
#' @export load1.memo
load1.memo = local({
    cache = list()
    bytes = list()
    function(fname) {
        if (is.null(fname)) {
            cache <<- list()
            bytes <<- list()
            gc()
            return(NULL)
        }
        fname = normalizePath(fname)
        info = file.info(fname)
        if (fname %nin% names(cache) || bytes[[fname]] != info$size) {
            cache[[fname]] <<- load1(fname)
            bytes[[fname]] <<- info$size
        }
        return(cache[[fname]])
    }
})

#' Read tabular data from files of various types.
#'
#' \code{read.any} guesses the filetype based on given filename's extension
#' and uses the appropriate function to read the given file. \code{read.any}
#' is suitable for cases where you have a vector of filenames, which you know
#' to contain standard form data, but where the format may vary.
#' @param fname name of file to read.
#' @param ... passed to filetype-specific reading function.
#' @return A data frame.
#' @export read.any
read.any = function(fname, ...) {
    if (grepl("\\.RData", fname))
        return(load1(fname, ...))
    if (grepl("\\.txt", fname))
        return(read.delims(fname, ...))
    stop(sprintf("Unrecognized filetype: %s", fname))
}

#' Read a file in delimited format keeping strings as strings.
#' @param ... passed to \code{\link{read.delim}}.
#' @return A data frame.
#' @export read.delims
read.delims = function(...)
    utils::read.delim(..., stringsAsFactors=FALSE)

#' Read matrix from file.
#'
#' See \code{\link[utils]{read.table}} for arguments.
#' @param fname The name of the file to read.
#' @param header A logical value indicating whether the file contains the names
#'   of the variables as its first line.
#' @param sep The field separator character. Values on each line of the file are
#'   separated by this character.
#' @param ... Further arguments to be passed to \code{\link[utils]{read.table}}.
#' @return A matrix object.
#' @export read.matrix
read.matrix = function(fname, header=FALSE, sep="\t", ...) {
    df = utils::read.table(fname, header=header, sep=sep, ...)
    matrix = as.matrix(df)
    colnames(matrix) = NULL
    return(matrix)
}

#' Write tab-delimited file.
#'
#' See \code{\link[utils]{write.table}} for arguments.
#' @param x The object to be written, preferably a matrix or data frame.
#' @param fname A character string naming a file.
#' @param quote A logical value (\code{TRUE} or \code{FALSE}) or a numeric 
#'   vector. If \code{TRUE}, any character or factor columns will be surrounded 
#'   by double quotes. If a numeric vector, its elements are taken as the
#'   indices of columns to quote. In both cases, row and column names are quoted
#'   if they are written. If \code{FALSE}, nothing is quoted.
#' @param sep The field separator string.
#' @param na The string to use for missing values in the data.
#' @param row.names Either a logical value indicating whether the row names of
#'   \code{x} are to be written along with \code{x}, or a character vector of
#'   row names to be written.
#' @param ... Further arguments to be passed to
#'   \code{\link[utils]{write.table}}.
#' @export write.delim
write.delim = function(x, fname, quote=FALSE, sep="\t", na="", row.names=FALSE,
                       ...)

    utils::write.table(x,
                       fname,
                       quote=quote,
                       sep=sep,
                       na=na,
                       row.names=row.names,
                       ...)

#' Write transpose to tab-delimited file.
#'
#' See \code{\link{write.table}} for arguments.
#' @inheritParams write.delim
#' @param col.names Either a logical value indicating whether the column names 
#'   of \code{x} are to be written along with \code{x}, or a character vector of
#'   column names to be written.
#' @export write.delim.t
write.delim.t = function(x, fname, quote=FALSE, sep="\t", na="", row.names=TRUE,
                         col.names=FALSE, ...)

    utils::write.table(t(x),
                       fname,
                       quote=quote,
                       sep=sep,
                       na=na,
                       row.names=row.names,
                       col.names=col.names,
                       ...)

#' Write matrix to file.
#'
#' See \code{\link{write.table}} for arguments.
#' @inheritParams write.delim.t
#' @export write.matrix
write.matrix = function(x, fname, quote=FALSE, sep="\t", na="", row.names=FALSE,
                        col.names=FALSE, ...)

    utils::write.table(x,
                       fname,
                       quote=quote,
                       sep=sep,
                       na=na,
                       row.names=row.names,
                       col.names=col.names,
                       ...)

#' Write xtabs table to file.
#'
#' See \code{\link{write.table}} for arguments.
#' @inheritParams write.delim
#' @param prefix A dumb variable to account for the first missing cell of a
#'   contingency table.
#' @export write.xtabs
write.xtabs = function(x, fname, prefix="", quote=FALSE, sep="\t", ...) {
    write.delim(x, fname, quote, sep, row.names=TRUE, ...)
    lines = readLines(fname)
    lines[1] = paste0(prefix, sep, lines[1])
    return(writeLines(lines, fname))
}
