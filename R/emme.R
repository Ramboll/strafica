# -*- coding: us-ascii-unix -*-

#' List matrices in Emme matrix file.
#' @param fname name of matrix file.
#' @return A character vector of matrix names.
#' @export list.emme.matrices
list.emme.matrices = function(fname) {
    lines = readLines(fname)
    matrices = grep("^a matrix=", lines, value=TRUE)
    matrices = gsub("^a matrix=", "", matrices)
    return(trimws(matrices))
}

#' Read matrices from Emme matrix file.
#' @param fname name of matrix file.
#' @return A list of data frames.
#' @export read.emme.matrices
read.emme.matrices = function(fname) {
    .read = function(x) read.emme.matrix(fname, x)
    names = list.emme.matrices(fname)
    matrices = lapply(names, .read)
    names(matrices) = names
    return(matrices)
}

#' Read matrix from Emme matrix file.
#' @param fname name of matrix file.
#' @param matrix name of matrix to read, e.g. "mf123".
#' @return A data frame with columns "p", "q" and "value".
#' @export read.emme.matrix
read.emme.matrix = function(fname, matrix) {
    lines = readLines(fname)
    m = grep(sprintf("^a matrix=%s ", matrix), lines)
    lines = lines[(m[1]+1):length(lines)]
    m = grep("^a matrix=", lines)
    if (length(m) > 0)
        lines = lines[1:(m-1)]
    lines = trimws(lines)
    lines = lines[lines != ""]
    lines = strsplit(lines, "[ :]+", perl=TRUE)
    values = lapply(lines, function(line) {
        stopif(length(line) == 1)
        stopif(is.even(length(line)))
        # FROM TO1:VALUE1 TO2:VALUE2 ...
        p = as.numeric(line[1])
        q = as.numeric(line[seq(2, length(line), 2)])
        v = as.numeric(line[seq(3, length(line), 2)])
        return(cbind(p, q, v))
    })
    values = do.call(rbind, values)
    df = as.data.frame(values)
    colnames(df) = c("p", "q", "value")
    df = arrange(df, p, q)
    rownames(df) = rows.along(df)
    return(downclass(df))
}

#' Read submatrix from emme output file.
#' @param fname name of matrix file.
#' @return A data frame.
#' @export read.emme.submatrix
read.emme.submatrix = function(fname) {
    lines = readLines(fname)
    header = grep("^ +\\(p\\) ", lines)[1]
    values = grep("^ +[0-9]+ ", lines)
    fname = tempfile()
    writeLines(lines[c(header, values)], fname)
    df = utils::read.table(fname, header=TRUE)
    colnames(df)[1:2] = c("p", "q")
    return(df)
}

#' Write matrices to Emme matrix file.
#' @param p a list of vectors of row values (e.g. origins).
#' @param q a list of vectors of column values (e.g. destinations).
#' @param value a list of vectors of values for p-q pairs.
#' @param fname name of file to write.
#' @param header a vector of values of matrix headers.
#' Do not include "t matrices" here, it is added automatically.
#' @export write.emme.matrices
write.emme.matrices = function(p, q, value, fname, header="") {
    if (length(header) == 1)
        header = rep(header, length(p))
    lines = unlist(lapply(seq_along(p), function(i) {
        lines = sprintf(" %s %s: %s", p[[i]], q[[i]], value[[i]])
        return(c(header[i], lines))
    }))
    lines = c("t matrices", lines)
    return(writeLines(lines, fname))
}

#' Write matrix to Emme matrix file.
#' @param p a vector of row values (e.g. origins).
#' @param q a vector of column values (e.g. destinations).
#' @param value a vector of values for p-q pairs.
#' @param fname name of file to write.
#' @param header a matrix header string.
#' Do not include "t matrices" here, it is added automatically.
#' @export write.emme.matrix
write.emme.matrix = function(p, q, value, fname, header="") {
    lines = sprintf(" %s %s: %s", p, q, value)
    lines = c("t matrices", header, lines)
    return(writeLines(lines, fname))
}
