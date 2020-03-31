# -*- coding: us-ascii-unix -*-

#' Quote variables to create a list of unevaluated expressions.
#'
#' Don't go crazy using \code{.} all over the place! \code{.} is ported over
#' from \pkg{plyr} for use in conjunction with \code{\link{ddply}}.
#' @param ... unevaluated expressions to be recorded.
#' @param .env environment in which to evaluate.
#' @return List of symbol and language primitives.
#' @import data.table ggplot2 methods parallel scales
#' @export .
#' @rdname dot
. = function(..., .env=parent.frame())
    # Copied and adapted from plyr::.,
    # http://github.com/hadley/plyr
    # Copyright Hadley Wickham, MIT-licensed.
    structure(as.list(match.call()[-1]), env=.env, class="quoted")

#' Add new columns to data frame with NA values.
#' @param df A data frame.
#' @param ... names of columns as either symbols or strings.
#' @return Data frame \code{df} with columns added.
#' @export add.columns
add.columns = function(df, ...) {
    dots = match.call(expand.dots=FALSE)$...
    names = unlist(lapply(dots, as.character))
    if (any(names %in% colnames(df))) {
        existing = names[names %in% colnames(df)]
        warning(sprintf(
            "Not adding existing columns: %s",
            paste(existing, collapse=", ")))
        names = setdiff(names, existing)
    }
    new = lapply(names, function(x) rep(NA, nrow(df)))
    new = as.data.frame(new)
    colnames(new) = names
    return(cbind(df, new))
}

#' Find file or directory from among ancestors.
#' @param name names of files or directories to look for.
#' Wildcards as defined in \code{\link{Sys.glob}} are allowed.
#' @param max.depth maximum depth to search upwards.
#' @return Path to file or directory relative to working directory.
#' @export ancfile
ancfile = function(name, max.depth=12) {
    if (length(name) > 1)
        return(sapply(name, ancfile))
    basename = name
    for (i in seq_len(max.depth)) {
        name = file.path("..", name)
        values = Sys.glob(name)
        if (length(values) > 0)
            return(values)
    }
    stop(sprintf("File '%s' not found among ancestors", basename))
}

#' Classify variable by breaks.
#' @param x a numeric vector.
#' @param class a vector of classes.
#' @param lower a numeric vector of lower bounds of classes.
#' @param upper a numeric vector of upper bounds of classes.
#' @return A vector the same length of \code{x} with values from \code{class}.
#' Elements outside given breaks will have values \code{NA}.
#' @seealso \code{\link{cut}}
#' @export apply.breaks
apply.breaks = function(x, class, lower, upper) {
    stopifnot(length(class) == length(lower))
    stopifnot(length(class) == length(upper))
    xclass = NA
    for (i in seq_along(class))
        xclass[x >= lower[i] & x <= upper[i]] = class[i]
    return(xclass)
}

#' Order a data frame by its colums.
#' @param df a data frame.
#' @param ... expressions in the context of \code{df} to order by.
#' @return A data frame.
#' @export arrange
arrange = function(df, ...) {
    # For large data frames conversion to data.table and using setorder
    # would be faster, but data.table only sorts based on columns in df
    # and has its own syntax for descending sort.

    # Copied and adapted from plyr::arrange,
    # http://github.com/hadley/plyr
    # Copyright Hadley Wickham, MIT-licensed.
    stopifnot(is.data.frame(df))
    ord = eval(substitute(order(...)), df, parent.frame())
    stopif(length(ord) != nrow(df))
    df = df[ord,,drop=FALSE]
    rownames(df) = rows.along(df)
    return(df)
}

#' Concatenate output separated by newlines.
#' @param ... passed to \code{\link{cat}}.
#' @param sep strings to append after each element.
#' @export catn
catn = function(..., sep="\n")
    cat(..., sep=sep)

#' Cubic root.
#' @param x numeric object.
#' @return A numeric vector the size of \code{x}.
#' @export cbrt
cbrt = function(x)
    x^(1/3)

#' Set working directory and list files.
#' @param ... passed to \code{\link{setwd}}.
#' @return See \code{\link{setwd}}.
#' @export cd
cd = function(...) {
    value = setwd(...)
    print(list.files())
    return(value)
}

#' Check for \code{NA} values and print result.
#' @param ... data frames or objects coercible to data frames.
#' @export check.na
check.na = function(...) {
    dfs = list(...)
    for (i in which(!sapply(dfs, is.data.frame))) {
        dfs[[i]] = as.data.frame(dfs[[i]])
        colnames(dfs[[i]]) = sprintf("C%d", seq_along(dfs[[i]]))
    }
    names = as.character(substitute(expression(...)))[-1]
    for (i in seq_along(dfs)) {
        messagef("Checking '%s' for NA values...", names[i])
        stat = dfsas(name=colnames(dfs[[i]]))
        stat$count = sapply(dfs[[i]], function(x) sum(is.na(x)))
        stat = subset(stat, count > 0)
        if (nrow(stat) == 0) next
        stat$pcent = 100 * (stat$count / nrow(dfs[[i]]))
        stat$pcent = sprintf("%.1f %%", stat$pcent)
        stat$name  = pad(stat$name,  n=max(nchar(stat$name)))
        stat$count = pad(stat$count, n=max(nchar(stat$count)))
        stat$pcent = pad(stat$pcent, n=max(nchar(stat$pcent)))
        for (j in rows.along(stat))
            with(stat[j,], messagef(" %s: %s: %s", name, count, pcent))
    }
    return(invisible())
}

#' Split data frame to a list of data frames.
#' @param x a data frame.
#' @param id a vector of values to chop by.
#' @return A list of data frames.
#' @export chop
chop = function(x, id) {
    xs = lapply(uind(id), function(ii) x[ii,,drop=FALSE])
    names(xs) = NULL
    return(xs)
}

#' Form unique identifiers from combinations of values.
#'
#' \code{classify} is what is also known as a "pairing function",
#' but not an invertible one. This also means that return values
#' for individual elements depend on other elements given as arguments
#' and the values of individual elements are thus not comparable
#' across calls.
#' @param ... vectors of the same length.
#' @return An integer-value vector the same length as arguments.
#' @export classify
classify = function(...) {
    ids = list(...)
    if (length(ids) == 1)
        return(Recall(ids[[1]], ids[[1]]))
    if (length(ids) >= 3)
        return(Recall(ids[[1]], do.call(Recall, ids[-1])))
    if (length(ids[[1]]) != length(ids[[2]]))
        stop("Arguments must be of equal length")
    if (length(ids[[1]]) == 0)
        return(integer(0))
    if (length(ids[[1]]) == 1)
        return(1L)
    # A base R implementation is faster for short arguments, but
    # with tens or hundreds of millions of elements, a data.table
    # implementation is roughly 50 % faster, depending on the amount
    # of unique values vs. the amount of total values.
    if (length(ids[[1]]) > 1000000) {
        # Find unique rows with data.table, key them
        # and join back to match original arguments.
        d = data.table::data.table(x=ids[[1]], y=ids[[2]])
        d$order = 1:nrow(d)
        data.table::setkey(d, x, y)
        # XXX: The by argument to unique should default to key(d),
        # but doesn't seem to work (data.table 1.10.0).
        du = unique(d, by=c("x", "y"))
        du$k = 1:nrow(du)
        d = d[du]
        setorder(d, order)
        return(d$k)
    } else {
        # Convert arguments to integers starting at one and
        # apply the Hopcroft-Ullman pairing function.
        # http://mathworld.wolfram.com/PairingFunction.html
        i = match(ids[[1]], sort(unique(ids[[1]]), na.last=TRUE))
        j = match(ids[[2]], sort(unique(ids[[2]]), na.last=TRUE))
        k = 0.5 * (i + j - 2) * (i + j - 1) + i
        k = match(k, sort(unique(k), na.last=TRUE))
        return(as.integer(k))
    }
}

#' Add prefix to column names.
#' @param x a matrix-like object.
#' @param prefix a character string.
#' @return A character vector.
#' @export colnames.prefix
colnames.prefix = function(x, prefix)
    gsub("^", prefix, colnames(x))

#' Add suffix to column names.
#' @param x a matrix-like object.
#' @param suffix a character string.
#' @return A character vector.
#' @export colnames.suffix
colnames.suffix = function(x, suffix)
    gsub("$", suffix, colnames(x))

#' Remove duplicate rows from a data frame.
#' @param x a data frame.
#' @param ... names of columns to judge duplication by as either
#' symbols or strings. If not given, all columns are used.
#' @return \code{x} with some rows removed.
#' @export dedup
dedup = function(x, ...) {
    stopifnot(is.data.frame(x))
    dots = match.call(expand.dots=FALSE)$...
    by = if (length(dots) > 0) {
        unlist(lapply(dots, as.character))
    } else colnames(x)
    missing = setdiff(by, colnames(x))
    if (length(missing) > 0)
        stop(sprintf("Columns not found: %s",
                     paste(missing, collapse=", ")))

    id = do.call(classify, x[,by,drop=FALSE])
    return(x[which(!duplicated(id)),,drop=FALSE])
}

#' Descending order.
#' @param x a vector.
#' @return A vector.
#' @export desc
desc = function(x)
    # Copied and adapted from plyr::desc,
    # http://github.com/hadley/plyr
    # Copyright Hadley Wickham, MIT-licensed.
    -xtfrm(x)

#' Create a data frame keeping strings as strings.
#' @param ... passed to \code{\link{data.frame}}.
#' @param stringsAsFactors \code{TRUE} to convert strings to factors.
#' @return A data frame.
#' @seealso \code{\link{data.frame}}
#' @export dfsas
dfsas = function(..., stringsAsFactors=FALSE)
    data.frame(..., stringsAsFactors=stringsAsFactors)

#' Join two data frames and subtract values of one column.
#' @param df0 data frame with baseline value for \code{name}.
#' @param df1 data frame with non-baseline value for \code{name}.
#' @param by names of columns to join \code{df0} and \code{df1} by.
#' @param name name of column in \code{df0} and \code{df1} to diff.
#' @param missing value to use observations missing from one data frame.
#' @return A data frame with columns \code{by} and \code{name}.
#' @method diff.incomplete not.an.s3.method
#' @export diff.incomplete
diff.incomplete = function(df0, df1, by, name, missing=0) {
    df0[,name] = -df0[,name]
    return(sum.incomplete(df0, df1, by, name, missing))
}

#' Use minimal classes for data frame columns.
#' @param df a data frame.
#' @return Data frame \code{df} with different column classes.
#' @export downclass
downclass = function(df) {
    for (col in which(sapply(df, is.numeric) & !sapply(df, is.integer))) {
        m = which(!is.na(df[,col]))
        if (all(df[m,col] %% 1 == 0) && all(df[m,col] < .Machine$integer.max))
            df[,col] = as.integer(df[,col])
    }
    if (nrow(df) > 1000000)
        gc()
    return(df)
}

#' Return directory of current file.
#' 
#' Use only when called from Rscript or sourced via R console!
#' @return A character string
#' @export file_location
file_location = function() {
    cmd_args = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmd_args)
    if (length(match) > 0) {
        # Rscript
        this_file = normalizePath(sub(needle, "", cmd_args[match]), winslash = "/")
    } else {
        # Sourced via R console
        this_file = normalizePath(sys.frames()[[1]]$ofile, winslash = "/")
    }
    directory = substr(this_file, 1, gregexpr("/", this_file) %>% unlist() %>% last() - 1)
    return(directory)
}

#' Sanitize filenames.
#' @param fname a character vector of filenames.
#' @return A character vector.
#' @export fname.clean
fname.clean = function(fname) {
    fname = tolower(fname)
    for (char in c(" "))
        fname = gsub(char, "_", fname)
    for (char in c(",", "'", '"'))
        fname = gsub(char, "", fname)
    return(fname)
}

#' Golden ratio dimensions.
#'
#' Given one length, \code{golden} will return that length divided by and
#' multiplied by the golden ratio, i.e. length of the other side of a golden
#' rectangle in the case of \code{x} being the longer side and \code{x} being
#' the shorter side. To get the ratio, use \code{golden(1)}.
#' @param x a numeric vector of length one.
#' @return A numeric vector of length two. Use \code{\link{min}} and
#' \code{\link{max}} to extract the wanted element.
#' @export golden
golden = function(x) {
    if (length(x) > 1) {
        warning("Only the first element of 'x' will be used")
        x = x[1]
    }
    ratio = (1 + sqrt(5)) / 2
    return(c((x / ratio), (x * ratio)))
}

#' Convert factors to integer without loss of information
#' 
#' @param x a vector containing factors.
#' @return vector \code{x} converted to integers
#' @export factor2int
factor2int = function(x) {
    return(as.integer(levels(x))[x])
} 

#' Convert factors to numeric without loss of information
#' 
#' @param x a vector containing factors.
#' @return vector \code{x} converted to numerics.
#' @export factor2num
factor2num = function(x) {
    return(as.numeric(levels(x))[x])
}

#' Convert factors to character without loss of information
#' 
#' @param x a vector containing factors.
#' @return vector \code{x} converted to characters
#' @export factor2chr
factor2chr = function(x) {
    return(as.character(levels(x))[x])
}

#' Show HTML help in browser.
#' @param ... passed to \code{\link{help}}.
#' @export help.html
help.html = function(...)
    utils::help(..., help_type="html")

#' Test whether or not input is divisible by two.
#' @param x a numeric vector.
#' @return A logical vector.
#' @export is.even
is.even = function(x)
    (x %% 2) == 0

#' Test whether or not input is divisible by two.
#' @param x a numeric vector.
#' @return A logical vector.
#' @export is.odd
is.odd = function(x)
    (x %% 2) == 1

#' List objects in the \pkg{strafica} package.
#' @return A character vector.
#' @export ls.strafica
ls.strafica = function()
    objects("package:strafica")

#' Return fraction of total memory in use.
#'
#' Return value is based on parsing output from the system command free.
#' In addition to free, awk is required. Return -1 in case of error.
#' Always return -1 on Windows.
#' @return Amount of total memory used, numeric between 0 and 1.
#' @export mem.used
mem.used = local({
    # Cache return values to avoid calling free too often,
    # in case we get called on every iteration of a loop.
    read.buffers = NA
    used = -1
    utime = -1
    function() {
        if (.Platform$OS.type == "windows") return(-1)
        time = .Internal(Sys.time())
        if (time < utime) return(used)
        if (is.na(read.buffers))
            # The return value of free has changed in some recent version
            # of Linux. In older versions we need to read the buffers/cache
            # line, in newer versions we can use the Mem line.
            # https://unix.stackexchange.com/a/152301
            # https://bugzilla.redhat.com/show_bug.cgi?id=1158828
            read.buffers <<- system2(command="free",
                                     args="| grep buffers/cache:",
                                     stdout=NULL,
                                     stderr=NULL,
                                     wait=TRUE) == 0

        output = if (read.buffers) {
            system2(command="free",
                    args="| grep buffers/cache: | awk '{print $3/($3+$4)}'",
                    stdout=TRUE,
                    stderr=NULL)
        } else {
            system2(command="free",
                    args="| grep Mem: | awk '{print $3/$2}'",
                    stdout=TRUE,
                    stderr=NULL)
        }
        # Gives NA if output is non-numeric.
        num = as.numeric(output)
        used <<- if (!is.na(num)) num else -1
        # Schedule next update time stochastically
        # so that all parallel processes are not forking
        # a new free and awk process simultaneously.
        utime <<- time + stats::runif(1, 5, 30)
        return(used)
    }
})

#' Generate a formatted diagnostic message.
#' @param ... passed to \code{\link{sprintf}}.
#' @export messagef
messagef = function(...)
    message(sprintf(...))

#' Generate a formatted diagnostic message without a newline.
#' @param ... passed to \code{\link{sprintf}}.
#' @export messagef0
messagef0 = function(...)
    message(sprintf(...), appendLF=FALSE)

#' Average two color values.
#' @param x a color as understood by \code{\link{col2rgb}}.
#' @param y a color as understood by \code{\link{col2rgb}}.
#' @param pos interpolation position between \code{x} and \code{y}.
#' @return A color.
#' @export mix.colors
mix.colors = function(x, y, pos=0.5)
    grDevices::colorRampPalette(c(x, y), space="Lab")(100)[ceiling(pos*100)]

#' Create directory
#'
#' \code{mkdir} is an alias for \code{\link{dir.create}}.
#' @param ... passed to \code{\link{dir.create}}.
#' @export mkdir
mkdir = function(...)
    dir.create(...)

#' Replace \code{NA}s with zeros.
#' @param x a numeric vector or a data frame.
#' @param names column names to replace in if \code{x} is a data frame.
#' If omitted, defaults to replacing in all numeric columns.
#' @return \code{x} with zeros instead of \code{NA}s.
#' @rdname na.to.zero
#' @export
na.to.zero = function(x, names=NULL)
    UseMethod("na.to.zero", x)

#' @rdname na.to.zero
#' @method na.to.zero data.frame
#' @export
na.to.zero.data.frame = function(x, names=NULL) {
    cols = if (!is.null(names)) names
           else which(sapply(x, is.numeric))

    for (col in cols)
        x[,col] = na.to.zero(x[,col])
    return(x)
}

#' @rdname na.to.zero
#' @method na.to.zero integer
#' @export
na.to.zero.integer = function(x, names=NULL)
    replace(x, which(is.na(x)), 0L)

#' @rdname na.to.zero
#' @method na.to.zero numeric
#' @export
na.to.zero.numeric = function(x, names=NULL)
    replace(x, which(is.na(x)), 0)

#' Discard \code{NA} elements.
#' @param x a vector.
#' @return \code{x} with \code{NA} elements discarded.
#' @export non.na
non.na = function(x)
    x[which(!is.na(x))]

#' Test for platform.
#' @return \code{TRUE} if on Windows, \code{FALSE} otherwise.
#' @export on.windows
on.windows = function()
    .Platform$OS.type == "windows"

#' Pad string.
#' @param x a vector coercible to character.
#' @param pad a padding string.
#' @param n length of padded string.
#' @return A character vector.
#' @export pad
pad = function(x, pad=" ", n) {
    x = as.character(x)
    while (any(nchar(x) < n))
        x[nchar(x) < n] = paste0(pad, x[nchar(x) < n])
    return(x)
}

#' Join string by a newline separator.
#' @param ... passed on to \code{\link{paste}}.
#' @param sep separator string.
#' @return A character vector.
#' @export pasten
pasten = function(..., sep="\n")
    paste(..., sep=sep)

#' Clip to limits.
#'
#' All elements smaller than \code{xmin} will be raised to \code{xmin} and
#' all elements greater than \code{xmax} will be lowered to \code{xmax}.
#' @param x a numeric object.
#' @param xmin minimum to clip to.
#' @param xmax maximum to clip to.
#' @param na.rm \code{TRUE} to remove missing values.
#' @return A numeric object the size of \code{x}.
#' @seealso \code{\link{pmin}}, \code{\link{pmax}}
#' @export pclip
pclip = function(x, xmin=-Inf, xmax=Inf, na.rm=FALSE)
    pmax(xmin, pmin(xmax, x, na.rm=na.rm), na.rm=na.rm)

#' Pick a column-wise subset of a data frame.
#' @param x a data frame.
#' @param ... names of columns to keep as either symbols or strings.
#' @return A data frame.
#' @seealso \code{\link{unpick}}
#' @export pick
pick = function(x, ...) {
    dots = match.call(expand.dots=FALSE)$...
    keep = unlist(lapply(dots, as.character))
    missing = setdiff(keep, colnames(x))
    if (length(missing) > 0)
        stop(sprintf("Columns not found: %s",
                     paste(missing, collapse=", ")))

    # Set to NULL to make sure memory is freed,
    # but also select to return in asked order.
    x[setdiff(colnames(x), keep)] = list(NULL)
    if (nrow(x) > 1000000) gc()
    return(x[,keep,drop=FALSE])
}

#' Collect garbage after another call.
#'
#' Use as e.g. post.gc(rm(big.object)).
#' @param x returned after calling \code{\link{gc}}.
#' @return \code{x} given.
#' @export post.gc
post.gc = function(x) {
    gc(verbose=FALSE)
    return(x)
}

#' Get working directory.
#'
#' \code{pwd} is an alias for \code{\link{getwd}}.
#' @param ... passed to \code{\link{getwd}}.
#' @export pwd
pwd = function(...)
    getwd(...)

#' Rename columns in data frame.
#' @param df a data frame.
#' @param ... named argument pairs of form \code{from=to}, e.g.
#' \code{rename(df, x=y)} to rename \code{x} to \code{y} in \code{df}.
#' Strings and function calls are allowed on the right side, e.g.
#' \code{rename(df, x="y")} or \code{rename(df, x=sprintf(...))}.
#' @return A data frame.
#' @export rename
rename = function(df, ...) {
    dots = match.call(expand.dots=FALSE)$...
    from = names(dots)
    if (all(grepl("^[a-zA-Z0-9._]+$", dots))) {
        # Convert symbol to character.
        to = unlist(lapply(dots, as.character))
    } else {
        # Evaluate expression (e.g. a sprintf call).
        to = unlist(as.list(list(...)))
    }
    if (any(to %in% colnames(df))) {
        # If any of the new names already exist, remove them.
        # Any interdependent sequentiality will fail.
        repl = paste(to[to %in% colnames(df)], collapse=", ")
        warning(sprintf("Replacing existing columns: %s", repl))
        df = df[,setdiff(colnames(df), to),drop=FALSE]
    }
    names = colnames(df)
    for (i in seq_along(from))
        names[colnames(df) == from[i]] = to[i]
    colnames(df) = names
    return(df)
}

#' Sequence of rows.
#'
#' Primary intended use case of \code{rows.along} is in
#' \code{for}-loops over the rows of a matrix or a data frame, where
#' the amount of rows is not known beforehand and might be zero
#' and thus 1:nrow(x) would be dangerous.
#' @param x a matrix-like object.
#' @return An integer vector.
#' @export rows.along
rows.along = function(x) {
    if (!is.null(x) && length(x) > 0) {
        n = nrow(x)
        if (!is.null(n) && n > 0)
            return(1:n)
    }
    return(integer(0))
}

#' Evaluate string with variables expanded.
#'
#' Variable definitions are of form \code{\{VAR:FORMAT\}},
#' where \code{FORMAT} corresponds to \code{\link{sprintf}} formats
#' without the leading percent sign. If \code{:FORMAT} is missing,
#' string format (\code{s}) is assumed. See examples for details.
#' @param ... strings to expand variables in.
#' @param sep separator to join \code{...} by.
#' @param envir environment within which to evaluate strings.
#' @return A string.
#' @examples
#' # Format value.
#' x = pi
#' sexpf("{x:.6f}")
#' # Format specification not needed for strings,
#' # or anything that cleanly renders to a string.
#' x = "foo"
#' sexpf("{x}")
#' # Split code to multiple lines.
#' x = 1
#' y = 2
#' z = 3
#' sexpf("{x:d}",
#'       "{y:d}",
#'       "{z:d}",
#'       sep=", ")
#'
#' # Use double curly braces to avoid expansion.
#' size = 12L
#' sexpf("body {{ font-size: {size}px; }}")
#' @export sexpf
sexpf = function(..., sep=" ", envir=parent.frame()) {
    string = paste(..., sep=sep)
    match = gregexpr("(^|[^}])\\{[^{}]+\\}", string)[[1]]
    if (all(match < 0)) return(string)
    for (i in rev(seq_along(match))) {
        a = match[i]
        a = if (substr(string, a, a) == "{") a else a+1
        z = match[i] + attr(match, "match.length")[i] - 1
        template = substr(string, a, z)
        template = substr(template, 2, nchar(template)-1)
        template = strsplit(template, ":")[[1]]
        if (length(template) == 1 || template[2] == "")
            # Default to string formatting.
            template = c(template[1], "s")
        value = eval(parse(text=template[1]), envir)
        fmt = paste0("%", template[2])
        string = paste0(substr(string, 1, a-1),
                        sprintf(fmt, value),
                        substr(string, z+1, nchar(string)))

    }
    string = gsub("{{", "{", string, fixed=TRUE)
    string = gsub("}}", "}", string, fixed=TRUE)
    return(string)
}

#' Evaluate string with variables expanded.
#'
#' See \code{\link{sexpf}} for documentation.
#' @param ... strings to expand variables in.
#' @param sep separator to join \code{...} by.
#' @param envir environment within which to evaluate strings.
#' @return A string.
#' @export sexpf0
sexpf0 = function(..., sep="", envir=parent.frame())
    sexpf(..., sep=sep, envir=envir)

#' Values of slots.
#' @param objs a vector of objects with slots.
#' @param name a string, name of a \code{\link{slot}}.
#' @return A vector of the values of slots of all objects.
#' @export slots
slots = function(objs, name)
    sapply(objs, function(x) methods::slot(x, name))

#' Read R code from files.
#'
#' Read and \code{\link{source}} all files that match \code{pattern}.
#' @param pattern a glob pattern of filenames.
#' @seealso \code{\link{Sys.glob}} for glob patterns.
#' @export source.glob
source.glob = function(pattern="*.R") {
    lapply(Sys.glob(pattern), source.message)
    return(invisible())
}

#' Read R code from file.
#' @param file name of file to read.
#' @param ... passed to \code{\link{source}}.
#' @export source.message
source.message = function(file, ...) {
    message(file)
    return(source(file, ...))
}

#' Ensure the truth of R expressions.
#' @param ... expressions that should evaluate to \code{FALSE}.
#' @seealso \code{\link{stopifnot}}
#' @export stopif
stopif = function(...) {
    mc = match.call()
    args = list(...)
    .stop = function(x)
        stop(sprintf("%s is (any) TRUE", deparse(x)), call.=FALSE)
    for (i in seq_along(args))
        tryCatch(
            stopifnot(!args[[i]]),
            error=function(e) .stop(mc[[i+1]]))
    return(invisible())
}

#' Wrap strings with newlines.
#' @param x a character vector.
#' @param width width to wrap to.
#' @param ... passed to \code{\link{strwrap}}.
#' @return A character vector.
#' @export strwrap.str
strwrap.str = function(x, width, ...)
    sapply(x, function(y) do.call(
        paste, c(as.list(strwrap(y, width, ...)), sep="\n")))

#' Subset data frame.
#' @param x data frame to be subsetted.
#' @param subset logical expression indicating rows to keep:
#' missing values are taken as false.
#' @param select expression, indicating columns to select.
#' @param drop passed on to \code{[} indexing operator.
#' @param ... not used.
#' @return A data frame.
#' @method subset data.frame
#' @export subset.data.frame
subset.data.frame = function(x, subset, select, drop=FALSE, ...) {
    # Faster version of subset.data.frame using integer indices.
    # Not wanted upstream. Only the trivial subset part implemented,
    # if the complicated select argument given, defer to base.
    # http://bugs.r-project.org/bugzilla/show_bug.cgi?id=15823
    # About roxygen tags: even though this is a S3 method for data.frame class,
    # writing `@export subset.data.frame` is compulsory to override the base
    # method!
    if (!missing(select))
        return(base::subset.data.frame(
            x, subset, select, drop))
    if (missing(subset))
        stop("At least one of subset, select needs to be given")
    m = eval(substitute(subset), x, parent.frame())
    return(x[which(m),,drop=drop])
}

#' Join two data frames and sum values of one column.
#' @param df0 data frame with first value for \code{name}.
#' @param df1 data frame with second value for \code{name}.
#' @param by names of columns to join \code{df0} and \code{df1} by.
#' @param name name of column in \code{df0} and \code{df1} to sum.
#' @param missing value to use observations missing from one data frame.
#' @return A data frame with columns \code{by} and \code{name}.
#' @method sum.incomplete not.an.s3.method
#' @export sum.incomplete
sum.incomplete = function(df0, df1, by, name, missing=0) {
    if (any(duplicated(do.call(classify, df0[,by,drop=FALSE]))))
        warning("Duplicate values of 'by' in 'df0'")
    if (any(duplicated(do.call(classify, df1[,by,drop=FALSE]))))
        warning("Duplicate values of 'by' in 'df1'")
    df0$value0 = df0[,name]
    df1$value1 = df1[,name]
    df0 = df0[,c(by, "value0"), drop=FALSE]
    df1 = df1[,c(by, "value1"), drop=FALSE]
    df = fulljoin(df0, df1, by=by)
    df$value0[is.na(df$value0)] = missing
    df$value1[is.na(df$value1)] = missing
    df[,name] = df$value1 + df$value0
    return(df[, c(by, name), drop=FALSE])
}

#' Invoke an OS command.
#' @param command the system command to invoke.
#' @param ... passed to \code{\link{system}}.
#' @export system.message
system.message = function(command, ...) {
    message(command)
    return(system(command, ...))
}

#' Print the call stack of the last uncaught error.
#'
#' \code{tb} is an alias for \code{\link{traceback}}.
#' @param ... passed to \code{\link{traceback}}.
#' @export tb
tb = function(...)
    traceback(...)

#' Find indices of unique values.
#'
#' \code{uind} is much like \code{split(seq_along(x), x)},
#' but without using factors and thus also having the return
#' value in increasing order of index rather than factor level.
#' @param x a vector of values.
#' @return A list of indices in \code{x}.
#' @export uind
uind = function(x) {
    # Transform x so that unique values are increasing.
    y = match(x, unique(x))
    return(split(seq_along(y), y))
}

#' Pick a column-wise subset of a data frame.
#' @param x a data frame.
#' @param ... names of columns to discard as either symbols or strings.
#' @return A data frame.
#' @seealso \code{\link{pick}}
#' @export unpick
unpick = function(x, ...) {
    dots = match.call(expand.dots=FALSE)$...
    trash = unlist(lapply(dots, as.character))
    x[trash] = list(NULL)
    if (nrow(x) > 1000000) gc()
    return(x)
}

#' Discard \code{NA} and duplicate elements.
#' @param x a vector.
#' @param ... passed to \code{\link{unique}}.
#' @return \code{x} with \code{NA} and duplicate elements discarded.
#' @export unique.non.na
unique.non.na = function(x, ...)
    unique(x[which(!is.na(x))], ...)

#' Invoke a viewer.
#'
#' View data with \code{\link{edit}}, but discard return value.
#' @param ... passed to \code{\link{edit}}.
#' @export view
view = function(...)
    invisible(utils::edit(...))
