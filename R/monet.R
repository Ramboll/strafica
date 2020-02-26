# -*- coding: us-ascii-unix -*-

#' Insert data into a MonetDB database table.
#'
#' @param conn An open database connection created with
#'   \code{\link[DBI]{dbConnect}}.
#' @param table Name of table to insert into.
#' @param value A data frame to insert.
#' @param fout A path where to write a temporary CSV file if not the working
#'   directory. The path must be writable by the script.
#' @param fin A path from where a temporary CSV file is read to the database if
#'   not the working directory. The path must be readable by the database.
#' @return \code{TRUE} if successful.
#' @export insert.monet
insert.monet = function(conn, table, value, fout=NULL, fin=NULL) {
    # Large chunks of data are fastest inserted using COPY INTO.
    # MonetDB.R's dbWriteTable has a csvdump argument, which
    # should do this, but for some reason doesn't work.
    # TODO: Use data.table::fwrite added in 1.9.7.
    fname_out = ifelse(is.null(fout),
                       file.path(getwd(), "monet.csv"),
                       file.path(fout, "monet.csv"))
    fname_in = ifelse(is.null(fin),
                      file.path(getwd(), "monet.csv"),
                      file.path(fin, "monet.csv"))
    utils::write.table(value,
                       fname_out,
                       sep=",",
                       quote=TRUE,
                       row.names=FALSE,
                       col.names=FALSE,
                       na="",
                       fileEncoding="UTF-8")
    
    DBI::dbSendStatement(conn, sexpf(
        "COPY INTO {table}",
        "FROM '{fname_in}'",
        "USING DELIMITERS ',','\\n','\"' NULL AS ''"
    ))
    unlink(fname_out)
    return(TRUE)
}
