# -*- coding: us-ascii-unix -*-

#' Read from MS Access database.
#'
#' Uses ODBC, will fail on non-Windows systems.
#' @param fname name of database file.
#' @param table name of table.
#' @param query SQL-query to use to fetch data. If not given, all
#' data in table is read using \code{"SELECT * FROM table"}.
#' @param ... passed to \code{\link[RODBC]{sqlQuery}}.
#' @return A data frame.
#' @export read.mdb
read.mdb = function(fname, table=NULL, query=NULL, ...) {
    if (!is.character(query)) {
        if (!is.character(table))
            stop("Must specify either 'table' or 'query'")
        query = sprintf("SELECT * FROM %s", table)
    }
    channel = RODBC::odbcConnectAccess(fname)
    messagef("%s: %s", fname, query)
    df = RODBC::sqlQuery(channel, query, ...)
    close(channel)
    return(df)
}

#' Names of tables in MS Access database.
#'
#' Uses ODBC, will fail on non-Windows systems.
#' @param fname name of database file.
#' @return A character vector.
#' @export tables.mdb
tables.mdb = function(fname) {
    channel = RODBC::odbcConnectAccess(fname)
    tables = RODBC::sqlTables(channel, tableType="TABLE")$TABLE_NAME
    close(channel)
    return(tables)
}
