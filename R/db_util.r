
dbInsert = function(conn, table, vals,sql=NULL,run=TRUE, mode=c("insert","replace")[1]) {
  restore.point("dbInsert")
  cols = names(vals)

  if (is.null(sql)) {
    sql <- paste0(mode, " into ", table," values (",
      paste0(":",cols,collapse=", "),")")
  }
  if (!run) return(sql)
  ret = dbSendQuery(conn, sql, params=vals)
  invisible(ret)
}

dbGetRow = function(conn, table, params, sql=NULL) {
  restore.point("dbGetRow")
  if (is.null(sql)) {
    if (length(params)==0) {
      where = ""
    } else {
      where = paste0(" where ", paste0(names(params)," = :",names(params), collapse= " AND "))
    }
    sql = paste0('select * from ', table, where)
  }
  rs = dbSendQuery(conn, sql, params=params)
  res = dbFetch(rs)
  if (NROW(res)==0) return(NULL)
  res
}

dbCreateSchemaTables = function(conn,schema=NULL, schema.file=NULL, overwrite=FALSE) {
  restore.point("dbCreateSchemaTables")

  if (is.null(schema)) {
    scheme = yaml.load_file(schema.file)
  }

  tables = names(schema)
  lapply(tables, function(table) {
    s = schema[[table]]
    if (overwrite)
      try(dbRemoveTable(conn, table), silent=TRUE)
    if (!dbExistsTable(conn, table)) {
      # create table
      sql = paste0("CREATE TABLE ", table,"(",
        paste0(names(s$table), " ", s$table, collapse=",\n"),
        ")"
      )
      dbSendQuery(conn,sql)

      # create indexes
      for (index in s$indexes) {
        try(dbSendQuery(conn,index))
      }
    }
  })
  invisible(scheme)
}
