examples.db = function() {
  library(RSQLite)

  app = eventsApp()
  matte = make.matte()
  set.matte(matte)

  #conn = matte.create.db(overwrite=TRUE)
  conn = matte.connect.db(matte)

  matte.get.user("Seb")

  user = list(userid="jon", email="sebkranz@gmail.com", confirmed=FALSE)
  user = matte.insert.user(conn=conn,user=user, mode="replace")

  users = matte.fetch.users(conn)

  user = set.list(user,make.password.hash(password= "password"))
  user = matte.insert.user(conn=conn,user=user, mode="insert or replace")


  con <- dbConnect(SQLite(), ":memory:")
  data(USArrests)
  dbWriteTable(con, "USArrests", USArrests)

  rs <- dbSendQuery(con, "select * from USArrests")
  d1 <- fetch(rs, n = 10)      # extract data in chunks of 10 rows
  dbHasCompleted(rs)
  d2 <- fetch(rs, n = -1)      # extract all remaining data
  dbHasCompleted(rs)
  dbClearResult(rs)
  dbListTables(con)
  rs
  d1
}

matte.connect.db = function(db.arg=matte$db.arg,matte=get.matte()) {
  matte$conn = dbConnect(db.arg$drv, db.arg$dbname)
  invisible(matte$conn)
}

matte.create.db = function(db.arg=matte$db.arg, overwrite=FALSE, schema.file = paste0(path.package("matteLogin"),"/db/tables/matte_tables.yaml"),matte=get.matte()) {
  restore.point("matte.create.db")
  conn = do.call(dbConnect, db.arg)
  dbCreateSchemaTables(conn =conn,schema.file = schema.file,overwrite = overwrite)
  conn
}

matte.insert.user = function(conn=matte$conn,userid=NULL, email="",salt="",hash="",confirmed=NULL, created=as.integer(Sys.time()), user=list(), mode="replace", matte=get.matte()) {
  restore.point("matte.insert.user")
  user = fill.defaults(user, nlist(userid,email,salt,hash,created,confirmed))
  dbInsert(conn,"users",user, mode=mode)
  invisible(user)
}

matte.get.users = function(conn) {
  dbReadTable(conn,"users")
}

matte.get.user = function(userid,conn=matte$conn, matte=get.matte()) {
  restore.point("matte.get.user")

  dbGetRow(conn,table = "users",params = list(userid=userid))
}

examples.matte.insert.smtp = function() {
  setwd("D:/libraries/matteLogin")
  txt = paste0(readLines("sender.yaml"),collapse="\n")
  matte.create.db()
  matte.insert.smtp(txt)
  matte.get.smtp()
}

matte.insert.smtp = function(yaml,conn=matte$conn, matte=get.matte()) {
  restore.point("matte.insert.smtp")
  li =   yaml.load(yaml)
  dbInsert(conn,table = "smtp",vals = list(smptid=li$from,yaml=yaml),mode="replace")
}


matte.get.smtp = function(smtpid=NULL,conn=matte$conn, matte=get.matte()) {
  if (is.null(smtpid)) {
    dat = dbReadTable(conn,"smtp")
  } else {
    dat = dbGetSingleRow(conn, "smtp",list(smptid=smptid))
  }
  if (NROW(dat)==0) return(NULL)
  yaml = dat$yaml[[1]]
  yaml.load(yaml)
}
