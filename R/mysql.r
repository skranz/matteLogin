test.mysql=function() {
  setwd("D:/libraries/matteLogin")
  library(DBI)
  library(RMySQL)

  db.arg = matte.db.arg(dbname="testdb",drv=MySQL(),username="admin",password="test")

  db.arg = matte.db.arg(dbname="testdb",drv=SQLite())

  matte = make.matte(db.arg = db.arg)
  set.matte(matte)
  conn = matte.create.db(matte = matte, overwrite=TRUE)

  user = list(userid="jon", email="sebkranz@gmail.com", confirmed=TRUE)
  user = set.list(user,make.password.hash(password= "password"))
  user = matte.insert.user(conn=conn,user=user, mode="replace")

  matte.get.users(conn)


}
