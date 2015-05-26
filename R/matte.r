
make.matte = function(db.arg=matte.db.arg(),conn=NULL,login.fun=NULL, signup.fun = NULL, check.email.fun=NULL, app.url = NULL, app.title="matteLogin",
    login = matte.login(...),
    crem = matte.crem(...),
    crepa = matte.crepa(...), ...)
{
  restore.point("make.matte")
  matte = list(
    app.title = app.title,
    app.url = app.url,
    db.arg = db.arg,
    conn = conn,
    login.fun = login.fun,
    signup.fun = signup.fun,
    check.email.fun = check.email.fun,
    login = login,
    crem = crem,
    crepa = crepa
  )
  if (!is.null(matte$sender.file)) {
    sender.txt = readLines(matte$sender.file)
    txt = poor.decrypt(sender.txt)
    matte$sender = yaml.load(txt)
  }

  as.environment(matte)
}

get.matte = function(app=getApp(), field="..MATTE.LOGIN") {
  app[[field]]
}

set.matte = function(matte,app=getApp(), field="..MATTE.LOGIN") {
  app[[field]] = matte
}

matte.db.arg = function(dbname="testdb",drv=SQLite(),...) {
  args = list(...)
  fill.defaults(args, nlist(dbname,drv))
}

show.html.message = function(id,msg="") {
  cat("\nhtml.message: ", msg)
  setUI(id,HTML(msg))
}

show.html.warning = function(id,msg="", color="red") {
  cat("\nhtml.warning: ", msg)
  html = paste0('<bold><font color="',color,'">',msg,'</font></bold>')
  setUI(id,HTML(html))
}

matte.create.link = function(userid,linkType="confirm", matte=get.matte()) {

  linkid = make.salt(50)
  url = paste0(matte$app.url,"/?confirm=",linkid)
  createTime = Sys.time()
  valid


  link = nlist(linkid, userid, url, l)

}
