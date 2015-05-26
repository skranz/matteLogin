examples.matte.login = function() {

  setwd("D:/libraries/matteLogin")
  app = eventsApp()

  login.fun = function(app=getApp(),userid,matte=get.matte(),...) {
    cat("Successfully logged in as ", userid)
    setUI("mainUI", success.ui)
  }

  signup.fun = function(app=getApp(), matte=get.matte(),...) {
    cat("\nSign up...\n")
    ui = matte.create.email.user.ui(matte)
    setUI("mainUI", ui)
  }

  check.email.fun = function(email="",...) {
    if (!isTRUE(email=="sebastian.kranz@uni-ulm.de" |
                email=="sebkranz@gmail.com")) {
      return(list(ok=FALSE, msg="Please only send to your own email adresses!"))
    }
  list(ok=TRUE,msg="")
  }

  sender.file = "D:/libraries/matteLogin/sender.txt"
  login = matte.login()
  db.arg = matte.db.arg(dbname="testdb",drv=SQLite())

  matte = make.matte(db.arg = db.arg,login=login, login.fun=login.fun, signup.fun=signup.fun, check.email.fun=check.email.fun,app.url="http://127.0.0.1:4915")
  set.matte( matte)
  matte.connect.db(matte=matte)
  matte$login$ui = matte.login.ui(matte)
  matte$smtp = matte.get.smtp()


  success.ui = wellPanel(
    actionButton("successBtn", "Success... log in again")
  )
  buttonHandler("successBtn", function(app,...) {
    show.html.message(matte$login$alert,"")
    setUI("mainUI",matte$login$ui)
  })


  ui = fluidPage(uiOutput("mainUI"))
  setUI("mainUI",matte$login$ui)
  runEventsApp(app,ui = ui, launch.browser=rstudio::viewer)

}


matte.login = function(failed.fun=matte.failed.login, create.user.fun=NULL, ui=NULL,...) {
  login = list(
    userid.label="User",
    password.label="Password",
    login.btn.label="log in",
    signup.btn.label="sign up",

    userid.inp="matte.login.user",
    password.inp="matte.login.password",
    login.btn = "matte.login.btn",
    signup.btn = "matte.login.signup.btn",
    alert="matte.login.alert",
    failed.fun=failed.fun
  )
  login
}

matte.login.ui = function(matte, create.user = TRUE,...) {
  copy.into.env(source = matte$login)

  widgets = list(
    textInput(userid.inp, userid.label, value = ""),
    passwordInput(password.inp, password.label, value = ""),
    actionButton(login.btn, login.btn.label),
    actionButton(signup.btn, signup.btn.label),
    uiOutput(alert)
  )
  ui = wellPanel(widgets)

  buttonHandler(login.btn,matte.login.btn.click, matte=matte)
  buttonHandler(signup.btn,matte.signup.btn.click, matte=matte)
  ui
}

matte.signup.btn.click = function(app=getApp(),matte,...) {
  if (!is.null(matte$signup.fun)) {
    matte$signup.fun(matte=matte,...)
  }
}

matte.login.btn.click = function(app=getApp(),matte=get.matte(),...) {
  login = matte$login
  userid = getInputValue(login$userid.inp)
  password = getInputValue(login$password.inp)


  res = matte.check.login(userid=userid,password = password, matte=matte)
  restore.point("matte.login.btn.click")
  if (res$ok==TRUE) {
    matte$login.fun(userid=userid, password=password, matte=matte)
  } else {
    login$failed.fun(userid=userid, password=password, msg=res$msg, matte=matte)
  }
}

matte.failed.login = function(app=getApp(),matte=get.matte(),msg,...) {
  login = matte$login
  show.html.warning(login$alert,msg)
  #createAlert(app$session, login$alert,title="Log-in failed",content=msg, style="warning")
  cat("\nlog-in failed: ",msg)
}

matte.check.login = function(userid, password, matte=get.matte()) {
  restore.point("matte.check.login")
  if (nchar(userid)==0)
    return(list(ok=FALSE,msg="No user name entered."))
  user = matte.get.user(userid=userid, matte=matte)
  if (NROW(user)==0) {
    return(list(ok=FALSE,msg="User does not exist."))
  }
  ok = check.password(password = password, salt=user$salt,hash=user$hash)
  if (ok) {
    return(list(ok=TRUE,msg=""))
  }
  return(list(ok=FALSE,msg="Wrong password."))
}
