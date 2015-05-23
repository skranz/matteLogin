examples.matte.login = function() {

  make.password.hash(user="jondoe",password= "password")
  res = make.password.hash(user="jondoe",password= "password")
  pw.df = as.data.frame(res)
  app = eventsApp()

  login.fun = function(app=getApp(),user,matte,...) {
    cat("Successfully logged in as ", user)
    setUI("mainUI", success.ui)
  }

  signup.fun = function(app=getApp(), matte,...) {
    cat("\nSign up...\n")
    ui = matte.create.email.user.ui(matte)
    setUI("mainUI", ui)
  }

  check.email.fun = function(email="",...) {
    if (!isTRUE(email=="???" |
                email=="???")) {
      return(list(ok=FALSE, msg="Please only send to your own email adresses!"))
    }
  list(ok=TRUE,msg="")
  }

  sender.file = "D:/libraries/matteLogin/sender.txt"
  login = matte.login()
  matte = make.matte(login=login, sender.file=sender.file, login.fun=login.fun, signup.fun=signup.fun, check.email.fun=check.email.fun,app.url="http://127.0.0.1:4915")
  matte$login$ui = matte.login.ui(matte)

  matte.set.pw.df(matte,pw.df)

  success.ui = wellPanel(
    actionButton("successBtn", "Success... log in again")
  )
  buttonHandler("successBtn", function(app,...) {
    setUI("mainUI",matte$login$ui)
  })


  ui = fluidPage(uiOutput("mainUI"))
  setUI("mainUI",matte$login$ui)
  #setUI("mainUI",success.ui)
  runEventsApp(app,ui = ui, launch.browser=rstudio::viewer)
}

matte.set.pw.df = function(matte,pw.df=NULL,app=getApp()) {
  app[[matte$pw.df.field]]=pw.df
}

matte.get.pw.df = function(matte,app=getApp()) {
  app[[matte$pw.df.field]]
}


make.matte = function(pw.file=NULL, sender.file=NULL, login.fun=NULL, signup.fun = NULL, check.email.fun=NULL, app.url = NULL, app.title="matteLogin",
    login = matte.login(...),
    crem = matte.crem(...),
    crepa = matte.crepa(...), ...)
{
  restore.point("make.matte")
  matte = list(
    app.title = app.title,
    app.url = app.url,
    pw.file = pw.file,
    sender.file = sender.file,
    pw.df.field = "pw.df",
    user.name.field = "user.name",
    user.email.field = "user.email",
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

matte.login = function(failure.fun=matte.failed.login, create.user.fun=NULL, ui=NULL,...) {
  login = list(
    user.label="User",
    password.label="Password",
    login.button.label="log in",
    signup.button.label="sign up",

    user.id="matte.login.user",
    password.id="matte.login.password",
    login.button.id = "matte.login.btn",
    signup.button.id = "matte.login.signup.btn",
    alert.id="matte.login.alert",
    failure.fun=failure.fun
  )
  login
}

matte.login.ui = function(matte, create.user = TRUE,...) {
  copy.into.env(source = matte$login)

  widgets = list(
    textInput(user.id, user.label, value = ""),
    passwordInput(password.id, password.label, value = ""),
    actionButton(login.button.id, login.button.label),
    actionButton(signup.button.id, signup.button.label),
    bsAlert(alert.id)
  )
  ui = wellPanel(widgets)

  buttonHandler(login.button.id,matte.login.button.click, matte=matte)
  buttonHandler(signup.button.id,matte.signup.button.click, matte=matte)
  ui
}


matte.signup.button.click = function(app=getApp(),matte,...) {
  if (!is.null(matte$signup.fun)) {
    matte$signup.fun(matte=matte,...)
  }
}


matte.login.button.click = function(app=getApp(),matte,...) {
  login = matte$login
  user = getInputValue(login$user.id)
  password = getInputValue(login$password.id)

  res = check.password(user = user,password = password,pw.df = login$pw.df)
  if (res==TRUE) {
    matte$login.fun(user=user, password=password, matte=matte)
  } else {
    login$failure.fun(user=user, password=password, matte=matte)
  }
}

matte.failed.login = function(app=getApp(),matte,...) {
  login = matte$login
  createAlert(app$session, login$alert.id,title="Log-in failed",content="Wrong user or wrong password.", style="warning")
  cat("\nlog-in failed")
}
