examples.make.user = function() {
  random.password(nchar=6)
}


matte.crem = function() {
  list(
    title = "<h3>Create new user account</h3>",
    info1.label = "",
    info2.label = "",
    user.label="User",
    password.label="Password",
    email.label ="Email",
    create.btn.label="Send email to confirm account",
    cancel.btn.label="Cancel",

    user.inp="matte.create.user",
    email.inp="matte.create.email",
    password.inp="matte.create.password",
    create.btn = "matte.create.btn",
    cancel.btn = "matte.create.btn",
    info="matte.create.info"
  )
}


matte.create.email.user.ui = function(matte, ...) {
  restore.point("matte.create.email.user.ui")
  copy.into.env(source = matte$crem)

  widgets = list(
    HTML(title),
    textInput(email.inp, email.label, value = ""),
    #passwordInput(password.inp, password.label, value = ""),
    actionButton(create.btn, create.btn.label),
    actionButton(cancel.btn, cancel.btn.label),
    uiOutput(info)
  )
  ui = wellPanel(widgets)

  buttonHandler(create.btn,create.email.user.click, matte=matte)
  ui
}

create.email.user.click = function(matte, passwd.len=6,...) {
  copy.into.env(source = matte$crem)
  user = email = getInputValue(email.inp)
  restore.point("create.email.user.click")

  if (is.null(matte$smtp)) {
    warning("matte$smpt not initialized")
    return(NULL)
  }

  if (!is.null(matte$check.email.fun)) {
    res = matte$check.email.fun(email)
    if (!res$ok) {
      show.html.warning(info,res$msg)
      return(NULL)
    }
  }


  link.code = make.salt(50)
  link = paste0(matte$app.url,"/?confirm=",link.code)

  subject = paste0("Confirm user account for ", matte$app.title)

  body = paste0("
Hi,

you get this email, because you want to sign-up on ",matte$app.title," with this email adress. To confirm your user account and to choose a password, please follow the link below:\n\n ", link,
"\n\nIf you have not registred on ",matte$app.title,", someone else unsuccessfully tried to sign up with your email address. Then please ignore this email."
  )

  mail = c(list(subject=subject,body=body,to=user), matte$smtp)
  #do.call(mailR::send.mail, mail)

  msg = paste0("I have send a confirmation email to ", email," from ",matte$smtp$from,".<br>The email contains a link to generate a password and activate your account.")
  show.html.message(info,msg)
}


matte.crepa = function() {
  crepa = list(
    info1 = "<p>You can set a randomly generated password for your account. Press accept to set the generated password.</p>",
    email.label ="Email",
    create.passwd.btn.label="Generate a password",
    cancel.btn.label="Cancel",

    show.password.inp="matte.show.password.inp",
    create.btn = "matte.create.passwd.btn",
    accept.btn = "matte.accept.passwd.btn",
    info="matte.create.passwd.info"
  )
  crepa
}

matte.create.passwd.ui = function(matte, user, email=user, passwd.len=6, ...) {
  copy.into.env(source = matte$crepa)

  widgets = list(
    HTML(paste0("<p>User: <b>",user,"</b></p><br>")),
    HTML(info1),
    uiOutput(show.passwd.inp),
    actionButton(create.btn, create.passwd,button.label),
    actionButton(accept.btn, cancel.btn.label),
    uiOutput(info)
  )
  ui = wellPanel(widgets)

  buttonHandler(create.btn,generate.passwd.click, matte=matte,user=user, passwd.len=passwd.len)
  buttonHandler(accept.btn,accept.passwd.click, matte=matte)
  ui
}

generate.passwd.click = function(matte, user, passwd.len=6,...) {
  copy.into.env(source = matte$crepa)
  passwd = random.password(passwd.len)
  setUI(show.passwd.inp, HTML(
    paste0("<p>Generated password: <b>", passwd,"</b></p>")
  ))
}

accept.passwd.click = function(matte, user, passwd.len=6,...) {
  copy.into.env(source = matte$crepa)
}


