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
    create.button.label="Send email to confirm account",
    cancel.button.label="Cancel",

    user.id="matte.create.user",
    email.id="matte.create.email",
    password.id="matte.create.password",
    create.button.id = "matte.create.button.id",
    cancel.button.id = "matte.create.button.id",
    alert.id="matte.create.alert"
  )
}


matte.create.email.user.ui = function(matte, ...) {
  restore.point("matte.create.email.user.ui")
  copy.into.env(source = matte$crem)

  widgets = list(
    HTML(title),
    textInput(email.id, email.label, value = ""),
    #passwordInput(password.id, password.label, value = ""),
    actionButton(create.button.id, create.button.label),
    actionButton(cancel.button.id, cancel.button.label),
    bsAlert(alert.id)
  )
  ui = wellPanel(widgets)

  buttonHandler(create.button.id,create.email.user.click, matte=matte)
  ui
}

create.email.user.click = function(matte, passwd.len=6,...) {
  copy.into.env(source = matte$crem)
  user = email = getInputValue(email.id)
  restore.point("create.email.user.click")

  if (!is.null(matte$check.email.fun)) {
    res = matte$check.email.fun(email)
    if (!res$ok) {
      cat("\n", res$msg)
      return()
    }
  }

  link.code = make.salt(50)
  link = paste0(matte$app.url,"/?code=",link.code)

  subject = paste0("Confirm user account for ", matte$app.title)

  body = paste0("
Hi,

you get this email, because you want to sign-up as '", user, "' on ",matte$app.title," with this email adress. To confirm your user account and to choose a password, please follow the link below:\n\n ", link,""
  )

  mail = c(list(subject=subject,body=body,to=user), matte$sender)
  do.call(mailR::send.mail, mail)
  print(mail)
  cat("\n","I have send a confirmation email to ", email)
}


matte.crepa = function() {
  crepa = list(
    info1 = "<p>You can set a randomly generated password for your account. Press accept to set the generated password.</p>",
    email.label ="Email",
    create.passwd.button.label="Generate a password",
    cancel.button.label="Cancel",

    show.password.id="matte.show.password.id",
    create.button.id = "matte.create.passwd.button.id",
    accept.button.id = "matte.accept.passwd.button.id",
    alert.id="matte.create.passwd.alert"
  )
  crepa
}

matte.create.passwd.ui = function(matte, user, email=user, passwd.len=6, ...) {
  copy.into.env(source = matte$crepa)

  widgets = list(
    HTML(paste0("<p>User: <b>",user,"</b></p><br>")),
    HTML(info1),
    uiOutput(show.passwd.id),
    actionButton(create.button.id, create.passwd,button.label),
    actionButton(accept.button.id, cancel.button.label),
    bsAlert(alert.id)
  )
  ui = wellPanel(widgets)

  buttonHandler(create.button.id,generate.passwd.click, matte=matte,user=user, passwd.len=passwd.len)
  buttonHandler(accept.button.id,accept.passwd.click, matte=matte)
  ui
}

generate.passwd.click = function(matte, user, passwd.len=6,...) {
  copy.into.env(source = matte$crepa)
  passwd = random.password(passwd.len)
  setUI(show.passwd.id, HTML(
    paste0("<p>Generated password: <b>", passwd,"</b></p>")
  ))
}

accept.passwd.click = function(matte, user, passwd.len=6,...) {
  copy.into.env(source = matte$crepa)
}


send.user.creation.email = function(email,user=email) {


}

examples.send.email = function() {
  sender <- "ulm.econ.tools@gmail.com"
  recipients <- c("sebastian.kranz@uni-ulm.de")
  email <- send.mail(from = sender,
                   to = recipients,
                   subject="Test email",
                   body = "Hi!",
                   smtp = list(host.name = "smtp.gmail.com", port = 587,ssl=TRUE, user.name=sender, passwd="ulmecon1234"),
                   authenticate = TRUE,
                   send = TRUE)

  smtp.googlemail.com
}

