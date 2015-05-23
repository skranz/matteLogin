
examples.make.password.hash = function() {
  make.password.hash(user="jondoe",password= "password")
  res = make.password.hash(user="jondoe",password= "password")
  pw.df = as.data.frame(res)

  check.password(user="jondoe",password="password",pw.df=pw.df)

}


make.salt = function(bytes=100) {
  salt = paste0(rand_bytes(bytes),collapse="")
  salt
}

make.password.hash = function(password, salt =  make.salt(), user=NULL, hash.fun=sha512) {
  raw = paste0(password, salt)
  hash = hash.fun(raw)
  if (is.null(user))
    return(list(hash=hash, salt=salt))
  return(list(user=user,hash=hash,salt=salt))
}

check.password = function(user,password,pw.df, hash.fun=sha512) {
  row = which(pw.df$user == user)
  if (length(row) == 0) {
    warning(paste0("User ", user, " not found in pw.df"))
    return(FALSE)
  }
  if (length(row)>1) {
    warning(paste0("Multiple users ", user, " found in pw.df."))
    return(FALSE)
  }

  stored.hash = pw.df$hash[row]
  act.hash = make.password.hash(password, salt=pw.df$salt[row], hash.fun=hash.fun)$hash
  if (stored.hash==act.hash)
    return(TRUE)
  return(FALSE)
}

random.password <- function(nchar=8, chars = c(setdiff(0:9,0), setdiff(letters,"o"), setdiff(LETTERS,"O")))
{
  paste0(sample(chars,nchar, replace=TRUE),collapse="")
}
