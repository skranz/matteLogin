#
#   .old <- c(LETTERS, letters, 0:9, " ",":","'",'"',",",";",".","\n","\t","?","!","@","*","+","_")
#   .new <- sample(.old)
#
#   old <- paste(.old, collapse = "")
#   new <- paste(.new, collapse = "")

# No security at all. Just reduces probability that people by chance read your file
poor.encrypt <- function(message) {
  old = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 :'\",;.\n\t?!@*+_"
  new = "os'w;0@bJnYMZ5d!uv\tk9T:LONhq72P.y+iGKzEt41 jrecD8lH\"a*U63gI_pCmBfAS\nQVF?WRXx,"
  chartr(old, new, message)
}

poor.decrypt <- function(message) {
  old = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 :'\",;.\n\t?!@*+_"
  new = "os'w;0@bJnYMZ5d!uv\tk9T:LONhq72P.y+iGKzEt41 jrecD8lH\"a*U63gI_pCmBfAS\nQVF?WRXx,"
  chartr(new, old, message)
}

examples.poor.encrypt.sender = function() {
  setwd("D:/libraries/matteLogin")
  poor.encrypt.sender()
}

# no serios security at all... file should be at a secure location
poor.encrypt.sender <- function(file="sender.yaml", dir=getwd(), out="sender.txt") {
  txt  = readLines(paste0(dir,"/",file))
  enc  = poor.encrypt(paste0(txt, collapse="\n"))
  writeLines(enc,paste0(dir,"/",out))
  enc
}
