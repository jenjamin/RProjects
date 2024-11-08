# Method to print object properties
print.APTFileHeader2 <- function(obj) {
  cat("cSignature:", obj$cSignature, "\n")
  cat("iHeaderSize:", obj$iHeaderSize, "\n")
  cat("iHeaderVersion:", obj$iHeaderVersion, "\n")
  cat("wcFilename:", obj$wcFilename, "\n")
  cat("ftCreationTime:", obj$ftCreationTime, "\n")
  cat("llIonCount:", obj$llIonCount, "\n")
}
