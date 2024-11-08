writeposR <- function(posData,posFileName){
  # size of floats = 4 bytes
  sizeOfFloat = 4
  to.write = file(posFileName, "wb")
  reshaped <- matrix(t(posData),ncol=1)
  writeBin(object=as.double(reshaped),
           con=to.write,
           size=sizeOfFloat,
           endian="big",
           useBytes = FALSE)
  close(to.write)
}