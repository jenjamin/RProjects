readposR <- function(posFileName) {
  # size of floats = 4 bytes
  sizeOfFloat = 4
  # get file size
  fileInfo <- file.info(posFileName)
  fileSize <- fileInfo$size / sizeOfFloat
  to.read = file(posFileName, "rb")
  posFile <- readBin(to.read,
                     double(),
                     size = 4,
                     n = fileSize,
                     endian = "big")
  close(to.read)
  posFile <-
    t(matrix(posFile, nrow = 4, dimnames = list(c("x", "y", "z", "m"))))
  posFile <- as.data.frame(posFile)
  return(posFile)
}