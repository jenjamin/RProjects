#### Create class for APTFileHeader####
# Define the APTFileHeader2 class
APTFileHeader2 <- function(fid) {
  # Initialize the object with properties
  objForFileHeader <- list(
    healthy = TRUE,
    cSignature = "",
    iHeaderSize = 0,
    iHeaderVersion = 0,
    wcFilename = "",
    ftCreationTime = 0,
    llIonCount = 0
  )

  # Constructor logic to read the file header
  if (!missing(fid)) {
    # Read the first 4 bytes and convert them to a character string
    raw_signature <- readBin(fid, what = "raw", n = 4) # Read 4 bytes
    objForFileHeader$cSignature <- rawToChar(raw_signature) # Convert raw bytes to character

    # Validate file signature
    if (objForFileHeader$cSignature != "APT") {
      objForFileHeader$healthy <- FALSE
      cat("File is not a valid APT file!\n")
      return(objForFileHeader)
    }

    # Read the header size and version
    objForFileHeader$iHeaderSize <- readBin(fid, what = "integer", n = 1, size = 4, endian = "little")
    objForFileHeader$iHeaderVersion <- readBin(fid, what = "integer", n = 1, size = 4, endian = "little")

    # Read the filename (up to 256 bytes) as UTF-16
    filename_raw <- readBin(fid, what = "raw", n = 512) # 256 characters in UTF-16
    objForFileHeader$wcFilename <- paste(rawToChar(filename_raw, multiple = TRUE), collapse = "")

    # Read creation time and ion count
    objForFileHeader$ftCreationTime <- readBin(fid, what = "integer", n = 1, size = 8, endian = "little")
    objForFileHeader$llIonCount <- readBin(fid, what = "integer", n = 1, size = 8, endian = "little")

    cat("Reading *.APT file header was successful\n")
    cat(objForFileHeader$llIonCount, "ions\n")
  }

  # Assign class
  class(objForFileHeader) <- "APTFileHeader2"
  return(objForFileHeader)
}
