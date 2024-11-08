# Function for converting .APT files to .pos
# Will read .APT file and then save as .pos in same file location
# Will only save, x, y, z, m information from .APT file

ConvertAPTtoPOS <- function(APTFilePath){
  writeposR(readAPTData(APTFilePath) %>%   # Read
              select(x, y, z, Mass) %>%  # Select x, y, z, Mass
              rename(m = Mass),
            gsub(".apt",".pos",APTFilePath))
}
