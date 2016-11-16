ffmpeg <- function(dir=".", pattern, file, glob=TRUE, codec="h264", start=0){
  # input files
  iglob <- "-pattern_type glob -i "
  input <- file.path(dir, pattern)
  input <- ifelse(glob, paste0(iglob, "'", input, "'"), paste("i", input))
  input <- paste("-start_number", start, input)

  #video codec
  vc <- paste("-c:v", codec)
  x <- paste("ffmpeg", start, input, vc, file)
  system(x)
  x
}
