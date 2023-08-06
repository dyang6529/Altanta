genPiAppxDigits <- function(numdigits, appxAcc) {
  
  precBits <- numdigits * log2(10)  # Conversion from decimal digits to binary bits
  
  # Create an mpfr vector to hold the sequence
  seqVec <- mpfr(seq(1, 2*appxAcc, by = 2), precBits = precBits)
  
  # Calculate pi_estimate
  pi_estimate <- sum((-1)^(1:appxAcc) / seqVec)
  
  # Convert to string with desired number of digits
  pi_string <- format(4 * pi_estimate, digits=numdigits)
  # Remove the decimal point
  pi_string <- gsub("\\-", "", pi_string)
  pi_string <- gsub("\\.", "", pi_string)
  
  # Split the string into individual characters
  split_pi <- strsplit(pi_string, "")
  
  # Convert the list to a vector
  pie <- data.frame(as.integer(unlist(split_pi)))
  
  return(pie)
}