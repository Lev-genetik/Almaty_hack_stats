#' Determine if a string is a file path or a ChatGPT API key
#'
#' This function checks whether the provided string corresponds to an 
#' existing file path, matches the pattern of an OpenAI ChatGPT API key, 
#' or is unknown.
#'
#' @param x A character string to be tested.
#'
#' @return A character string indicating the type: 
#'   \itemize{
#'     \item `"file"` if the input is an existing file path.
#'     \item `"chatgpt_key"` if the input matches the OpenAI API key pattern.
#'     \item `"unknown"` otherwise.
#'   }
#'
#' @examples
#' is_file_or_key("path/to/myfile.txt")
#' is_file_or_key("sk-abcdefghijklmnopqrstuvwxyz1234567890abcd")
#' is_file_or_key("randomstring")
#'
#' @export
is_file_or_key <- function(x) {
  assertthat::assert_that(
    is.character(x), 
    length(x) == 1,
    !is.na(x)
  )
  
  if (file.exists(x)) {
    return("file")
  }
  
  # Check if it matches OpenAI API key format: sk- followed by 48+ letters/numbers
  if (grepl("^sk-[A-Za-z0-9]{48,}$", x)) {
    return("chatgpt_key")
  }
  
  return("unknown")
}
