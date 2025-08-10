is_file_or_key <- function(x) {
  if (file.exists(x)) {
    return("file")
  }
  
  # Check if it matches OpenAI API key format: sk- followed by 48+ letters/numbers
  if (grepl("^sk-[A-Za-z0-9]{48,}$", x)) {
    return("chatgpt_key")
  }
  return("unknown")
}