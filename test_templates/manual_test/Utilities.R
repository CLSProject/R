paste_chars_to_string <- function(chars) {
    paste0(chars, collapse = "")
}
split_string_to_chars <- function(string) {
    return (strsplit(string, "")[[1]])
}
