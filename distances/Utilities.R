cat_vector <- function(vector) {
  cat(deparse(substitute(vector)))
  cat(": ", vector, "\n")
}

paste_chars_to_string <- function(chars) {
  paste0(chars, collapse = "")
}

press_any_key <- function() {
  input <- readline()
  stopifnot(input == "")
}

# print_anything <- function(anything) {
#   print(substitute(anything))
#   cat("\n")
#   print(anything)
#   cat("\n\n")
# }

# print_class <- function(object) {
#   print(class(object))
#   print(object)
# }

print_summary <- function(object) {
  printf("\n%s:\n", deparse(substitute(summary(object))))
  print(summary(object))
}

split_string_to_chars <- function(string) {
  return (strsplit(string, "")[[1]])
}

print_everything <- function(object) {
    print(object)
    cat("\n")
    cat("typeof\n")
  	print(typeof(object))
    cat("\n")
    cat("class\n")
		print(class(object))
    cat("\n")
    cat("str\n")
		print(str(object))
    cat("\n")
    cat("attributes\n")
		print(attributes(object))
    cat("\n")
  # print_anything(object)
  # print(typeof(object))
  # print(class(object))
  # print(str(object))
  # print(attributes(object))
  # print(names(object))
  # print_summary(object)
}
