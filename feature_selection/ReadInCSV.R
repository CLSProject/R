library(dplyr)


###
# reading CSV
find_patient_data_csv_files <- function() {
  wd <- getwd()
  list.files(wd, pattern = ".*[.]csv", full.names = TRUE)
}

has_found_csv_files <- function(found_files) {
  if (length(found_files) > 0) {
    return(TRUE)
  }
  FALSE
}

read_patient_data_csv <- function() {
  dir_csv_files <- find_patient_data_csv_files()
  if (! has_found_csv_files(dir_csv_files)) {
    break
  }
  info_dir_csv_files <- file.info(dir_csv_files)
  # most recent CSV file
  latest_csv_file_name <- rownames(info_dir_csv_files)[which.max(info_dir_csv_files$mtime)] # nolint: line_length_linter.
  read.csv(latest_csv_file_name)
}


###
# testing for special cases
check_for_missing_data <- function(data) {
  if (any(is.na(data))) {
    return(TRUE)
  }
  FALSE
}
check_for_only_numeric_data <- function(data) {
  if (any(sapply(data, is.numeric))) {
    return(TRUE)
  }
  FALSE
}


###
# processing patient data
expression_ids_as_indizes <- function(data) {
  rownames(data) <- data[, 1]
  select(data, -1)
}


extract_labels <- function(data) {
  # extracting labels
  c(data["labels", ])
}


convert_to_matrix <- function(data) {
  # dropping labels from patient data
  data <- data[rownames(data) != "labels", ]
  # converting to matrix
  data.matrix(data)
}


get_processed_patient_data <- function() {
  # patient_data <- read_patient_data_csv()
  patient_data <- read.csv(paste(getwd(),
                                 "/feature_selection/data",
                                 "/Colon_vs_Pancreas_selected.csv",
                                 sep = "", collapse = ""))

  patient_data <- expression_ids_as_indizes(patient_data)

  # extracting labels
  patient_data_labels <- extract_labels(patient_data)
  # converting to matrix
  patient_data_matrix <- convert_to_matrix(patient_data)

  list(patient_data_matrix, patient_data_labels)
}
