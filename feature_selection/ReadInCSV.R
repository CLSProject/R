if (! require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)


###
# reading CSV
# find_patient_data_csv_files <- function() {
#   wd <- getwd()
#   list.files(wd, pattern = ".*[.]csv", full.names = TRUE)
# }

# has_found_csv_files <- function(found_files) {
#   if (length(found_files) > 0) {
#     return(TRUE)
#   }
#   FALSE
# }

read_patient_data_csv <- function(file_name) {
  # dir_csv_files <- find_patient_data_csv_files()
  # if (! has_found_csv_files(dir_csv_files)) {
  #   break
  # }
  # info_dir_csv_files <- file.info(dir_csv_files)
  # # most recent CSV file
  # latest_csv_file_name <- rownames(info_dir_csv_files)[which.max(info_dir_csv_files$mtime)] # nolint: line_length_linter.
  if (file_test("-r", file_name)) {
    data <- read.csv(file_name)
    # dropping all rows with NA values
    na.omit(data)
  } else {
    NULL
  }
}


###
# testing for special cases
# check_for_only_numeric_data <- function(data) {
#   if (any(sapply(data, is.numeric))) {
#     return(TRUE)
#   }
#   FALSE
# }


###
convert_to_matrix <- function(data) {
  # dropping labels from patient data
  data <- data[rownames(data) != "labels", ]
  # converting to matrix
  data.matrix(data)
}


get_processed_patient_data <- function(file_name = "") {
  patient_data <- read_patient_data_csv(file_name)

  # extracting labels
  label_col_idx <- grep("label", patient_data[, 1], ignore.case = TRUE)
  patient_data_labels <- c(patient_data[label_col_idx,
                                        2:ncol(patient_data)])
  if (length(patient_data_labels) == 0) {
    # if no labels are included
    patient_data_labels <- NULL
  } else {
    # dropping the label rows
    patient_data <- patient_data[-label_col_idx, ]
  }

  # extracting gen ids
  patient_data_genids <- patient_data[, 1]
  patient_data <- dplyr::select(patient_data, -1)

  # converting to matrix
  patient_data_matrix <- convert_to_matrix(patient_data)

  list(patient_data_matrix, patient_data_labels, patient_data_genids)
}
