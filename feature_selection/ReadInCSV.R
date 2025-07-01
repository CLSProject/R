if (! require("dplyr")) {
  install.packages("dplyr")
}
library(dplyr)


###
# helper functions
read_patient_data_csv <- function(file_name) {
  if (file_test("-r", file_name)) {
    data <- read.csv(file_name)
    # dropping all rows with NA values
    na.omit(data)
  } else {
    NULL
  }
}


###
# function to be called
get_processed_patient_data <- function(file_name = "") {
  patient_data <- read_patient_data_csv(file_name)
  if (is.null(patient_data)) {
    return(NULL)
  }

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
  patient_data_matrix <- data.matrix(patient_data)

  list(patient_data_matrix, patient_data_labels, patient_data_genids)
}
