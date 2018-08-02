library(readr)
library(tidyverse)

source('~/Projects/retentiontimealignment/R/preprocesses_peptides.R')

paths_to_check = list.files('/run/user/1000/gvfs/smb-share:server=msserver,share=users/Matteo/4Ute', full.names = T)
files_to_check = basename(paths_to_check)
paths_to_check = paths_to_check[files_to_check != 'info.csv']
files_to_check = files_to_check[files_to_check != 'info.csv']
good_folder_names = tools::file_path_sans_ext(files_to_check)

path = paths_to_check[1]
output_path = "data/micro_vs_nano"

prepare_data_for_alignment = function(path,
                                      output_path = "data/micro_vs_nano"){
  base_name = tools::file_path_sans_ext(basename(path))
  D = read_delim(path, ";", escape_double = FALSE, trim_ws = TRUE)
  D = preprocess_peptides(D)
  output_path = file.path(output_path, base_name)
  dir.create(output_path, showWarnings = T, recursive = T, mode = "0777")
  save(D, file=file.path(output_path, 'r_preprocessed.Rd'))
  write_csv(D$data,       path = file.path(output_path, 'annotated_data.csv'  ) )
  write_csv(D$unlabelled, path = file.path(output_path, 'unannotated_data.csv') )
  return(0)
}

outputs = parallel::mclapply(paths_to_check[2:4], prepare_data_for_alignment)