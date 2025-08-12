library(tidyverse)
library(readxl)

file_paths <- list.files(path = "dataset/", pattern = "\\.xlsx$", 
                         full.names = TRUE)

all_crops_df <- file_paths |>
  set_names() |>  
  map_dfr(~ read_xlsx(.x), .id = "Crop_Type")

all_crops_df <- all_crops_df |>
  mutate(Crop_Type = Crop_Type |>
           basename() |>                  
           tools::file_path_sans_ext() |> 
           str_replace("_rule", ""))   

# Optional (if trees don't need a dedicated column)
# all_crops_df1 <- all_crops_df |>
#  select(!Crop_Type)

# check the new combine table
View(all_crops_df)
#View(all_crops_df1)

# save the file
saveRDS(all_crops_df, "dataset/all_crops_df.rds")

# save to csv (optional)
# write_csv(all_crops_df, "data/all_crops_df.csv")