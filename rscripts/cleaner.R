#Clean the data

library(tidyverse)
library(here)

#Compile raw results
files <- list.files(here::here("results"))

results <- lapply(files, function(x){
  df <- readRDS(here::here("results", x))
  return(df)
})

df <- do.call(rbind, results)


#Roll values
roll_value <- lapply(1:nrow(df), function(x){
  if (!is.null(df$roll_value_table[[x]])) {
    df <- df$roll_value_table[[x]] %>% 
      mutate(id = df$id[x])
    return(df)
  }
})

roll_value <- do.call(rbind, roll_value)


#Property tables

property_tables_key <- data.frame(
  table_id = c(3,4,5,6,7,8,9),
  year = c(2021, 2020, 2019, 2018, 2017, 2016, 2015)
)



property_table_2021 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[3]][[x]])) {
    df <- data.frame(
      id = df[[3]][[x]]$x2[[2]], year = 2021,
      owner_name = df[[3]][[x]]$x2[[12]],
      owner_mailing_addr = df[[3]][[x]]$x2[[13]],
      exemptions = df[[3]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)

property_table_2020 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[4]][[x]])) {
    df <- data.frame(
      id = df[[4]][[x]]$x2[[2]], year = 2020,
      owner_name = df[[4]][[x]]$x2[[12]],
      owner_mailing_addr = df[[4]][[x]]$x2[[13]],
      exemptions = df[[4]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)

property_table_2019 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[5]][[x]])) {
    df <- data.frame(
      id = df[[5]][[x]]$x2[[2]], year = 2019,
      owner_name = df[[5]][[x]]$x2[[12]],
      owner_mailing_addr = df[[5]][[x]]$x2[[13]],
      exemptions = df[[5]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)

property_table_2018 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[6]][[x]])) {
    df <- data.frame(
      id = df[[6]][[x]]$x2[[2]], year = 2018,
      owner_name = df[[6]][[x]]$x2[[12]],
      owner_mailing_addr = df[[6]][[x]]$x2[[13]],
      exemptions = df[[6]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)

property_table_2017 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[7]][[x]])) {
    df <- data.frame(
      id = df[[7]][[x]]$x2[[2]], year = 2017,
      owner_name = df[[7]][[x]]$x2[[12]],
      owner_mailing_addr = df[[7]][[x]]$x2[[13]],
      exemptions = df[[7]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)

property_table_2016 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[8]][[x]])) {
    df <- data.frame(
      id = df[[8]][[x]]$x2[[2]], year = 2016,
      owner_name = df[[8]][[x]]$x2[[12]],
      owner_mailing_addr = df[[8]][[x]]$x2[[13]],
      exemptions = df[[8]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)

property_table_2015 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[9]][[x]])) {
    df <- data.frame(
      id = df[[9]][[x]]$x2[[2]], year = 2015,
      owner_name = df[[9]][[x]]$x2[[12]],
      owner_mailing_addr = df[[9]][[x]]$x2[[13]],
      exemptions = df[[9]][[x]]$x4[[14]]
    )
    return(df)
  }
}) %>% 
  do.call(rbind, .)


property_tables <- rbind(property_table_2021, property_table_2020, property_table_2019,
                         property_table_2018, property_table_2017, property_table_2016,
                         property_table_2015)


#Final dataset
df_final <- left_join(roll_value, property_tables, by=c("id", "year")) %>% 
  filter(year>=2015) %>% 
  dplyr::select(id, year, everything()) %>% 
  arrange(id, year)



