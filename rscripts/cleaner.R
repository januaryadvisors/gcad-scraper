#Clean the data

library(tidyverse)
library(here)

#Compile raw results ------------
files <- list.files(here::here("results"))

results <- lapply(files, function(x){
  df <- readRDS(here::here("results", x))
  return(df)
})

df <- do.call(rbind, results) %>%
  filter(is.na(error)) %>% 
  distinct(id, .keep_all = T)


#Roll values ----------------
roll_value <- lapply(1:nrow(df), function(x){
  if (!is.null(df$roll_value_table[[x]])) {
    df <- df$roll_value_table[[x]] %>% 
      mutate(id = df$id[x])
    return(df)
  }
}) %>% 
  do.call(rbind, .)


#Property tables --------------

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
  if (!is.null(df[[4]][[x]]) & ncol(df[[4]][[x]])==4) {
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
    if (ncol(df[[5]][[x]])==4) {
      df <- data.frame(
        id = df[[5]][[x]]$x2[[2]], year = 2019,
        owner_name = df[[5]][[x]]$x2[[12]],
        owner_mailing_addr = df[[5]][[x]]$x2[[13]],
        exemptions = df[[5]][[x]]$x4[[14]]
      )
      return(df)
    }
  }
}) %>% 
  do.call(rbind, .)

property_table_2018 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[6]][[x]]) ) {
    if (ncol(df[[6]][[x]])==4) {
      df <- data.frame(
        id = df[[6]][[x]]$x2[[2]], year = 2018,
        owner_name = df[[6]][[x]]$x2[[12]],
        owner_mailing_addr = df[[6]][[x]]$x2[[13]],
        exemptions = df[[6]][[x]]$x4[[14]]
      )
      return(df)
    }
  }
}) %>% 
  do.call(rbind, .)

property_table_2017 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[7]][[x]])) {
    if (ncol(df[[7]][[x]])==4) {
      df <- data.frame(
        id = df[[7]][[x]]$x2[[2]], year = 2017,
        owner_name = df[[7]][[x]]$x2[[12]],
        owner_mailing_addr = df[[7]][[x]]$x2[[13]],
        exemptions = df[[7]][[x]]$x4[[14]]
      )
      return(df)
    }
  }
}) %>% 
  do.call(rbind, .)

property_table_2016 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[8]][[x]])) {
    if (ncol(df[[8]][[x]])==4) {
      df <- data.frame(
        id = df[[8]][[x]]$x2[[2]], year = 2016,
        owner_name = df[[8]][[x]]$x2[[12]],
        owner_mailing_addr = df[[8]][[x]]$x2[[13]],
        exemptions = df[[8]][[x]]$x4[[14]]
      )
      return(df)
    }
  }
}) %>% 
  do.call(rbind, .)

property_table_2015 <- lapply(1:nrow(df), function(x) {
  if (!is.null(df[[9]][[x]]) ) {
    if (ncol(df[[9]][[x]])==4) {
      df <- data.frame(
        id = df[[9]][[x]]$x2[[2]], year = 2015,
        owner_name = df[[9]][[x]]$x2[[12]],
        owner_mailing_addr = df[[9]][[x]]$x2[[13]],
        exemptions = df[[9]][[x]]$x4[[14]]
      )
      return(df)
    }
  }
}) %>% 
  do.call(rbind, .)


property_tables <- rbind(property_table_2021, property_table_2020, property_table_2019,
                         property_table_2018, property_table_2017, property_table_2016,
                         property_table_2015)


#Final dataset ------------------
df_final <- left_join(roll_value, property_tables, by=c("id", "year")) %>% 
  filter(year>=2015) %>% 
  dplyr::select(id, year, everything()) %>% 
  arrange(id, year) %>% 
  mutate(
    homestead = ifelse(str_detect(exemptions, "HS"), 1, 0),
    improvements = as.numeric(str_remove(str_remove(improvements, "\\$"), ",")),
    land_market = as.numeric(str_remove(str_remove(land_market, "\\$"), ",")),
    ag_valuation = as.numeric(str_remove(str_remove(ag_valuation, "\\$"), ",")),
    appraised = as.numeric(str_remove(str_remove(appraised, "\\$"), ",")),
    hs_cap = as.numeric(str_remove(str_remove(hs_cap, "\\$"), ",")),
    assessed = as.numeric(str_remove(str_remove(assessed, "\\$"), ","))
  )

length(unique(df_final$id))

saveRDS(df_final, here::here("gcad_clean_data.rds"))

