library(RSelenium)
library(rvest)
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(config)
library(aws.s3)
library(data.table)
library(httr)
library(jaspatial)
jaspatial::load_geo_packages()
jaspatial::set_geo_options()

#Make list of parcels to scrape in Galveston: ~28,000
parcels <- read_sf(here::here("data", "parcels", "parcels.shp")) %>%
  clean_shape()

parcel_data_2021 <- read_sf(here::here("data", "parcels-2021", "parcels.shp")) %>% 
  st_drop_geometry() %>% 
  clean_names()
  
galveston <- tigris::places("TX", cb=T) %>% 
  clean_shape() %>% 
  filter(name == "Galveston") %>% 
  dplyr::select(galveston=name)

df <- st_join(parcels, galveston) %>% 
  filter(galveston=="Galveston") %>% 
  dplyr::select(id, name, exempt, landuse) %>% 
  st_drop_geometry()


# SCRAPER #############################

scraper <- function(id) {
  #Sys.sleep(2)
  #Navigate to page
  rd$navigate("https://propaccess.trueautomation.com/clientdb/?cid=81")
  
  #First page
  w <- rd$findElement(using="xpath", '//*[@id="propertySearchOptions_searchText"]')
  w$clearElement()
  w$sendKeysToElement(list(id))
  submit <- rd$findElement(using="xpath", '//*[@id="propertySearchOptions_search"]')
  submit$clickElement()
  
  #Click View Details
  w <- rd$findElement(using="xpath", '/html/body/form/div[3]/div[2]/table/tbody/tr[2]/td[10]/a')
  w$clickElement()
  
  #Pull property table
  html <- rd$getPageSource()[[1]]
  property_table <- read_html(html) %>% 
    html_nodes(., xpath='/html/body/form/div/div[5]/div[3]/table') %>% 
    .[[1]] %>% 
    html_table(fill=T) %>% 
    clean_names()
  
  property_table_2021 <- property_table
  
  #Pull Roll Value table
  roll_value_table <- read_html(html) %>% 
    html_nodes(., xpath='/html/body/form/div/div[5]/div[13]/table') %>% 
    .[[1]] %>% 
    html_table(fill=T) %>% 
    clean_names()
  
  #Click back through years
  click_next_year <- function(x) {
    w <- rd$findElement(using="xpath", paste('/html/body/form/div/div[3]/div/select/option[', x,']'))
    w$clickElement()
  }
  
  stop <- "NO"
  x <- 1
  while (stop=="NO" & x<7) {
   x <- x + 1
   try_next <- try(click_next_year(x))
   
   if (stop=="NO") {
     html <- rd$getPageSource()[[1]]
   }
   
   if (!str_detect(html, "Sorry for the inconvenience")) {
     property_table <- read_html(html) %>% 
       html_nodes(., xpath='/html/body/form/div/div[5]/div[3]/table') %>% 
       .[[1]] %>% 
       html_table(fill=T) %>% 
       clean_names()
     
     assign(paste0("property_table_", 2022-x), property_table)
   } else {
     stop <- "YES"
   }
   
  }
  
  #Prepare data for export
  data <- tibble(
    id = id,
    roll_value_table = list(roll_value_table),
    prop_2021 = list(property_table_2021)
  ) 
  
  if (exists("property_table_2020")) {
    data <- data %>% mutate(property_table_2020 = list(property_table_2020))
  }
  
  if (exists("property_table_2019")) {
    data <- data %>% mutate(property_table_2019 = list(property_table_2019))
  }
  
  if (exists("property_table_2018")) {
    data <- data %>% mutate(property_table_2018 = list(property_table_2018))
  }
  
  if (exists("property_table_2017")) {
    data <- data %>% mutate(property_table_2017 = list(property_table_2017))
  }
  
  if (exists("property_table_2016")) {
    data <- data %>% mutate(property_table_2016 = list(property_table_2016))
  }
  
  if (exists("property_table_2015")) {
    data <- data %>% mutate(property_table_2015 = list(property_table_2015))
  }
  
  return(data)
}

scrape_safe <- function(id) {
  result <- try(scraper(id))
  if (class(result) == "try-error") { 
    cat("Error encountered for id:", id)
    return(data.frame("id" = c(id), error = c("Error!"))) 
    Sys.sleep(runif(1, 1, 3))
  } else { 
    return(result)
  }
}

#START SCRAPER -------------
driver <- rsDriver(browser=c("firefox"))
rd <- driver[["client"]]
#rd$open()
rd$setTimeout(type = "implicit", milliseconds = 8000)

start <- 1001
end <- 3000
results <- slice(df, start:end) %>% 
  group_by(id) %>% 
  do(scrape_safe(.$id))

saveRDS(results, here::here("results", paste0("results_", start, "_", end, ".rds")))

#Shut down server #######################
rd$close()
driver$server$stop()

