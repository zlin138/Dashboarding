library(tidyverse)
library(readxl)
library(Hmisc)
library(plotly)
library(patchwork)
library(ggthemes)
library(leaflet)
library(sf)
library(lubridate)


# rename columns 
setwd("~/ECMC_Materials")
here()
spills_df <- read_excel('Spills/Spills.xlsx')
spills_df <- spills_df %>% rename_with(~ spills_df %>% 
                                         colnames %>% str_trim() %>% 
                                         str_squish() %>% str_replace_all(' ', "_")  %>% 
                                         str_replace_all("#", "number") %>% 
                                         str_replace('&', '_and_') %>% 
                                         tolower() ) %>% 
  rename(residence_occupied_structure = 'residence_/_occupied_structure')

# Remove duplicates and undocumented tracking number 
spills_df <- spills_df %>% 
  distinct(document_number, .keep_all  =TRUE) %>% 
  filter(tracking_number != 112537) # Incorrect historical data 


# split dataset to merge and join later
spills_init <- spills_df %>% 
  filter(report == 'I') %>% 
  select(document_number:spill_description)

spills_supp <- spills_df %>% 
  filter(report == 'S')

spills_init_supp <- spills_df %>% 
  filter(report == "I w/S")


# Join with the latest information 
spills_df <- spills_init %>% 
  left_join(spills_supp %>% 
              group_by(tracking_number) %>% 
              mutate(row = row_number(desc(document_number))) %>%
              filter(row == 1) %>% 
              select(tracking_number, supplemental_report_date:form_27_project_number), join_by(tracking_number)) %>% 
  rbind(spills_supp %>% 
          filter(tracking_number %in% spills_init_supp$tracking_number) %>% 
          rbind(spills_init_supp) %>% 
          group_by(tracking_number) %>%  
          mutate(row = row_number(desc(document_number))) %>%
          filter(row == 1) %>% 
          select(document_number:form_27_project_number))

# Convert from dates to columns 
spills_df <- spills_df  %>% 
  mutate(date_of_discovery = mdy(date_of_discovery)) %>% 
  mutate(year_of_discovery = year(date_of_discovery))

# Around 4000/6700 entries have numerical data within it 
# Not a perfect parse -- will miss certains values and misread some edge case inputs but good enough


parse_weather <- spills_df %>%  
  select(weather_conditions) %>% 
  mutate(weather_original = weather_conditions) %>% 
  mutate(weather_conditions = weather_original %>% str_remove_all("°") %>%str_trim %>% str_squish()) %>% 
  mutate(weather_conditions = case_when(
    # parse ranged values such as 50-80, 80-100 degrees leading/trailing spaces will be trimmed
    str_detect(weather_conditions, regex("\\d{1,2}-\\d{1,3}\\s?(deg|F)", ignore_case = TRUE)) ~ substr(weather_conditions, str_locate(weather_conditions, "-") -2,
                                                                                                       str_locate(weather_conditions, "-") + 3), 
    # Get negative temperatures
    str_detect(weather_conditions, regex("(?<!\\d)-\\d{1,2}\\s?(deg|F)", ignore_case = TRUE)) ~  substr(weather_conditions, str_locate(weather_conditions, "-"), str_locate(weather_conditions, "-") +2), 
    # Grep degrees 
    str_detect(weather_conditions, regex("deg", ignore_case = TRUE)) ~  substr(weather_conditions, str_locate(weather_conditions, regex("deg", ignore_case = T))-3,
                                                                               str_locate(weather_conditions, regex("deg", ignore_case = T)) -1), 
    # grep F 
    str_detect(weather_conditions, "F") & str_detect(weather_conditions, "\\d") ~ substr(weather_conditions, str_locate(weather_conditions, "F")-3,
                                                                                         str_locate(weather_conditions, "F")-1),
    # parse digits -- omitting entries that contains wind mph 
    str_detect(weather_conditions, "\\d") & !str_detect(weather_conditions, regex("mph", ignore_case = T)) ~ map_chr(str_extract_all(weather_conditions, "\\d"),
                                                                                                                     ~ str_c(.x, collapse = "")),
    TRUE ~ weather_conditions)) %>% 
  # Extra Cleaning 
  mutate(weather_conditions = ifelse(str_detect(weather_conditions, regex("\\d{1,2}-\\d{2}[^0-9](?!\\w)")), 
                                     substr(weather_conditions, 0, str_locate(weather_conditions, "-") + 2), weather_conditions)) %>% 
  mutate(weather_conditions = weather_conditions %>% str_remove("~") %>% str_remove(",")) %>% 
  # remove leading and trailing spaces after cleaning
  mutate(weather_conditions = weather_conditions%>% str_trim() %>%str_squish()) %>% 
  # take the average of ranged estiamtes 
  mutate(weather_conditions = case_when(
    str_detect(weather_conditions, "\\d{1,2}-\\d{1,3}") ~  as.character((as.integer(substr(weather_conditions, 0, str_locate(weather_conditions, "-")-1)) + as.integer(substr(weather_conditions, str_locate(weather_conditions, "-")+1, str_locate(weather_conditions, "-")+3))) /2 ) , 
    T ~ weather_conditions)) %>% 
  # Convert to integer -- R will drop columns that cannot be converted --contains non-numerica characters
  mutate(weather_conditions = weather_conditions %>% as.integer())  %>% 
  # Clean boundries 
  mutate(weather_conditions = ifelse(weather_conditions < -20, (-1)*weather_conditions, weather_conditions))  %>% # eg mid -90 would be read as -90 we can cast that to 90 the min is -20
  mutate(weather_conditions = ifelse(weather_conditions > 110, NA,  weather_conditions)) # multiple numeric values parsed together for the 5th case_when

# change original dataset's entries 
spills_df$weather_conditions <- parse_weather$weather_conditions
