# Setup -------------------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)


# Read in data ------------------------------------------------------------

spartan_df <- read_csv('data/spartan_carolinas_sun_open_sprint.csv', col_types = 'ncccnnnnc')

# Add leading zero to times
spartan_df$nchar_ticks <- nchar(spartan_df$TicksString)
spartan_df$TicksString[spartan_df$nchar_ticks == 7] <- paste0('0', spartan_df$TicksString[spartan_df$nchar_ticks == 7])
spartan_df$TicksString[spartan_df$nchar_ticks == 5] <- paste0('00:', spartan_df$TicksString[spartan_df$nchar_ticks == 5])
spartan_df$date_time <- mdy_hms(paste('11/15/2015', spartan_df$TicksString))
spartan_df$time <- spartan_df$date_time - mdy_hms('11/15/2015 00:00:00')

# Calculate total time and pace
results_df <- spartan_df %>%
  mutate(total_time = as.numeric(time),
         pace = total_time / 4.2) %>%
  mutate(event_name = 'Spartan SC Sprint - Sun Open') %>%
  rename(age = Age, gender = Gender, finish_time = total_time, bib_number = BibNum) %>%
  select(event_name, bib_number, age, gender, finish_time, pace) 

write_csv(results_df, 'output/results_spartan_sc_sun_sprint_open.csv')
  

# Read in Snowdrop ultra data ---------------------------------------------

ultra_df <- read_tsv('data/ultra.txt')

ultra_df <- ultra_df %>%
  mutate(finish_time = 60 * 55,
         pace = finish_time / Distance) %>% #55 hr race!
  mutate(bib_number = 1:n(),
         event_name = 'Snowdrop 55 Hour Ultra') %>%
  rename(age = Age, gender = Gender) %>%
  select(event_name, bib_number, age, gender, finish_time, pace) 

results_df <- ultra_df

write_csv(results_df, 'output/results_snowdrop_ultra.csv')


# Read in Charlotte data  -------------------------------------------------

char1_df <- read_tsv('data/charlotte_1.txt')
char2_df <- read_tsv('data/charlotte_2.txt')
char3_df <- read_tsv('data/charlotte_3.txt')
char4_df <- read_tsv('data/charlotte_4.txt')

char_df <- rbind(char1_df, char2_df, char3_df, char4_df)

char_df <- char_df %>%
  mutate(total_time = 60 * as.numeric(mdy_hms(paste('1/15/2016', `Chip Time`)) - mdy_hms('1/15/2016 00:00:00')),
         pace = total_time / 13) %>%
  mutate(event_name = 'Charlotte Trial Race 13m') %>%
  rename(age = Age, gender = Gender, finish_time = total_time, bib_number = Bib) %>%
  select(event_name, bib_number, age, gender, finish_time, pace) 

results_df <- char_df

write_csv(results_df, 'output/results_charlotte_half.csv')



# Read in Western States data ---------------------------------------------

west_df <- read_tsv('data/western_states_ultra_2016.txt', col_types = 'ncccccncc')

western_df <- west_df %>%
  mutate(total_time = 60 * as.numeric(mdy_hms(paste0('06-23-2016 ', Time)) - mdy_hms('06-23-2016 00:00:00')),
         pace = total_time / 100.2) %>%
  mutate(event_name = 'Western States 100') %>%
  rename(age = Age, gender = Gender, finish_time = total_time, bib_number = Bib) %>%
  select(event_name, bib_number, age, gender, finish_time, pace)

results_df <- western_df 

write_csv(results_df, 'output/results_western_states.csv')
