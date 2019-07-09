
# Setup -------------------------------------------------------------------

library(readr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)


# Read in data ------------------------------------------------------------

spartan_df <- read_csv('data/spartan.csv', col_types = 'ncccnnnnc')

# Add leading zero to times
spartan_df$nchar_ticks <- nchar(spartan_df$TicksString)
spartan_df$TicksString[spartan_df$nchar_ticks == 7] <- paste0('0', spartan_df$TicksString[spartan_df$nchar_ticks == 7])
spartan_df$date_time <- mdy_hms(paste('11/14/2015', spartan_df$TicksString))
spartan_df$time <- spartan_df$date_time - mdy_hms('11/14/2015 00:00:00')

# Summarize ---------------------------------------------------------------

spartan_df <- spartan_df %>%
  arrange(RankO) %>%
  mutate(pctO = 100 * RankO/max(RankO),
         total_time = as.numeric(60 * time),
         pace = total_time / 12.1)

spartan_df <- spartan_df %>%
  arrange(RankO) %>%
  group_by(Gender) %>%
  mutate(pctG = 100 * RankG/max(RankG)) %>%
  ungroup() 

spartan_df %>%
ggplot(aes(x = pace)) + theme_bw() +
  geom_histogram(aes(color = Gender))

pdf('output/spartan_open_sat_beast_percentiles.pdf')

spartan_df %>%
  ggplot(aes(x = pace, y = pctO)) + theme_bw() +
  xlim(c(9.5, 40)) + xlab('Pace (min/mile)') +
  ylab('Percentile overall') +
  geom_line() +
  ggtitle('Carolinas Beast (open Sat) \n ')

spartan_df %>%
  ggplot(aes(x = total_time/60, y = pctO)) + theme_bw() +
  xlim(c(1.9, 8)) + xlab('Time (hrs)') +
  geom_line() + ylab('Percentile overall') +
  ggtitle('Carolinas Beast (open Sat) \n ')

spartan_df %>%
  ggplot(aes(x = pace, y = pctG)) + theme_bw() +
  geom_line(aes(color = Gender)) +
  xlim(c(9.5, 30)) + xlab('Pace (min/mile)') +
  ylab('Percentile within gender') +
  ggtitle('Carolinas Beast (open Sat) \n ')

spartan_df %>%
  ggplot(aes(x = total_time/60, y = pctG)) + theme_bw() +
  geom_line(aes(color = Gender)) +
  xlim(c(1.9, 7)) + xlab('Time (hrs)') +
  ylab('Percentile within gender') +
  ggtitle('Carolinas Beast (open Sat) \n ')

dev.off()


# Calculate gender gap ----------------------------------------------------

gap_df <- spartan_df %>%
  mutate(percentileG = ceiling(pctG)) %>%
  group_by(Gender, percentileG) %>%
  summarize(mean_time = mean(total_time),
            mean_pace = mean(pace))

gap_wide1_df <- gap_df %>%
  select(-mean_pace) %>%
  spread(Gender, mean_time) %>%
  mutate(gap_gender = F - M)

gap_wide2_df <- gap_df %>%
  select(-mean_time) %>%
  spread(Gender, mean_pace) %>%
  mutate(gap_gender = F - M)

pdf('output/spartan_open_sat_beast_gender_gap.pdf')
gap_wide1_df %>%
  ggplot(aes(x = percentileG, y = gap_gender)) + theme_bw() +
  geom_line() + 
  xlab('Percentile within gender') + xlim(c(0, 80)) +
  ylab('Gender gap (minutes)') + ylim(c(30, 60)) +
  ggtitle('Carolinas Beast (open Sat) \n ')

gap_wide2_df %>%
  ggplot(aes(x = percentileG, y = gap_gender)) + theme_bw() +
  geom_line() + 
  xlab('Percentile within gender') + xlim(c(0, 80)) +
  ylab('Gender gap (minutes/mile)') + ylim(c(2, 5)) +
  ggtitle('Carolinas Beast (open Sat) \n ')
dev.off()



# Assign age groups -------------------------------------------------------

spartan_df <- spartan_df %>%
  ungroup() %>%
  filter(Age >= 20) %>%
  mutate(age_cat = ifelse(Age < 16, 'under16',
                          ifelse(Age < 18, '16-18',
                                 ifelse(Age < 20, '19',
                                        ifelse(Age < 25, '20-24',
                                               ifelse(Age < 30, '25-29',
                                                      ifelse(Age < 35, '30-34',
                                                             ifelse(Age < 40, '35-39',
                                                                    ifelse(Age < 45, '40-44',
                                                                           ifelse(Age < 50, '45-49',
                                                                                  ifelse(Age < 55, '50-54',
                                                                                         ifelse(Age < 60, '55-59',
                                                                                                ifelse(Age < 65, '60-64',
                                                                                                       ifelse(Age < 70, '65-69',
                                                                                                              ifelse(Age < 75, '70-74')))))))))))))),
         age_group = paste(Gender, age_cat, sep = '_'))

spartan_df <- spartan_df %>%
  filter(!is.na(age_cat)) %>%
  group_by(age_group) %>%
  mutate(pct_ag = 100 * RankA / max(RankA))

pdf('output/spartan_open_sat_beast_age.pdf')

spartan_df %>%
  group_by(age_cat, Gender) %>%
  summarize(n_racers = n()) %>%
  ggplot(aes(x = age_cat, y = n_racers)) + theme_bw() +
  geom_bar(stat = 'identity') + facet_wrap( ~ Gender) +
  ylab('Number of racers') + xlab('Age group') +
  ggtitle('Carolinas Beast (open Sat) \n ') +
  theme(axis.text.x = element_text(size = 5))

spartan_df %>%
  ggplot(aes(x = total_time/60, y = pct_ag)) + theme_bw() +
  geom_line() + facet_wrap( ~ age_group) +
  xlab('Time (hours)') + ylab('Percentile in age group') + 
  ggtitle('Carolinas Beast (open Sat) \n ')

spartan_df %>%
  filter(Age < 50) %>%
  ggplot(aes(x = total_time/60, y = pct_ag, color = age_cat)) + theme_bw() +
  geom_line() + facet_wrap( ~ Gender, ncol = 1) +
  xlab('Time (hours)') + xlim(c(1.95, 6)) +
  ylab('Percentile in age group') + 
  ggtitle('Carolinas Beast (open Sat) \n ')

spartan_df %>%
  filter(Age < 50) %>%
  group_by(age_cat, Gender) %>%
  summarize(median_time = median(total_time/60)) %>%
  ggplot(aes(x = age_cat, y = median_time, color = Gender)) + theme_bw() +
  geom_point() + geom_line(aes(x = as.numeric(as.factor(age_cat)))) +
  xlab('Age group') + ylab('Median time (hours)') +
  ylim(c(3.5, 5.5))

spartan_df %>%
  filter(Age < 50) %>%
  group_by(age_cat, Gender) %>%
  summarize(top_5th = quantile(total_time/60, .05)) %>%
  ggplot(aes(x = age_cat, y = top_5th, color = Gender)) + theme_bw() +
  geom_point() + geom_line(aes(x = as.numeric(as.factor(age_cat)))) +
  xlab('Age group') + ylab('Top 5th percentile time (hours)') +
  ylim(c(2, 4))

dev.off()