### Practice 01 answer ###

# 01

library(readxl)

df <- readxl::read_excel("./raw_data_practice_01.xlsx")
head(df)
View(df)

###########################################################

# 02

library(tidyverse)

df <- df %>% dplyr::mutate(Average = (January + February + March)/3)
df

df_good <- df %>% dplyr::filter(Average >= 30000)
df_good

df_bad <- df %>% dplyr::filter(Average <= 20000)
df_bad

###########################################################

# 03

df_bad_help <- df_bad %>% dplyr::filter(March < January | March < February)
df_bad_help

###########################################################

# 04

df_good_award <- df_good %>% dplyr::filter(February > January &
                                           March >  January &
                                           March >  February)
df_good_award