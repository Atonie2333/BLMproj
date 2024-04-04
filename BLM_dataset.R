###Data Set###
#import player data set
library(readxl)
df<-read_xlsx("summary.xlsx")

#Delete summary rows
df <- df[!is.na(df$Pos) & df$Pos != "", ]

#Rename Variables
library(dplyr)
library(lubridate)
start_date <- ymd("2019-08-09")
#df <- df[ , !duplicated(names(df))]
df <- df %>%
  rename("Blocked_Passes" = "Pass_blocked", 
         "TklplusInt" = "Tkl+Int",
         "Double_CrdY" = "2CrdY")

df$date = as.Date(as.character(df$date), format = "%Y%m%d")
df$Age = as.numeric(sub("-.*", "", df$Age))

#Club Post Schedule
post_schedule<-read_xlsx("post_schedule_long.xlsx")

post_cum <- post_schedule %>%
  filter(club != "Brentford") %>%
  mutate(date = as.Date(date)) %>%
  group_by(club) %>%
  summarise(cum_posts = sum(post)) %>%
  ungroup() 

data <- df %>%
  left_join(post_cum, 
            by = c("club" = "club")
            ) %>%
  # Count days' difference
  mutate(
        after = ifelse(date >= as.Date('2020-05-25'),1,0),
        days_diff = as.numeric(difftime(date, start_date, units = "days")),
        week_count = as.numeric(floor(days_diff / 7) + 1),
        douweek = as.numeric(floor(days_diff / 14) + 1),
        triweek = as.numeric(floor(days_diff / 21) + 1),
        monthcount = as.numeric(floor(days_diff / 30) + 1),
        ) 

# delete rows including infinity value
data<- data[!is.na(df$CmpRa) & df$CmpRa != "", ]

#Binary post equals to 1 if post greater than median, 0 otherwise.
data <- data %>%
  mutate(
    median_post = median(cum_posts, na.rm = TRUE), # Calculate median, remove NA values if any
    post_dummy = if_else(cum_posts > median_post, 1, 0),
    post_MorL = if_else(post_dummy == 1 & after == 1, 1, 0)
  ) %>%
  select(-median_post) %>%# Remove the median_post column
#Binary post equals to 1 if post greater than zero, 0 otherwise.
  mutate(
    TGroup = if_else(cum_posts > 0, 1, 0),#Define clubs without any posts as control group
    quantile_0 = if_else(TGroup == 1 & after == 1, 1, 0), #Define treatment happen after May 25
    post_Howmany = if_else(after == 1, cum_posts, 0), #Continues post as club's support intensity
    quantile_1 = ifelse(cum_posts > 1 & after == 1, 1, 0),
    quantile_2 = ifelse(cum_posts > 2 & after == 1, 1, 0),
    quantile_3 = ifelse(cum_posts > 3 & after == 1, 1, 0),
    quantile_4 = ifelse(cum_posts > 4 & after == 1, 1, 0)
  )


#Export descriptive statistic.
library(stargazer)
library(gt)
library(psych)
#Select Variables describe performance
perf_subset <- data %>% 
  select(Min:SoT) %>%
  select(where(is.numeric)) 

perf_subset %>%
  describe() %>%
  select(n, mean, sd) %>%
  mutate(across(everything(), round, 3)) %>%
  as.data.frame() %>%
  rename(N = n, Mean = mean, St.Dev = sd) %>%
  cbind(Statistic = names(perf_subset), .) %>%
  gt() %>%
  gtsave(filename = "Des_all.tex", 
         path = "D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables")

