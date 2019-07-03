# pie chart for reserve  

library(here)
library(dplyr)
library(ggplot2)

# where to look for the input file
file_in <- here::here("data", "intermediate", "rate_summary.csv")
dat <- read.csv(file_in)

dat2 <- dat %>%
    mutate(dir_0 = case_when(CI_high < 0 ~ "dec_sig",
                             CI_low > 0  ~ "inc_sig",
                             rate < 0 ~ "dec_nonsig",
                             rate > 0 ~ "inc_nonsig",
                             TRUE ~ "nonsig"),
           dir_slr = case_when(CI_high < slr_CI_low ~ "dec_sig",
                               CI_low > slr_CI_high  ~ "inc_sig",
                               rate < slr_rate ~ "dec_nonsig",
                               rate > slr_rate ~ "inc_nonsig",
                               TRUE ~ "nonsig"),
           map_lab = paste0(set_id, ": ", user_friendly_set_name, "; ",
                            round(rate, 2), " mm/yr")) %>% 
    rename(lat = latitude_dec_deg,
           long = longitude_dec_deg)


summ <- dat2 %>% 
    group_by(reserve, dir_slr) %>% 
    summarize(value = n())

dat2 %>% 
    group_by(reserve) %>% 
    summarize_at(c("dir_0", "dir_slr"), n(.x))

ggplot(summ) +
    geom_bar(aes(x = "", y = value, fill = dir_slr), stat = "identity") +
    coord_polar("y", start = 0) +
    theme_void()
