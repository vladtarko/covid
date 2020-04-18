library(tidyverse)
library(gganimate)

covid <- read_csv("http://covidtracking.com/api/states/daily.csv")

theme_set(ggthemes::theme_clean())

logistic <- function(x, ymax, xmid, scal) { ymax / (1 + exp((xmid-x)/scal) ) }

covid_TOTAL <- covid %>%
  group_by(date) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE) %>% 
  filter(death != 0) %>% 
  rowid_to_column("id")  

covid_total_est <- list()
for (day in covid_TOTAL$date %>% unique) {

  covid_total <- covid_TOTAL %>% filter(date <= day)
  
  try(
    {
      # fitting a logistic curve to the death data
      us_death_est <- nls(death ~ SSlogis(id, death_max, tmid, scal), data = covid_total) %>% 
        broom::tidy() %>% 
        mutate(est_max = estimate + 2*std.error,
               est_min = estimate - 2*std.error)
  
      us_death_time     <- us_death_est$estimate[2]*2
      us_death_time_max <- us_death_est$est_max[2]*2
      
      covid_total_est[[as.character(day)]] <- tibble(
          id            = seq(0, max(us_death_time, us_death_time_max)),
          
          death_est_max = logistic(id, us_death_est$est_max[1], us_death_est$est_max[2], us_death_est$est_max[3]),
          death_est_min = logistic(id, us_death_est$est_min[1], us_death_est$est_min[2], us_death_est$est_min[3]),
          death_est     = logistic(id, us_death_est$estimate[1], us_death_est$estimate[2], us_death_est$estimate[3]),
        ) %>% 
        full_join(covid_total %>% select(id, date, death), by = "id") 
  
  },
  silent = TRUE)
  
}

cvd_est <- bind_rows(covid_total_est, .id = "day")


# animate --------------------------------------

cvd_est %>%
  filter(day >= 20200406) %>% 
  ggplot(aes(x = id)) +
    geom_ribbon(aes(ymin = death_est_min, ymax = death_est_max),  alpha = 0.2) +
    geom_line(aes(y = death_est)) +
    geom_point(aes(y = death), color = "black") +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "days", y = "", title = "Covid death estimates for United States") +
  transition_manual(day)
