# library packages
library(tidyverse)

# unzip data
unzip("data-6.zip")

# read data
df_initial18 <- data.frame(read_csv("data//AQ_2018.csv"))

df_initial19 <- data.frame(read_csv("data//AQ_2019.csv"))

df_initial20 <- data.frame(read_csv("data//AQ_2020.csv"))

# function for data cleaning
clean_AQ <- function(df){
  
  # rename columns
  df_clean <- df |> rename(co_max = Daily.Max.8.hour.CO.Concentration, 
                                  pm2_max = Daily.Mean.PM2.5.Concentration,
                                  o3_max = Daily.Max.8.hour.Ozone.Concentration)|> 
    mutate(Date = mdy(Date)) |> # format date
    group_by(Date) |> # replace duplicates with average
    summarise(co_max = mean(co_max),
              pm2_max = mean(pm2_max),
              o3_max = mean(o3_max),
              .groups = "drop")

  # set negative values to 0
  pollutant_cols <- c("co_max", "pm2_max", "o3_max")

  for(col in pollutant_cols) {
   df_clean[[col]][df_clean[[col]] < 0] <- 0
  }
  
  return(df_clean)
}

df_AQ18 <- clean_AQ(df_initial18)
df_AQ19 <- clean_AQ(df_initial19)
df_AQ20 <- clean_AQ(df_initial20)

# verify cleaning process
list_df <- list(
  "2018" = df_AQ18,
  "2019" = df_AQ19,
  "2020" = df_AQ20
)

for(year in names(list_df)) {
  print(paste("========== Checking", year, "=========="))
  print(paste("Date class:", class(list_df[[year]]$Date)))
  print(paste("Unique dates:", length(unique(list_df[[year]]$Date))))
  print(paste("CO min:", min(list_df[[year]]$co_max, na.rm = TRUE)))
  print(paste("PM2.5 min:", min(list_df[[year]]$pm2_max, na.rm = TRUE)))
  print(paste("O3 min:", min(list_df[[year]]$o3_max, na.rm = TRUE)))
  print("")
}

# create analytic dataframe, combining three years
df_analytic <- bind_rows(
  df_AQ18 |> mutate(year = 2018),
  df_AQ19 |> mutate(year = 2019),
  df_AQ20 |> mutate(year = 2020)
)

# create monthly dataframe
df_monthly <- df_analytic |> 
  mutate(month = month(Date, label = TRUE)) |> 
  group_by(month) |> 
  summarise(co_mean = mean(co_max, na.rm = TRUE),
            co_se = sd(co_max, na.rm = TRUE) / sqrt(n()),
            co_ci = co_se * 1.96,
            o3_mean = mean(o3_max, na.rm = TRUE),
            o3_se = sd(o3_max, na.rm = TRUE) / sqrt(n()),
            o3_ci = o3_se * 1.96,
            pm2_mean = mean(pm2_max, na.rm = TRUE),
            pm2_se = sd(pm2_max, na.rm = TRUE) / sqrt(n()),
            pm2_ci = pm2_se * 1.96)

# pivot longer for ggplot
df_plot <- df_monthly |> 
  pivot_longer(cols = c(co_mean, o3_mean, pm2_mean),
               names_to = "pollutant",
               values_to = "concentration") |> 
  mutate(pollutant_clean = case_when(
    pollutant == "co_mean" ~ "CO",
    pollutant == "o3_mean" ~ "O3",
    pollutant == "pm2_mean" ~ "PM2"
  )) |> 
  # add CI
  mutate(ci = case_when(
    pollutant == "co_mean" ~ co_ci,
    pollutant == "o3_mean" ~ o3_ci,
    pollutant == "pm2_mean" ~ pm2_ci
  ))

# plot CO and O3 by month
df_plot |> 
  filter(pollutant_clean %in% c("CO", "O3")) |> 
  ggplot(aes(x = month, y = concentration, color = pollutant_clean, group = pollutant_clean)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = concentration - ci, ymax = concentration + ci), 
                width = 0.2) +
  labs(title = "Monthly Average CO and O3 Concentrations (2018-2020)",
       x = "Month",
       y = "Concentration",
       color = "Pollutant") +
  theme_minimal()

# plot PM2 by month
df_plot |> 
  filter(pollutant_clean == "PM2") |> 
  ggplot(aes(x = month, y = concentration, color = pollutant_clean, group = pollutant_clean)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = concentration - ci, ymax = concentration + ci), 
                width = 0.2) +
  labs(title = "Monthly Average PM2 Concentrations (2018-2020)",
       x = "Month",
       y = "Concentration",
       color = "Pollutant") +
  theme_minimal()











