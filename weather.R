
# Packages
library(tidyverse)
library(roll)
library(zoo)  # rolling averages


# Load from climatedata.ca
df <- read.csv("hope_weather.csv") %>%
      mutate(LOCAL_DATE = as.Date(substr(LOCAL_DATE,1,10)))

df_rain <- df %>%
           filter(STATION_NAME == 'HOPE LITTLE MOUNTAIN' |
                  (STATION_NAME == 'HOPE A' & LOCAL_DATE >= '1955-01-01' & LOCAL_DATE <= '1995-03-31') |
                  (STATION_NAME == 'LAIDLAW' & LOCAL_DATE >= '1995-04-01' & LOCAL_DATE <= '2017-02-28') |
                  (STATION_NAME == 'HOPE AIRPORT' & LOCAL_DATE >= '2017-03-01')) %>%
           mutate(TOTAL_RAIN = case_when(is.na(TOTAL_RAIN) ~ TOTAL_PRECIPITATION,
                                         TRUE ~ TOTAL_RAIN)) %>%
           arrange(LOCAL_DATE) %>%
           mutate(TWO_DAY_TOTAL = TOTAL_RAIN + lag(TOTAL_RAIN),
                  THREE_DAY_TOTAL = TOTAL_RAIN + lag(TOTAL_RAIN) + lag(TOTAL_RAIN,2),
                  FOUR_DAY_TOTAL = TOTAL_RAIN + lag(TOTAL_RAIN) + lag(TOTAL_RAIN,2)+ lag(TOTAL_RAIN,3),
                  hvy_rain_day = case_when(TOTAL_RAIN > 50 ~ 1,
                                           TRUE ~ 0))

one_day <- df_rain %>% slice_max(TOTAL_RAIN, n = 10) %>% select(LOCAL_DATE, TOTAL_RAIN) %>% mutate(LOCAL_DATE = format(LOCAL_DATE, "%b %d, %Y"))

one <- ggplot(one_day, aes(x = LOCAL_DATE, y = TOTAL_RAIN)) +
       scale_x_discrete(name = "Date",
                        limits = one_day$LOCAL_DATE) +
       geom_col(fill = "#0099f9") +
       theme_classic(base_size = 15) + 
       ggtitle("Hope BC - Highest Single Day Rainfall Totals") +
       labs(y = "Total Rainfall (mm)",
            caption = "Chart: Michael Hainke  Data: Hope Weather Stations 1910-2021 (climatedata.ca)") +
       geom_text(label = paste0(round(one_day$TOTAL_RAIN,0)," mm"), nudge_y = 4, size=4) +
       theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


two_day <- df_rain %>% slice_max(TWO_DAY_TOTAL, n = 20) %>% select(LOCAL_DATE, TWO_DAY_TOTAL)
two_day <- two_day %>% filter(!LOCAL_DATE %in% c(as.Date('1975-12-02'),as.Date('1990-11-09'),as.Date('2021-11-14'))) %>% slice_max(TWO_DAY_TOTAL, n = 10) %>%
           mutate(START_DATE = LOCAL_DATE - 1) %>%
           mutate(RANGE = paste0(format(START_DATE, "%b %d"),"-",format(LOCAL_DATE, "%d, %Y")))

two <- ggplot(two_day, aes(x = RANGE, y = TWO_DAY_TOTAL)) +
        scale_x_discrete(name = "Date",
                         limits = two_day$RANGE) +
        geom_col(fill = "#0099f9") +
        theme_classic(base_size = 15) + 
        ggtitle("Hope BC - Highest Two Day Rainfall Totals") +
        labs(y = "Total Rainfall (mm)",
             caption = "Chart: Michael Hainke  Data: Hope Weather Stations 1910-2021 (climatedata.ca)") +
        geom_text(label = paste0(round(two_day$TWO_DAY_TOTAL,0)," mm"), nudge_y = 5, size=4) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


three_day <- df_rain %>% slice_max(THREE_DAY_TOTAL, n = 20) %>% select(LOCAL_DATE, THREE_DAY_TOTAL)
three_day <- three_day %>% filter(!LOCAL_DATE %in% c(as.Date('1990-11-11'),
                                                     as.Date('2021-11-16'),
                                                     as.Date('1975-12-04'),
                                                     as.Date('1990-11-09'),
                                                     as.Date('1974-01-14'),
                                                     as.Date('2021-11-14'),
                                                     as.Date('1975-12-02'),
                                                     as.Date('2003-10-18'))) %>% slice_max(THREE_DAY_TOTAL, n = 10) %>%
            mutate(START_DATE = LOCAL_DATE - 2) %>%
            mutate(RANGE = paste0(format(START_DATE, "%b %d"),"-",format(LOCAL_DATE, "%d, %Y")))

three <- ggplot(three_day, aes(x = RANGE, y = THREE_DAY_TOTAL)) +
        scale_x_discrete(name = "Date",
                         limits = three_day$RANGE) +
        geom_col(fill = "#0099f9") +
        theme_classic(base_size = 15) + 
        ggtitle("Hope BC - Highest Three Day Rainfall Totals") +
        labs(y = "Total Rainfall (mm)",
             caption = "Chart: Michael Hainke  Data: Hope Weather Stations 1910-2021 (climatedata.ca)") +
        geom_text(label = paste0(round(three_day$THREE_DAY_TOTAL,0)," mm"), nudge_y = 5, size=4) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


four_day <- df_rain %>% slice_max(FOUR_DAY_TOTAL, n = 20) %>% select(LOCAL_DATE, FOUR_DAY_TOTAL)
four_day <- four_day %>% filter(!LOCAL_DATE %in% c(as.Date('1990-11-11'),
                                                     as.Date('1990-11-12'),
                                                     as.Date('1975-12-03'),
                                                     as.Date('2021-11-16'),
                                                     as.Date('2021-11-17'),
                                                     as.Date('1975-12-05'),
                                                     as.Date('1989-11-11'),
                                                     as.Date('1974-01-15'),
                                                     as.Date('1990-11-13'))) %>% slice_max(FOUR_DAY_TOTAL, n = 10) %>%
            mutate(START_DATE = LOCAL_DATE - 3) %>%
            mutate(RANGE = paste0(format(START_DATE, "%b %d"),"-",format(LOCAL_DATE, "%d, %Y")))

four <- ggplot(four_day, aes(x = RANGE, y = FOUR_DAY_TOTAL)) +
        scale_x_discrete(name = "Date",
                         limits = four_day$RANGE) +
        geom_col(fill = "#0099f9") +
        theme_classic(base_size = 15) + 
        ggtitle("Hope BC - Highest Four Day Rainfall Totals") +
        labs(y = "Total Rainfall (mm)",
             caption = "Chart: Michael Hainke  Data: Hope Weather Stations 1910-2021 (climatedata.ca)") +
        geom_text(label = paste0(round(four_day$FOUR_DAY_TOTAL,0)," mm"), nudge_y = 5, size=4) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# Extreme Rain Days (greater than 50mm)

heavy_rainfall_days <- df_rain %>%
                       group_by(LOCAL_YEAR) %>%
                       summarise(hvy_rain_days = sum(hvy_rain_day))
heavy_rainfall_days <- heavy_rainfall_days %>%
                       mutate(rolling_mean = rollmean(hvy_rain_days, k=10, align="right", fill=NA))

heavy <- ggplot(heavy_rainfall_days, aes(x = LOCAL_YEAR, y = hvy_rain_days)) +
          geom_col(fill = "#0099f9") +
          geom_line(data = heavy_rainfall_days, aes(x=LOCAL_YEAR, y=rolling_mean), size=1) +
          scale_x_continuous(breaks = seq(1910, 2025, by = 5)) +
          theme_classic(base_size = 15) + 
          ggtitle("Hope BC - Number of Heavy Rain Days (more than 50mm in 24 hrs)") +
          labs(y = "Days",
               x = "Year",
               caption = "Chart: Michael Hainke  Data: Hope Weather Stations 1910-2021 (climatedata.ca)") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position="bottom")
heavy

# September and October Totals

fall_rain <- df_rain %>%
             filter(LOCAL_MONTH %in% c(9,10)) %>%
             group_by(LOCAL_YEAR) %>%
             summarise(total = sum(TOTAL_RAIN)) %>%
             arrange(desc(total)) %>%
             mutate(LOCAL_YEAR = as.character(LOCAL_YEAR)) %>%
             slice_max(total, n = 15)
  
fall <- ggplot(fall_rain, aes(x = LOCAL_YEAR, y = total)) +
            geom_col(fill = "#0099f9") +
            scale_x_discrete(name = "Year",
                             limits = fall_rain$LOCAL_YEAR) +
            theme_classic(base_size = 15) + 
            ggtitle("Hope BC - Highest Sept+Oct Rainfall Totals") +
            labs(y = "Total Rainfall (mm)",
                 caption = "Chart: Michael Hainke  Data: Hope Weather Stations 1910-2021 (climatedata.ca)") +
            geom_text(label = paste0(round(fall_rain$total,0)," mm"), nudge_y = 10, size=4) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
fall

  
  
