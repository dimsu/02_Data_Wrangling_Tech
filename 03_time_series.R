# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH => LIBRARY (LUBRIDATE)----

library(tidyverse)
library(lubridate)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

glimpse(bike_orderlines_tbl)


# 1.0 Date & Lubridate Basics ----

# 1.1 Character vs Date/Datetime ----
order_date_tbl <- bike_orderlines_tbl %>%
    select(order_date)

order_date_tbl %>%
    pull(order_date) %>% class()


# 1.2 Date Classes ----

order_date_tbl %>%
    
# 1.2.1 transform dttm -> character(chr) (without the hour) ----
      mutate(order_date_chr = as.character(order_date)) %>%

# 1.2.2 Add to the character the character 00:00:00 ----
    mutate(order_date_chr2 = order_date_chr %>% str_c(" 00:00:00")) %>%
    
# 1.2.3 Transform a chr to a real date (year - month - day) format ----
    mutate(order_date_date = order_date_chr %>% ymd()) %>%
           
# 1.2.4 transform a ddtm (chr) to a ddtm (ymd_hms) format ----
    
    mutate(order_date_dttm = order_date_chr2 %>% ymd_hms()) %>%
    
    class(order_date_dttm)

# 1.3 Lubridate Functions ----

# 1.3.1 Extractor independently day, month, year ----

"feb 01 2018" %>% mdy() %>% class()
"06/01/16 12:30:15" %>% mdy_hms() #%>% class()
"02/01/18" %>% mdy()
"January 1, 1985" %>% mdy()

#   __mdy() => Conversion in (month - day - year) and labels with month(), wday()----
"2011-01-01" %>% ymd() %>% year()

# Month 
"2011-01-01" %>% ymd() %>% month(label = TRUE, abbr = FALSE)

# weekday 
"2011-01-01"  %>% ymd() %>% wday(label = TRUE, abbr = FALSE)

# Day 
"2011-01-01"  %>% ymd() %>% day()

# Helpers
now()

# 1.3.2 Periods & Durations - Add/subtract time to/from a date ----

today() + days(2)

today() + ddays(2) + dyears(1)

#   __years() => Add years in the date ---- 

today() + years(1)   #Period

# 1.3.3 Duration ----
today() + dyears(1)  #Duration

#   __Intervals() => Calculate time-based distance ----

i <- interval(today(), today() + ddays(2))
# interval / ddays, dminutes() = how many days, minutes
# contains in the interval - number of days elapsed 
# in interval 
i / ddays(1) 
i / dminutes(1)

# difference in numbers of day inside a timeseries ----
order_date_tbl %>% 
    mutate(today = today())  %>%
    mutate(diff_days = interval(order_date, today) / ddays(1))
    
# 2.0 Time-Based Data Grouping ----

# 2.1 Sales by year ----

bike_sales_y_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    
    # Lubridate 
    mutate(order_date = ymd(order_date)) %>%
    mutate(year = year(order_date)) %>%
    
    #group_by + summarize 
    group_by(year) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup()

bike_sales_y_tbl

bike_orderlines_tbl %>% 
    select(order_date, total_price) %>%
    
    # Lubridate
    mutate(order_date = ymd(order_date)) %>%
    mutate(
        year = year(order_date), 
        month = month(order_date, label = TRUE, abbr = TRUE)
    ) %>%
    
    # group_by() + summarise()
    group_by(year, month) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup()

#   __floor_Date => (reduces a data to the nearest unit time ----
#x <- ymd_hms("2009-08-03 12:01:59.23")
#floor_date(x, ".1s")     --> "2009-08-03 12:01:59 UTC"
# floor_date(x, "second") --> "2009-08-03 12:01:59 UTC"
#floor_date(x, "minute")  --> "2009-08-03 12:01:00 UTC"
#floor_date(x, "hour")    --> "2009-08-03 12:00:00 UTC"
#floor_date(x, "day")     --> "2009-08-03 UTC"
#floor_date(x, "week")    --> "2009-08-02 UTC"
#floor_date(x, "month")   --> "2009-08-01 UTC"
#floor_date(x, "bimonth") --> "2009-07-01 UTC"
#floor_date(x, "quarter") --> "2009-07-01 UTC"
#floor_date(x, "season")  --> "2009-06-01 UTC"

# Sales by month ----

bike_sales_m_tbl <- bike_orderlines_tbl %>%
    select(order_date, total_price) %>%
    
    #Lubridate 
    mutate(order_date = ymd(order_date)) %>%
    mutate(
        year = year(order_date), 
        month = month(order_date, label = TRUE, abbr = TRUE)) %>%
    
    # __floor_date() => pour rapporter les dates à l'unité temporelle "mensuelle" MOIS ---- 
    mutate(year_month = floor_date(order_date, unit = "month")) %>%
    
    # group_by + summarize 
    group_by(year, month) %>%
    summarise(sales = sum(total_price))

# 3.0 Measuring Change ----

# 3.1 Difference from most recent observation ----

#   aligning past observations (lagging) with future observations (present) ----
#   __lag() => shift a time series by n lags useful for comparing previous values in a vector ----

 bike_sales_y_tbl %>%
    mutate(sales_lag_1 = lag(sales, n = 1)) %>%
    
# 3.2 Handle NA (2 methods possible) ---- 

    # method1.  __fill() ----
    #fill(sales_lag_1, .direction = "up")
    
    # method2.  __mutate() ----
    mutate(sales_lag_1 = case_when(
        is.na(sales_lag_1) ~ sales, 
        TRUE ~ sales_lag_1
        )) %>%
    
    # Difference between following years and percentage 
    mutate(diff_1 = sales - sales_lag_1) %>%
    mutate(pct_diff = diff_1/sales_lag_1) %>%
    
    # in percentage writing 
    mutate(pct_diff_1_chr = scales::percent(pct_diff))

#   __function() => application FUNCTION() ----
calculate_pct_diff <- function (data) {    
    
    data %>% mutate(sales_lag_1 = lag(sales, n = 1)) %>%
        
    mutate(sales_lag_1 = case_when(
        is.na(sales_lag_1) ~ sales, 
        TRUE ~ sales_lag_1)) %>%
        # Difference between following years and percentage 
        mutate(diff_1 = sales - sales_lag_1) %>%
        mutate(pct_diff = diff_1/sales_lag_1) %>%
        # in percentage writing 
        mutate(pct_diff_1_chr = scales::percent(pct_diff))
}
# apply the function to calculate the difference in month 
bike_sales_m_tbl %>% calculate_pct_diff()

# 3.3 Difference from first observation ----
#   __first() => We want to compare the yearly value with the first year (2011) value 

bike_sales_y_tbl %>% 
    mutate(sales_2011 = first (sales)) %>%
    mutate(diff_2011 = sales - sales_2011) %>%
    mutate(pct_diff_2011 = diff_2011 / sales_2011) %>%
    mutate(pct_diff_2011_chr = scales::percent(pct_diff_2011))

#   __first() => compare the sales of the rest of year with the january month ----
bike_sales_m_tbl %>% 
    group_by(year)%>% 
    mutate(sales_jan = first(sales)) %>% 
    mutate(diff_jan = sales - sales_jan, 
           pct_diff_jan = diff_jan / sales_jan, 
           pct_diff_jan_symboll = scales::percent(pct_diff_jan)) %>%
    ungroup()


# 4.0 Cumulative Calculations ----
#   __cumsum() => calculate the cumulative of a numeric variables

bike_sales_y_tbl %>% 
    mutate(cumulative_sales = cumsum(sales)) %>%
    mutate(cumulative_sales_pct = cumulative_sales / sum(sales))

bike_sales_m_tbl %>% 
    group_by(year) %>%
    mutate(cumulative_sales = cumsum(sales)) %>%
    mutate(cumulative_sales_pct = cumulative_sales /sum(sales)) %>%
    mutate(cumulative_sales_pct_chr = scales::percent(cumulative_sales_pct))

# 5.0 Rolling Calculations somme/moyenne mobile rollmean, rollsum(), rollapply() () ----

#   __rollmean() - contains in the library ZOO ---- 
# (k = unit element for rolling, na.pad = indispensable, align ="right", 
# fill = combler les NA)
# you can also grouping before rolling in order to respect the yearly division ----
bike_sales_m_tbl %>% ungroup() %>%
    mutate(roll_mean_3mo = rollmean(sales, k=3, na.pad = TRUE, align = "right")) %>% view()

# 6.0 Filtering Date Ranges ---- 

#   __between(left= , right=) +  __filter() => extract a period of time ----
bike_orderlines_tbl %>%
    mutate(order_date = ymd(order_date)) %>%
    filter(order_date %>% 
               between(left = ymd("2012-01-01"), right = ymd("2013-12-31"))) %>%
    
#   __tail() => View the last rows of your tibble ----
    tail()

bike_orderlines_tbl %>%
    mutate(order_date = ymd(order_date)) %>%
    filter(year(order_date) %in% c(2012, 2013))
    



