# DS4B ----
# DATA WRANGLING OVERVIEW ----

library(tidyverse)
library(readxl)
library(fs)

fs::dir_("")
bike_tbl            <- read_excel("../00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("../00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_tbl
bike_orderlines
# 1.0 Selecting columns with select()* ----
#   __function pull => extract a column in tibble ---- 
bike_tbl %>% select(model, description)
bike_orderlines %>% select(contains(c("order", "id")))
bike_orderlines %>% pull(price)

#   __select_if => selection conditionnelle ---- 
bike_orderlines %>% select_if(~!is.numeric(.))

# 2.0 Arranging with Arrange()* and desc()* ----
#   __Arrange => order the rows of a column by a predefined order ----
#   __Desc => define the order of the arrangement ----
bike_orderlines
bike_orderlines %>% 
    select(model, price) %>% slice(0,10) %>%
    arrange(desc(price))
bike_orderlines %>% select_if(is.numeric)

# 3.0 filtering Rows with filter()*  ----
#   __str_detect() => detect a particular strg character ----
bike_tbl %>% 
    select(model, price) %>%
    filter(price > 6000, 
           model %>% str_detect("Supersix"))

# 3.1 Filtering one or more conditions ----

#   __filter() => formula filtering = extract rows that meet a criteria ----
#   __%in% => match/compare 2 vectors and sort the elements in the both ----

bike_orderlines_tbl %>%
    filter(category_2 %in% c("Trail", "Endurance Road"))

#   __symbols ==, %in%, !=, !(%in%)
bike_orderlines_tbl %>%
    filter(!(category_2 %in% c("Trail", "Endurance Road")))

#   __slice() => filtering with row number(s) ----
bike_tbl

bike_tbl %>%
    arrange(desc(price)) %>% slice(1:5)

bike_tbl %>% 
    arrange(desc(price)) %>%
    slice(1:5)

#   __distinct() => get the unique feature elements inside a column ----
bike_orderlines_tbl %>% distinct(category_1)

# 4.0 Adding calculated Columns with mutate()* = add column ----
#   __mutate() Add the calculated column  ----

# overwrite column 
bike_orderlines_prices <- bike_orderlines_tbl %>% select(order_date, model, total_price) %>% 
    mutate(total_price = 10*(total_price)) 
    
# Transformations 
bike_orderlines_prices
bike_orderlines_prices %>% 
    mutate(total_price_log = log(total_price)) %>%
    mutate(total_price_sqrt = total_price^0.5)

# 4.1 Adding Flag (mettre un marqueur sur une colonne) ----
#   __str_detect("a character") => detect "a character" in the column 
bike_orderlines_prices %>% 
    mutate(is_supersix = model %>% str_to_lower() %>% str_detect("supersix")) %>%
    filter(is_supersix)

# Binning with ntile() = converts numeric continuous data into bins (4, 3, 2, 1)
#   __ntile() = > binned a data column ----
bike_orderlines_prices %>% 
    mutate(total_price_binned = ntile(total_price, 4)) 

# 5.0 case_when() - more flexible binning ----
#   __case_when() => if_else() Excel condition ----

# 5.1 Pass Numeric to Categorical ----
bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned2 = case_when(
        total_price > quantile(total_price, 0.75) ~ "High", 
        total_price > quantile(total_price, 0.25) ~ "Medium", 
        TRUE ~ "Low"
    ))
# 5.2 Pass Text to categorical ----
bike_orderlines_prices %>%
    mutate(bike_type = case_when(
        model %>% str_to_lower() %>% str_detect("supersix") ~ "Supersix", 
        model %>% str_to_lower() %>% str_detect("jekyll") ~ "jekyll", 
        TRUE ~ "Not supersix or jekyll"
    ))

# 6.0 Grouping & summarizing with group_by()** and summarize()** ----
#   __Always ungroup() => after use the group_by() ----

bike_orderlines_tbl %>%
    summarise(
        revenue = sum(total_price))

bike_orderlines_tbl %>% 
    group_by(category_1) %>%
    summarise(revenue = sum(total_price
                ))
bike_orderlines_tbl %>%
    group_by(category_1, category_2, frame_material) %>%
                 summarise(revenue = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(revenue))

#   __Descriptive Summary Statistics functions ----
bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(count = n(), 
              avg   = mean(total_price), 
              med   = median(total_price), 
              sd    = sd(total_price), 
              min   = min(total_price), 
              max   = max(total_price), 
              ) %>%
    ungroup() %>%
    arrange(desc(count))

#   __Summarize_all() - detect missing values ----

#   __rep(chr, Xtime) => repeat a value/chr ----

#   __[ : ] => subsetting a tibble ----

bike_orderlines_tbl[1:size(.)]

bike_orderlines_missing <-  bike_orderlines_tbl %>% 
    mutate(total_price = c(rep(NA, 4),  total_price[5:nrow(.)]))

#   __sum() => returns the sum of all the values in its argument ----

# __Returns the number of NA contains in any column (symbol ~ ) ----
bike_orderlines_missing %>% summarise_all(~ sum(is.na(.)))

# Returns the percentage of missing data 
bike_orderlines_missing %>% summarise_all(~ sum(is.na(.)) / length(.))

# __Elements different to NA (non missing data) ----
bike_orderlines_missing %>% filter(!is.na(total_price))

# 6.0 Renaming columns with renames() and set_names() ----

#   __rename() => One column at a time rename(`newName`= `oldnale`)----
bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, category_1, total_price) %>%
    group_by(bikeshop_name, category_1) %>% 
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(sales))

bikeshop_revenue_tbl %>% 
    rename(
        `Bikeshop Name` = bikeshop_name, 
        `Primary Category` = category_1,
        Sales = sales
    )
#   __set_names => All columns at once in a precise order ----
bikeshop_revenue_tbl
bikeshop_revenue_tbl %>% set_names(c("Bikeshop Name", "Primary Category", "Sales"))

bikeshop_revenue_tbl %>% 
    set_names(names(.) %>% str_replace("_", " ") %>% str_to_title()) #%>%
    mutate(
        Mountain = scales::dollar(Mountain), 
        Road     = scales::dollar(Road)
    )

# 7.0 Reshaping (PIVOTING) Data with spread() and gather() ----
bike_orderlines_tbl

#   __Spread() => long to wide  ---- 
bikeshop_revenue_RESHAPE_tbl <- bikeshop_revenue_tbl %>% spread(category_1, sales) %>% 
    mutate(
        Mountain = scales::dollar(Mountain), 
        Road     = scales::dollar(Road)
    )
    
#   __gather() => wide to long ----   
bikeshop_revenue_RESHAPE_tbl %>% 
    gather(`Mountain`, `Road`,  key = "category_1",  value = "sales") %>%

# retransform the category_1 et sales in numeric (text => Numeric)
# use de \\to introduce a special character, | = or 
    mutate(sales = sales %>% str_remove_all("\\$|,") %>% as.double()) %>%
    arrange(desc(sales))

# 8.0 Joining Data by key(s) with left_join() (VLOOKUP in Excel) ----
orderlines_tbl
bike_tbl
#   __left_join()
orderlines_tbl %>% 
    left_join(y = bike_tbl, by = c("product.id" =  "bike.id"))

# 9.0 Bindind data by Row or by column with bind_rows() ans bind_col() ----

#   __bind_cols() => combine columns ----
bike_orderlines_tbl
bike_orderlines_tbl %>%
    select(-contains("order")) %>%

bind_cols(bike_orderlines_tbl %>% select(order_id))

# 10.0 Separate & Unite ----
bike_orderlines_tbl %>%
    select(order_date)  %>%
    mutate(order_date = as.character(order_date)) %>%

#   __Separate() => separate elements patterns in a column ----
    # remove = TRUE --> remove the original column 
    separate(col = order_date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
    
    #make it as numeric instead of sting 
    mutate(
        year = as.numeric(year), 
        month = as.numeric(month), 
        day = as.numeric(day)) %>%

#   __Unite() => unite elements patterns in a column ----
    # unite(data, name_of_the_output_column, columns)
    unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
    mutate(order_date_united = as.Date(order_date_united))


    