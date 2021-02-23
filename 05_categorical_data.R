# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# CATEGORICAL DATA MANIPULATION ----

library(tidyverse)
library(tidyquant)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl



# 1.0 Factor Basics ----

# What is a Factor?
# A way of managing categorical data

# Why do we want factors? 
# 1. Can group numeric values into bin (think price = low, medium, high)
# 2. Can reorder categories for visualization (fct_reorder)
# 3. Can manipulate categories much eaiser (fct_lump)
# 4. Machine learning and modeling algorithms may require factor data type for categorical data. 

# 2.0 Motivating Example -----

# Manipulation

sales_by_cat_2_tbl <- bike_orderlines_tbl %>% select(category_2, total_price) %>%
    
    group_by(category_2) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup() %>%
    arrange(desc(sales)) %>%
    
    # __as_factor() => transform to category column (character) into a factor type (fct) numerical ----
    # 1 = HIGHT number -----> 10 = LOW number
    # __fct_rev()   => factor reverse = reverse the order/level made in the as_factor() operation ----
    mutate(category_2 = category_2 %>% as_factor() %>% fct_rev())
    

# Plotting
sales_by_cat_2_tbl %>%
    ggplot(aes(x = sales, y = category_2)) +
    geom_point(size = 5, color = "#2c3e50" ) + 
    labs(title = "Sales By Category 2") + 
    scale_x_continuous (labels = scales::dollar_format()) + 
    theme_tq() + 
    
    # ___expand_limits() Expand the X axis to print the 0$ value right to the origin of the axis ----
    expand_limits(x =0)

# Function plot_point_sales ----

plot_sales <- function(data) {
    data %>%
        ggplot(aes(x = sales, y = category_2)) +
        geom_point(size = 5, color = "#2c3e50" ) + 
        labs(title = "Sales By Category 2") + 
        scale_x_continuous (labels = scales::dollar_format()) + 
        theme_tq() + 
        
        # ___expand_limits() Expand the X axis to print the 0$ value right to the origin of the axis ----
    expand_limits(x =0)
}

sales_by_cat_2_tbl %>% plot_sales()

# 3.0 Forcats Basics ----


# 3.1 Inspecting Factors ----

# Vector
#   __levels() => display the level of the column factor 
sales_by_cat_2_tbl %>% pull(category_2) %>% levels()

# __as.numeric() => display the factor associated to each category 
# __as.character() => displat the category as a character 
sales_by_cat_2_tbl %>% pull(category_2) %>% as.numeric()


# Tibble
sales_by_cat_2_tbl %>% 
    mutate(
        label = category_2 %>% as.character(), 
        value = category_2 %>% as.numeric() )

# 3.2 Creating Factors: as_factor() vs as.factor() ----

#   __as.factor => make categorical data as factor class them function of the alphabetic order ----
#   __as_factor => make categorical data as factor class them function of the numerical value ----
sales_by_cat_2_tbl %>%
    mutate(
        category_2  = as.character(category_2), 
        category_2_as_factor = as_factor(category_2) %>% as.numeric(), 
        category_2_as.factor = as.factor(category_2) %>% as.numeric()
    )

# 3.3 Reording Factors: fct_reorder() and fct_rev() ----

#   __levels() => (apply only on factor cate) display the levels factor once you apply the as_factor() ---- 
#   __fct_reorder(.fct, X, .fun, .desc) => reorder factor levels by sorting along  variable X (numeric) ----
#   __fct_rev() => reverse the level order (fct_reorder VS fct_rev) ----

sales_by_cat_2_tbl %>% 
    arrange(desc(sales)) %>%
    mutate(sales_neg = -sales ) %>% 
    mutate(category_2 = category_2 %>% fct_reorder(sales_neg) %>% fct_rev(), 
           value     =  category_2 %>% as.numeric()) %>% plot_sales()

# 3.4 Time-Based Reordering: fct_reorder2() ----

#   __floor_date("quarter") => put the date data in quarter interesting ----
sales_by_cat_2_q_tbl <- bike_orderlines_tbl %>%
    mutate(order_date = order_date %>% floor_date("quarter") %>% ymd()) %>%
    group_by(category_2, order_date) %>%
    summarise(sales = sum(total_price)) %>%
    ungroup()

#   __fct_reorder => order factor in 2D display, the factor is mapped to non-position aesthetic ----

sales_by_cat_2_q_tbl %>% 
    
    mutate(category_2 = category_2 %>% fct_reorder2(order_date, sales)) %>%
    
    ggplot(aes( x= order_date,  y = sales, color = category_2)) + 
    geom_point() + 
    geom_line() + 
    facet_wrap(~ category_2) + 
    theme_tq() + 
    scale_color_tq() + 
    scale_y_continuous(labels = scales::dollar_format(scale = 1e-6, suffix = "M"))

# 3.5 Creating "Other" Category - fct_lump() & fct_relevel() ----


#   __fct_lump() => put irrelevant category in the same basket ----
#   __fct_lump(.fct, n= number of relevant class you want preserve, w = numeric vector weighting the class, other_level = value of level used for "other") ----
sales_by_cat_2_tbl %>%
    mutate(category_2 = category_2 %>% fct_lump(n = 6, w = sales, 
                                                other_level = "All other bike categories")) %>%
    group_by(category_2) %>%
    summarise(sales = sum(sales))  %>%

    
#   __fct_relevel(unordered factor, Level we want to move) => Reorder a precise of factor in the column ---- 
mutate(category_2 = category_2 %>% fct_relevel("All other bike categories", after = 0)) %>%
    
    plot_sales()
