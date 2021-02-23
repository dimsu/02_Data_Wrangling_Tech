# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TEXT MANIPULATION => LIBRARY (STRING) ----

library(tidyverse)
library(lubridate)

bike_orderlines_tbl <- read_rds("../00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bike_orderlines_tbl

bikes_tbl <- readxl::read_excel("../00_data/bike_sales/data_raw/bikes.xlsx")

bikes_tbl


# 1.0 Basicsgft

# 1.1 Detection: Used with filter() ----

# Vector
#   __str_detect() => detect a particuliar pattern
c("Supersix Evo Hi-Mod Team", "Supersix Evo Ultegra 3") %>%
    str_detect(pattern = "Supersix")

# Tibble
#   __as.numeric => transform a TRUE/FALSE in a binary 1/0 
bikes_tbl %>%
    select(model) %>%
    mutate(supersix = model %>% str_detect("Supersix") %>% as.numeric()) %>%
    mutate(black = model %>% str_detect("Black") %>% as.numeric())

# 1.2 Case & Concatenation ----


# Case
bikeshop_name <- "Supersix Evo Hi-Mod Utegra"

str_to_upper(bikeshop_name)
str_to_lower(bikeshop_name)

# Concatenation

# Vector
order_id <- 1
order_line <- 1

str_c("Order Line: ", order_id, ".", order_line, 
      " sent to customer: ", bikeshop_name, 
      sep = "") %>% 

#   __str_glue => glue str variables in a one , declared variables should be in {} ----
#   __str_glue produce a glue and str_c produce a character ----
str_glue("Order Line : {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}") 

# Tibble
bike_orderlines_tbl %>%
    select(bikeshop_name, order_id, order_line) %>%
    mutate(purchase_statement = str_glue("Order Line : {order_id}.{order_line} sent to Customer: {str_to_upper(bikeshop_name)}"
                                         )%>% as.character())

# 1.3 Separating Text: See tidyr::separate() ----

# Vector

#   __str_split() => split element => the output is a List or a Matrix
c("Road - Elite Road - Carbon", "Road - Elite Road") %>% str_split(pattern = " - ", simplify = TRUE) %>% class()


# Tibble
#   __separate() => separate a column following a separator/pattern (sep = "-")
bikes_tbl %>%
    select(description) %>%
    separate(col  = description, 
             into = c("category_1", "category_2", "frame_material"), 
             sep = " - ", 
             remove = FALSE)

# 1.4 Trimming Text ----
#   __str_trim => supprimer l'espace vide en début/fin d'une chaine de caractère ----
#    str_trim(string, side = c("both", "left", "right"))
" test with space   " %>% str_trim(side = "both")

# 1.5 Replacement: Used with mutate() [and optionally case_when()] ----

# Vector
#   __str_replace => replace a pattern in a character ----
c("CAAD12", "CAAD", "CAAD8") %>% str_replace(pattern = "[0-9]", replacement = "")

#   __str_replace_all => replace all the pattern in the character 

# Tibble
#   __regex() => symboles alpha-numériques ("[]") ----
bikes_tbl %>% 
    select(model) %>%
    mutate(model_num_removed = model %>% str_replace_all("[0-9]", "") %>% str_trim())

# 1.6 Formatting Numbers ----
# values
value <- 1e6

#   __scales::number(prefix, suffix, big.mark = ) ----
# by default (without big.mark is space separator )

# 1.6.1 Add prefix or currency symbol ----
(value / 1e6) %>% scales::number(prefix = "$", suffix = "M")

value %>% scales::number(prefix = "£", big.mark = ",")

value %>% scales::dollar(scale = 1/1e6, suffix = "M")

# 1.6.2 percents ----
pct <- 0.15 

pct %>% scales::percent()#scales::number(scale = 100, suffix = "%")

# 1.7 Formatting Column Names ----

# 1.7.1 Replacing text in column names ----
#   __str_replace => replace (old, new)
bike_orderlines_tbl %>% set_names(names(.) %>% str_replace("_", ".") %>% str_to_upper())


# 1.7.2 Appending text to column names ----
#   __set_names(str_glue("{}') => 
bike_orderlines_tbl %>% set_names(str_glue("gg_{names(.)}_bi")) #%>% class()

# 1.7.3 Appending text to specific column names ----
#   __rename_at() => rename a specific or a bunch of column in a particular order ----
#   __rename_with() => ----

bike_orderlines_colnames_tbl <- bike_orderlines_tbl %>% rename_at(
    .vars = vars(model:frame_material), 
    .funs = ~ str_glue("prod_{.}")) %>%
    rename_at(vars(bikeshop_name:state), 
                ~ str_glue("customer_{.}")) %>% glimpse()

bike_orderlines_colnames_tbl %>% select(contains("customer"))

# 2.0 Feature Engineering with Text -----
# Investigating "model" and extracting well-formatted features ----

bikes_tbl %>% 
    
    select(model) %>%
    
    # Fix text typo 
    mutate(model = case_when(
        model == "CAAD Disc Ultegra" ~ "CAAD12 Disc Ultegra",
        model == "Syapse Carbon Tiagra" ~ "Synapse Carbon Tiagra",
        model == "Supersix Evo Hi-Mod Utegra" ~ "Supersix Evo Hi-Mod Ultegra",
        TRUE ~ model
    )) %>%
    
    #Separate column model into different category (models)
    separate(col = model, 
             into = str_c("model_", 1:7) , 
             sep = " " , 
             remove= FALSE, 
             convert = FALSE, 
             extra = "drop" , 
             fill = "right") %>%
    
    # creating a "base" feature
    mutate (model_base = case_when (
        
        # Fix Supersix Evo
        # detecter dans la colonne model_1, l'éléments "supersix". si l'élement 
        # "supersix" s'y trouve, écrire dans la colonne model_base la combinaison 
        # (str_c) des éléments des colonnes model_1 et model_2
        
        str_detect(
            str_to_lower(model_1), "supersix") 
        ~  str_c(model_1, model_2, sep = " "), 
        
        # Fix Fat CAAD bikes
        str_detect(str_to_lower(model_1), "fat") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Beast of the East
        str_detect(str_to_lower(model_1), "beast") ~ str_c(model_1, model_2, model_3, model_4, sep = " "),
        
        # Fix Bad Habit
        str_detect(str_to_lower(model_1), "bad") ~ str_c(model_1, model_2, sep = " "),
        
        # Fix Scalpel 29
        str_detect(str_to_lower(model_2), "29") ~ str_c(model_1, model_2, sep = " "),
        
        # catch all
        TRUE ~ model_1)) %>%
        
        # Get "tier" feature
#   __str_replace() => str_replace(vector_a_remplacer,  remplacements)
    
        mutate(model_tier = model %>% str_replace(model_base, replacement = "") %>% str_trim()) %>%
        
      #delete unnecessary column following a character 
    
#   __matches() => Eq à contains() 

      select(-matches("[0-9]")) %>%
    
    # Create Flags
    mutate(
        black     = model_tier %>% str_to_lower() %>% str_detect("black") %>% as.numeric(),
        hi_mod    = model_tier %>% str_to_lower() %>% str_detect("hi-mod") %>% as.numeric(),
        team      = model_tier %>% str_to_lower() %>% str_detect("team") %>% as.numeric(),
        red       = model_tier %>% str_to_lower() %>% str_detect("red") %>% as.numeric(),
        ultegra   = model_tier %>% str_to_lower() %>% str_detect("ultegra") %>% as.numeric(),
        dura_ace  = model_tier %>% str_to_lower() %>% str_detect("dura ace") %>% as.numeric(),
        disc      = model_tier %>% str_to_lower() %>% str_detect("disc") %>% as.numeric()
    ) 



