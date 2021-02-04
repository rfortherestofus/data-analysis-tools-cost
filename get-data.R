
# Packages ----------------------------------------------------------------

library(tidyverse)



# Get Basic Pricing Data --------------------------------------------------

# I'm trying to find the cheapest pricing (not including academic licenses)

# SPSS: https://www.g2.com/products/ibm-spss-statistics/pricing

spss_pricing <- tribble(
  ~users, ~price,
  1, 99*12
) %>% 
  mutate(tool = "SPSS") 

# SAS

# https://www.sas.com/store/products-solutions/cSoftware-p1.html

sas_pricing <- tribble(
  ~users, ~price,
  1, 8700
) %>% 
  mutate(tool = "SAS") 



# Stata

# https://www.stata.com/order/new/gov/single-user-licenses/dl/


stata_pricing <- tribble(
  ~users, ~price,
  1, 595,
  2, 995, 
  3, 1260, 
  4, 1525, 
  5, 1790
) %>% 
  mutate(tool = "Stata") 



# Combine Data into Single Data Frame -------------------------------------

price_data <- bind_rows(spss_pricing,
                        sas_pricing, 
                        stata_pricing) %>% 
  select(tool, everything()) %>% 
  mutate(year = 1)



# Function to Return Price ------------------------------------------------

dk_return_price <- function(tool_name, number_of_users, years) {
  price_data %>% 
    filter(tool == tool_name) %>% 
    pull(price)
}


dk_return_price("SAS", 1)

