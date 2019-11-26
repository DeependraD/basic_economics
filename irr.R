# require tidyverse
require(tidyverse)

# # investment return analysis

# suppose a firm has a capital expenditure (initial investment) of
C0 <- 300000

# it has yearly variable cost and return of 
cost <- c(110000, 75000, 75000, 110000, 120000)
benefit <- c(140000, 120000, 320000, 260000, 230000)

# at discounting rate (opportunity cost of capital) at dr%, and
# at `flow_start_year` years after investment
dr <- 12
flow_start_year <- 10

# non discounted cash flow on each year is simply:
bc_diff <- benefit - cost

# total investment portfolio is:
bc_diff_invest <- c(-C0, bc_diff)

# for bc ratio calculation both cost and benefit should be discounted separately

# discounted cost is:
discounted_c <- map2_dbl(.x = cost, 
                         .y = seq(length.out = length(bc_diff), from = flow_start_year),
                         .f = ~(.x/(1 + dr/100)^.y))

# discounted benefit is:
discounted_b <- map2_dbl(.x = benefit, 
                         .y = seq(length.out = length(bc_diff), from = flow_start_year),
                         .f = ~(.x/(1 + dr/100)^.y))

# discounted cash flow is (note that we can used difference here):
discounted_cf <- map2_dbl(.x = bc_diff, 
                          .y = seq(length.out = length(bc_diff), from = flow_start_year),
                          .f = ~(.x/(1 + dr/100)^.y))

# same can be done by using calculator
-FinCal::pv.simple(dr/100, seq(length.out = length(bc_diff), from = flow_start_year), bc_diff)

# npv is:
discounted_cf %>% 
  sum() %>% 
  magrittr::subtract(C0)

# irr is:
FinCal::irr(bc_diff_invest)

# npv is:
FinCal::npv(r = .12, cf = bc_diff_invest)

# bc ratio is:
sum(discounted_b)/(sum(discounted_c)+C0)
