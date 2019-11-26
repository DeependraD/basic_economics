# requirements
require(tidyverse)
require(tsibble)
require(tidyquant)

# in complementary goods, utility is limitted by the good with smallest quantity
complementary_goods1 <- tribble(~"left_shoe", ~"right_shoe", ~"utility", ~"utility_level",
                               2, 4, 100, 1, 
                               3, 2, 100, 1,
                               12, 2, 100, 1, 
                               2, 2, 100, 1,
                               7, 2, 100, 1,
                               2, 5, 100, 1,
                               8, 2, 100, 1,
                               2, 10, 100, 1)

complementary_goods2 <- tribble(~"left_shoe", ~"right_shoe", ~"utility", ~"utility_level",
                                4, 5, 200, 2,
                                5, 4, 200, 2,
                                6, 4, 200, 2,
                                10, 4, 200, 2,
                                4, 12, 200, 2,
                                7, 4, 200, 2,
                                4, 6, 200, 2)

complementary_goods <- ggplot() +
  geom_segment(aes(x = `left_shoe`, y = `right_shoe`, xend = 2, yend = 2), data = complementary_goods1) +
  geom_segment(aes(x = `left_shoe`, y = `right_shoe`, xend = 4, yend = 4), data = complementary_goods2) +
  labs(title = "Complementary relationship between left and right shoes of a pair", 
       x = "Left shoe", 
       y = "Right shoe") +
  expand_limits(x = 0, y = 0) +
  scale_x_continuous(breaks = seq(2, 12, by = 2)) +
  scale_y_continuous(breaks = seq(2, 12, by = 2)) +
  theme_bw()
complementary_goods

# ggsave("./complementary_goods.png", complementary_goods, units = "in", 
#        dpi = 250, width = 6, height = 4)

# in supplementary goods 
# marginal rate of substitution of quantity x for quantity y should be constant
# the function below could be thought of as data generating process 

# i. constant marginal rate of substitution
# here MRS of y for x is given by slope coefficient
tea <- c(2, 3, 5, 6, 7, 8, 10)
coffee <- function(x){
  -2*x + 20 # MRS is "-2"
}
coffee(tea)

ggplot() +
  geom_line(aes(tea, coffee(tea))) +
  labs(x = "Tea", y = "Coffee") +
  theme_bw()

# ii. increasing marginal rate of substitution
mrs <- c(0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)
tea <- c(1, 3, 5, 7, 9, 11, 13)
coffee <- function(x, mrs){
  mrs*x + 20
}

coffee(tea, mrs)

ggplot(data = Hmisc::bezier(tea, coffee(tea, mrs)) %>% 
         bind_cols()) +
  geom_line(aes(x, y)) +
  labs(x = "Tea", y = "Coffee") +
  theme_bw()

# iii. decreasing marginal rate of substitution/ synonymous to indifference curve
bz_curves <- data.frame(`Tea` = c(12L, 8L, 5L, 3L, 2L, 1L), 
                        `Coffee` = c(1L, 2L, 3L, 5L, 8L, 12L))

bezier::bezier(t = seq(0,1,length.out = 50), bz_curves) %>% 
  as_tibble() %>% 
  ggplot2::ggplot(aes(y = V1, x = V2)) + 
  ggplot2::geom_line(size = 1.0) +
  xlab("Cups of tea") + 
  ylab("Cups of coffee")
