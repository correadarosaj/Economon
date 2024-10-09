calculate_price = function(alpha, prices = seq(.01, 100, .01), Q0_S = 100, divisor = 2){
  data.frame(price = prices, demand = calculate_QS(prices, alpha_S = alpha)) %>%
    mutate(euclidean_distance = sqrt((demand - (Q0_S/divisor))^2),
           new_price = min(euclidean_distance, na.rm = T) == euclidean_distance) %>%
    dplyr::filter(new_price) %>% .$price %>% return()
}
