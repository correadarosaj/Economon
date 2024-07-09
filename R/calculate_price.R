calculate_price = function(alpha, prices = seq(.01, 100, .01), Q0_S = 100){
  data.frame(price = prices, demand = calculate_QS(prices, alpha_S = alpha)) %>%
    mutate(log_price = log10(price), log_demand = log10(demand),
           euclidean_distance = sqrt((log_demand - (Q0_S/2))^2),
           new_price = min(euclidean_distance, na.rm = T) == euclidean_distance) %>%
    dplyr::filter(new_price) %>% .$price %>% return()
}
