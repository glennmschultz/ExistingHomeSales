  
  require(quantmod)
  
  getSymbols('MORTGAGE30US', src = 'FRED')
  index(MORTGAGE30US)
  
  ggplot(MORTGAGE30US['2012-01-01/2017-03-02'], aes(x = Index, y = MORTGAGE30US/100))+
    geom_line(color = 'grey') +
    geom_point(color = 'blue', size = 3) +
    labs(y = "FHLMC 30-year Mortgage Rate", x = "Date") +
    scale_y_continuous(labels = percent_format()) +
    theme_minimal()