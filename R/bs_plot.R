library(dplyr)
library(ggplot2)

df <- data.frame(
  Name  = c(paste0("Item", 1:3)),
  Value = c(10, 20, 70)
)

df_plot <- df %>% 
  mutate(
    xmax = cumsum(Value),
    xmin = dplyr::lag(xmax, n = 1L, default = 0),
    ymax = nrow(df):1,
    ymin = (nrow(df)-1):0
  )

ggplot(df_plot, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) + 
  geom_rect()


