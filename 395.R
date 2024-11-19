library(tidyverse)
library(modelsummary)

# Random Variables --------------------------------------------------------
set.seed(006)

N <- 200


x <- rnorm(N) + 1

y <- rnorm(N) + 1


c <- ifelse( x + y > 2 , 1 , 0)


data <- tibble(
  x = x,
  y = y,
  c = c
)

ggplot(
  data = data,
  mapping = aes(x = x, y = y)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
theme_minimal() +
  labs(title = "Scatter Plot of X and Y",  x = "X", y = "Y"
  )

lm_model_1 <- lm(y ~ x, data = data)
msummary(lm_model_1, stars = TRUE)


ggplot(
  data = data,
  mapping = aes(x = x, y = y, color = factor(c))
) +
  geom_point() +
  theme_minimal() +
  labs(title = "Scatter Plot of X and Y",  x = "X", y = "Y"
  )  


data2 <- data |>
  group_by(c) |> 
  summarize(y_r = y - mean(y), 
            x_r = x - mean(x),
  )


ggplot(
  data = data2,
  mapping = aes(x = x_r, y = y_r)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
theme_minimal() +
  labs(title = "Scatter Plot of X and Y",  x = "X", y = "Y"
  )

lm_model_2 <- lm(y ~ x + c, data = data)
msummary(lm_model_2, stars = TRUE)

lm_model_e <- lm(y_r ~ x_r , data = data2)
msummary(lm_model_e)

ggplot(data2) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = x_r, y = y), alpha = 0.5, color = "blue") 

ggplot(data2) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = x, y = y_r), alpha = 0.5, color = "blue") 

ggplot(data2) +
  geom_point(aes(x = x, y = y), alpha = 0.5) +
  geom_point(aes(x = x_r, y = y_r), alpha = 0.5, color = "blue") 

model_list <- list(
  "Model 1" = lm_model_1,
  "Model 2" = lm_model_2
)

msummary(model_list, stars = TRUE)
modelplot(model_list)

modelplot(lm_model_e)

