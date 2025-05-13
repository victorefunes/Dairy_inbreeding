library(tidyverse)
library(estimatr)
library(statar)
library(modelsummary)
library(lfe)
library(DRDID)
library(fixest)

# Set parameters
set.seed(1)
N <- 1000
T <- 2
tau <- 10

f <- function(x){
  y <- x
  return(y)
}

g <- function(x){
  y <- exp(x)
  return(y)
}

df_i <- tibble(
    i = 1:N,
    x = rnorm(length(i)),
    v = rnorm(length(i)),
    d = (runif(length(i)) < exp(g(x)) / (1 + exp(g(x))) ) |> as.integer()
  )

df <- expand_grid(i = 1:N, t = 0:(T - 1)) |>
  left_join(df_i, by = "i") |>
  mutate(
    e_0 = rnorm(length(i)),
    e_1 = rnorm(length(i)),
    y_0 = f(x) * t + e_0,
    y_1 = tau + x + f(x) * t + e_1,
    y = (1 - d) * y_0 + d * (1 - t) * y_0 + d * t * y_1)

df |> 
  datasummary_skim()

# Heterogeneity
df |> 
  mutate(t = as.factor(t)) |>
  ggplot(aes(x = x, y = d, color = t, group = t)) + 
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE) + 
  scale_color_viridis_d() + 
  theme_classic()

df |> 
  mutate(t = as.factor(t)) |>
  ggplot(aes(x = x, y = y_0, color = t, group = t)) + 
  geom_point() + 
  scale_color_viridis_d() + 
  theme_classic()

df |> 
  mutate(t = as.factor(t)) |>
  ggplot(aes(x = x, y = y_1, color = t, group = t)) + 
  geom_point() + 
  scale_color_viridis_d() + 
  theme_classic()

df |> 
  mutate(t = as.factor(t)) |>
  dplyr:::mutate(tau_i = y_1 - y_0) |>
  ggplot(aes(x = x, y = tau_i, color = t, group = t)) + 
  geom_point() + 
  scale_color_viridis_d() + 
  theme_classic()

df |> 
  filter(t == 1, d == 1) |>
  summarise(mean(y_1 - y_0))

# TWFE estimator
model_twfe <- felm(data = df, 
                   formula = y ~ d:t | i + t)
summary(model_twfe)

# outcome regression
model_ordid <- ordid(data = df, 
                     yname = "y", 
                     tname = "t", 
                     idname = "i",
                     dname = "d", 
                     xformla = ~ x)
summary(model_ordid)

# IPW
model_ipwdid <- ipwdid(data = df, 
                       yname = "y",
                       tname = "t",
                       idname = "i",
                       dname = "d", 
                       xformla = ~ x)
summary(model_ipwdid)

# Doubly-robust DID
model_drdid <- drdid(data = df, 
                     yname = "y", 
                     tname = "t", 
                     idname = "i",
                     dname = "d", 
                     xformla = ~ x)
summary(model_drdid)

## Multiple periods
set.seed(1)
N_g <- 100
T <- 3
G <- T
N <- G * N_g
tau <- expand_grid(
    t = 1:T,
    g = 1:G) %>%
  mutate(tau = 1:length(t))

# Make data
df <- expand_grid(
    t = 1:T,
    i = 1:N) %>%
  mutate(g = ceiling(i / 100)) %>%
  left_join( tau,
    by = c("g", "t"))

df <- df %>%
  mutate(y_0 = 0,
    y_1 = tau + rnorm(length(t))) %>%
  mutate(d = (g > 1) * (t >= g),
    y = y_1 * d + y_0 * (1 - d))

# POLS
model_ols <- lm(data = df,
  formula = y ~ I((g == 2)&(t == 2)) + I((g == 2) & (t == 3)) + 
    I((g == 3) & (t == 3)) + as.factor(g) + as.factor(t))

modelsummary(model_ols, coef_omit = "as.factor")

model_fe <- feols(y~i(g, t, ref = "1", ref2 = "1")|i+t, data = df)
etable(model_fe)

# TWFE
model_twfe <- lfe::felm(data = df,
  formula = y ~ I((g == 2)&(t == 2)) + I((g == 2) & (t == 3)) + 
    I((g == 3) & (t == 3)) | i + t)
modelsummary(model_twfe)

# Separate DID for tau_2
did_g2t2 <- df %>%
  filter((g == 1 & t <= 2) |(g == 2 & t <= 2)) %>%
  lfe::felm(data = .,formula = y ~ d | g + t)
modelsummary(did_g2t2)

# Separate DID for tau_3
did_g2t3 <- df %>%
  filter((g == 1 & (t == 1 | t == 3)) | (g == 2 & (t == 1 | t == 3))) %>%
  lfe::felm(data = ., formula = y ~ d | g + t)
modelsummary(did_g2t3)

did_g3t3 <- df %>%
  filter((g == 1) | (g == 3)) %>%
  lfe::felm(data = ., formula = y ~ d | g + t)
modelsummary(did_g3t3)

modelsummary(list(model_twfe, did_g2t2, did_g2t3,  did_g3t3))

# Common trends hold only in the limit
set.seed(1)
N_g <- 100
T <- 3
G <- T
N <- G * N_g
tau <- expand_grid(
    t = 1:T,
    g = 1:G) %>%
  mutate(tau = 1:length(t))


df <- expand_grid(
    t = 1:T,
    i = 1:N) %>%
  mutate(g = ceiling(i / 100)) %>%
  left_join(tau, by = c("g", "t"))

df <- df %>%
  mutate(y_0 = rnorm(length(t)),
    y_1 = tau + rnorm(length(t))) %>%
  mutate(d = (g > 1) * (t >= g),
    y = y_1 * d + y_0 * (1 - d))

model_ols <- lm(data = df, 
                formula = y ~ I((g == 2)&(t == 2)) + I((g == 2) & (t == 3)) + 
                  I((g == 3) & (t == 3)) + as.factor(g) + as.factor(t))
modelsummary(model_ols, coef_omit = "as.factor")

model_twfe <- lfe::felm(
  data = df,
  formula = y ~ I((g == 2)&(t == 2)) + I((g == 2) & (t == 3)) + 
    I((g == 3) & (t == 3)) | i + t)
modelsummary(model_twfe)