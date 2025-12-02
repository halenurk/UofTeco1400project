set.seed(42)
library(gmm)
library(haven)


url <- "https://github.com/halenurk/UofTeco1400project/raw/refs/heads/main/CEX_Cleaned.rds"
download.file(url, "project_data_v2.rds", mode = "wb")
CEX_data <- readRDS("project_data_v2.rds")


## Data prep & cleaning
CEX_repli <- read_dta("CEX_1980_2012.dta")

CEX_panel <- CEX_repli %>%
  select(
    household_id,
    survey_year,
    total_consumption,
    consumption_nondurables,
    total_income,
    durables,
    hh_age,
    family_size
  )

CEX_panel <- CEX_panel %>%
  mutate(
    C_t = consumption_nondurables,   # nondurable consumption
    K_t = durables,                  # durable stock
    TC  = total_consumption,         # total consumption
    Y_t = total_income,              # income
    I_t = TC - C_t                   # durable "investment"
  )

CEX_gmm <- CEX_panel %>%
  # 1) Drop rows with missing key variables
  filter(
    !is.na(C_t),
    !is.na(K_t),
    !is.na(TC),
    !is.na(Y_t),
    !is.na(hh_age),
    !is.na(family_size)
  ) %>%
  # 2) Drop nonpositive values where we need logs / powers
  filter(
    C_t     > 0,
    TC      > 0,
    Y_t     > 0,
    K_t     > 0,
  )
##

## make variables for Euler equation
CEX_gmm <- CEX_gmm %>%
  group_by(household_id) %>%
  arrange(survey_year, .by_group = TRUE) %>%
  mutate(
    C_tp1 = dplyr::lead(C_t, n = 1),   # C_{t+1}
    K_tp1 = dplyr::lead(K_t, n = 1),   # K_{t+1}
    
    tilde_C_tp1 = C_tp1 / C_t,         # C_{t+1} / C_t
    tilde_K_tp1 = K_tp1 / C_t          # K_{t+1} / C_t
  ) %>%
  ungroup()

## logs for instruments
CEX_gmm <- CEX_gmm %>%
  mutate(
    logC_t = log(C_t),
    logY_t = log(Y_t),
    logK_t = log(K_t)
  )
##

## create lags for instruments and take logs
CEX_gmm <- CEX_gmm %>%
  group_by(household_id) %>%
  arrange(survey_year, .by_group = TRUE) %>%
  mutate(
    C_tm1 = dplyr::lag(C_t),
    K_tm1 = dplyr::lag(K_t)
  ) %>%
  ungroup()
CEX_gmm <- CEX_gmm %>%
  mutate(
    logC_tm1 = log(C_tm1),
    logK_tm1 = log(K_tm1)
  )
##

## remove t+1 NA values
CEX_gmm <- CEX_gmm %>%
  filter(!is.na(C_tp1))

## merge average_index (P_D)
CEX_data <- CEX_data %>%
  filter(!is.na(year), year != "1979")
P_D_by_year <- CEX_data %>%
  group_by(year) %>%
  summarise(
    P_D = mean(Average_Index, na.rm = TRUE),
    .groups = "drop"
  )
P_D_by_year <- P_D_by_year %>%
  mutate(year = as.integer(as.character(year)))
CEX_gmm <- CEX_gmm %>%
  rename(year = survey_year)
CEX_gmm <- CEX_gmm %>%
  left_join(P_D_by_year, by = "year")
##

## keep non-negative I_t
CEX_gmm <- CEX_gmm %>%
  filter(!is.na(I_t), I_t > 0)
##

## create sample moment function
gmm_moments <- function(par, data) {
  #theta <- par[1]
  phi <- par[1]
  psi <- par[2]
  #gamma <- par[4]

  
  with(data, {
    m <- 0.768 * tilde_C_tp1^(-3) +
      0.96  * K_tp1^(-psi)/C_t^(-3) * 1/ (P_D + phi * I_t) -
      1
    
    cbind(
      m * 1,
      m * logC_tm1,
      m * logK_tm1,
      m * family_size
      
    )
  })
}
##

## To check instrument collinearity
Z <- CEX_gmm_lag %>%
  transmute(
    const       = 1,
    logC_t      = logC_t,
    logK_t      = logK_t,
    logC_tm1      = logC_tm1,
    logK_tm1      = logK_tm1,
    family_size = family_size,
    logI_t = log(I_t)
  )

Z_mat <- as.matrix(Z)
kappa(Z_mat)
##

## to check instrument relevance
m_C_next <- lm(
  log(C_tp1) ~ logC_t + logK_t + logC_tm1 + logK_tm1 + family_size,
  data = CEX_gmm_lag
)
m_K_next <- lm(
  log(K_tp1) ~ logC_t + logK_t + logC_tm1 + logK_tm1 + family_size,
  data = CEX_gmm_lag
)
summary(m_C_next)
summary(m_K_next)
##


## make subset for testing
CEX_gmm_sub <- CEX_gmm %>%
  sample_frac(size = 0.10)

## remove t-1 lag
CEX_gmm_lag <- CEX_gmm %>%
  filter(!is.na(C_tm1))
CEX_gmm_lag_sub <- CEX_gmm_lag %>%
  sample_frac(size = 0.10)

# set initial parameter values
par0 <- c(phi = 0.1, psi = 3)

## fit gmm
gmm_fit_iterative <- gmm(
  g  = gmm_moments,
  x  = CEX_gmm_lag,
  t0 = par0,
  type = "twoStep",
  vcov = "iid",
  optfct = "optim"
)
summary(gmm_fit_iterative)
