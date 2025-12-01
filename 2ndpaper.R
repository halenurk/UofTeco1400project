set.seed(42)
library(gmm)


url <- "https://github.com/halenurk/UofTeco1400project/raw/refs/heads/main/CEX_Cleaned.rds"
download.file(url, "project_data_v2.rds", mode = "wb")
CEX_data <- readRDS("project_data_v2.rds")

CEX_data <- CEX_data %>%
  mutate(
    C_t  = consumption_nondurables,  # C_t
    K_t  = durables,                 # K_t
    TC   = total_consumption,        # total consumption (TC)
    P_D  = Average_Index             # P^D_t (price of durables)
  )

CEX_data <- CEX_data %>%
  group_by(region) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    # 1) t+1 variables using dplyr::lead explicitly
    C_tp1 = dplyr::lead(C_t, n = 1),   # C_{t+1}
    K_tp1 = dplyr::lead(K_t, n = 1),   # K_{t+1}
    
    # 2) tilde variables: divide by C_t
    tilde_C_tp1 = C_tp1 / C_t,         # \tilde C_{t+1} = C_{t+1} / C_t
    tilde_K_tp1 = K_tp1 / C_t,         # \tilde K_{t+1} = K_{t+1} / C_t
    
    # 3) I_t = total consumption - nondurables
    I_t = TC - C_t
  ) %>%
  ungroup()

CEX_data <- CEX_data %>%
  mutate(
    I_t = TC - C_t   # I_t = total consumption - nondurables
  )


CEX_gmm <- CEX_data %>%
  filter(year != 1979) %>%                 # drop 1979
  filter(!is.na(tilde_C_tp1),
         !is.na(tilde_K_tp1)) %>%
  mutate(
    z1 = 1,       # constant
    z2 = C_t,     # C_t
    z3 = I_t      # I_t
  )

CEX_gmm <- CEX_gmm %>%
  filter(
    !is.na(tilde_K_tp1),
    tilde_K_tp1 != 0    # drop zero tilde_K_tp1
  )

gmm_moments <- function(par, data) {
  theta <- par[1]
  phi   <- par[2]
  psi <- par[3]
  
  with(data, {
    m <- 0.768 * tilde_C_tp1^(-2) +
      0.96  * K_tp1^(-psi)/C_t^(-2) * theta/ (P_D + phi * I_t) -
      1
    
    cbind(
      m * 1,
      m * total_income,
      m * P_D
      
    )
  })
}

CEX_gmm_sub <- CEX_gmm %>%
  sample_frac(size = 0.10)

par0 <- c(theta = 1, phi = 2, psi = 2)

gmm_fit_iterative <- gmm(
  g  = gmm_moments,
  x  = CEX_gmm_sub,
  t0 = par0,
  type = "iterative",
  vcov = "iid"
)
summary(gmm_fit_iterative)
