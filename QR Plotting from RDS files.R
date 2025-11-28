# ==============================================================================
# COMBINED PLOTTING SCRIPT: Assets Comparison + Synchronized Appendices
# ==============================================================================

# --- 1. SETUP ---
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)

# Load the results files (Make sure these exist in your Files tab!)
df_nondur <- readRDS("results_nondurables_ci.rds")
df_total  <- readRDS("results_total_ci.rds")

# Tag them for identification
df_nondur$Type <- "Nondurables"
df_total$Type  <- "Total Consumption"

# Define Variable Groups
var_focus    <- "assets_0"
vars_control <- c("(Intercept)", "total_income", "hh_age", "family_size", "marital2", "unemp")


# ==============================================================================
# PART 1: ASSETS COMPARISON (Side-by-Side, Fixed Scales)
# ==============================================================================
message("Generating Plot 1: Assets Comparison (Fixed Scales)...")

# Combine data for just assets_0
assets_data <- bind_rows(
  df_nondur %>% filter(Variable == var_focus),
  df_total  %>% filter(Variable == var_focus)
)

p1 <- ggplot(assets_data, aes(x = Tau, y = Coefficient, group = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper, fill = Type), alpha = 0.2) +
  geom_line(aes(color = Type), size = 1.2) +
  geom_point(aes(color = Type), size = 0.8) +
  
  # NO 'scales="free_y"' here. This forces both panels to share the exact same Y-axis.
  facet_wrap(~ Type) + 
  
  scale_color_manual(values = c("Nondurables"="#2E86C1", "Total Consumption"="#C0392B")) +
  scale_fill_manual(values = c("Nondurables"="#2E86C1", "Total Consumption"="#C0392B")) +
  labs(
    title = "Effect of Initial Assets (assets_0) across the Distribution",
    subtitle = "Direct Comparison (Same Scale): Nondurables vs Total Consumption",
    y = "Coefficient Estimate", x = "Quantile (Tau)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14),
    plot.title = element_text(face = "bold", size = 16)
  )

ggsave("assets_0_comparison_synced.png", p1, width = 12, height = 6, bg = "white")


# ==============================================================================
# PART 2: SYNCHRONIZED APPENDIX PLOTS (The Skeleton Method)
# ==============================================================================
message("Calculating Global Scales for Appendices...")

# Filter down to the control variables
d_blue <- df_nondur %>% filter(Variable %in% vars_control)
d_red  <- df_total  %>% filter(Variable %in% vars_control)

# 1. Calculate the absolute Min and Max across BOTH datasets for each variable
global_ranges <- bind_rows(d_blue, d_red) %>%
  group_by(Variable) %>%
  summarise(
    min_val = min(CI_Lower, na.rm = TRUE),
    max_val = max(CI_Upper, na.rm = TRUE)
  )

# 2. Create the "Invisible Skeleton"
# This puts points at the very top and bottom limits for every variable
scale_skeleton <- bind_rows(
  global_ranges %>% select(Variable, y = min_val),
  global_ranges %>% select(Variable, y = max_val)
) %>%
  mutate(Tau = 0.5) # Dummy X value (invisible)


# --- GENERATE APPENDIX A (Nondurables / Blue) ---
message("Generating Plot 2: Appendix A (Nondurables)...")

p_blue <- ggplot(d_blue, aes(x = Tau, y = Coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  # THE INVISIBLE SKELETON (Forces the Scale to match the Red plot)
  geom_blank(data = scale_skeleton, aes(x = Tau, y = y)) +
  
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "#2E86C1", alpha = 0.2) +
  geom_line(color = "#2E86C1", size = 1) + 
  geom_point(color = "#2E86C1", size = 0.5) +
  
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  
  labs(
    title = "Appendix A: Economic Controls (Nondurables)",
    subtitle = "Scales are synchronized with Appendix B for direct comparison",
    y = "Coefficient Estimate", x = "Quantile (Tau)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("appendix_A_nondurables_synced.png", p_blue, width = 12, height = 8, bg = "white")


# --- GENERATE APPENDIX B (Total Consumption / Red) ---
message("Generating Plot 3: Appendix B (Total Consumption)...")

p_red <- ggplot(d_red, aes(x = Tau, y = Coefficient)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  
  # THE INVISIBLE SKELETON (Forces the Scale to match the Blue plot)
  geom_blank(data = scale_skeleton, aes(x = Tau, y = y)) +
  
  geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = "#C0392B", alpha = 0.2) +
  geom_line(color = "#C0392B", size = 1) + 
  geom_point(color = "#C0392B", size = 0.5) +
  
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  
  labs(
    title = "Appendix B: Economic Controls (Total Consumption)",
    subtitle = "Scales are synchronized with Appendix A for direct comparison",
    y = "Coefficient Estimate", x = "Quantile (Tau)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave("appendix_B_total_synced.png", p_red, width = 12, height = 8, bg = "white")

message("DONE! All 3 comparison plots saved.")