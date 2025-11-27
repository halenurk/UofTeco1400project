# ==============================================================================
# FINAL SCRIPT: QR with Confidence Intervals (Random Chunking Method)
# ==============================================================================

# --- 1. LIBRARIES ---
if (!require("quantreg")) install.packages("quantreg")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")
if (!require("stringr")) install.packages("stringr")

library(quantreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

# --- 2. LOAD DATA ---
# Download the v2 dataset (with total_consumption)
url <- "https://github.com/halenurk/UofTeco1400project/raw/refs/heads/main/CEX_Cleaned.rds"
download.file(url, "project_data_v2.rds", mode = "wb")
CEX_data <- readRDS("project_data_v2.rds")

# --- 3. CONFIGURATION ---
# Expanded Tau Sequence (0.01 to 0.99)
tau_seq <- seq(0.01, 0.99, by = 0.02) 

# RANDOMIZE DATA (Crucial for CI calculation)
set.seed(123) # For reproducibility
CEX_data <- CEX_data[sample(nrow(CEX_data)), ]

# Chunking
target_chunk_size <- 50000
n_total <- nrow(CEX_data)
n_chunks <- ceiling(n_total / target_chunk_size)
chunk_ids <- cut(seq(1, n_total), breaks = n_chunks, labels = FALSE)

message(paste("Data split into", n_chunks, "randomized chunks for CI estimation."))

# Formulas
form_nondur <- consumption_nondurables ~ total_income + assets_0 + hh_age + family_size + marital + unemp + region + year
form_total  <- total_consumption ~ total_income + assets_0 + hh_age + family_size + marital + unemp + region + year

# --- 4. CORE FUNCTION (With Standard Error Calculation) ---
run_qr_with_ci <- function(formula_obj, label) {
  message(paste("\n--- Starting QR Run for:", label, "---"))
  results_list <- list()
  
  # Progress bar
  pb <- txtProgressBar(min = 0, max = length(tau_seq), style = 3)
  
  for (t_idx in seq_along(tau_seq)) {
    tau_val <- tau_seq[t_idx]
    chunk_coeffs <- list()
    
    # Run model on each chunk
    for (i in 1:n_chunks) {
      chunk <- CEX_data[chunk_ids == i, ]
      tryCatch({
        suppressWarnings({
          model <- rq(formula_obj, data = chunk, tau = tau_val, method = "br")
        })
        chunk_coeffs[[i]] <- coef(model)
      }, error = function(e) NULL)
    }
    
    # --- AGGREGATION & CI CALCULATION ---
    valid <- which(!sapply(chunk_coeffs, is.null))
    
    if (length(valid) > 1) { # Need at least 2 chunks to calculate SD
      valid_coeffs <- chunk_coeffs[valid]
      
      # Align columns (handle missing variables in some chunks)
      all_names <- unique(unlist(lapply(valid_coeffs, names)))
      mat <- matrix(NA, nrow = length(valid_coeffs), ncol = length(all_names))
      colnames(mat) <- all_names
      
      for (k in seq_along(valid_coeffs)) {
        mat[k, names(valid_coeffs[[k]])] <- valid_coeffs[[k]]
      }
      
      # Calculate Stats across chunks
      # Mean Coefficient
      est_mean <- apply(mat, 2, mean, na.rm = TRUE)
      # Standard Error of the Mean (SD / sqrt(N))
      est_sd   <- apply(mat, 2, sd, na.rm = TRUE)
      est_se   <- est_sd / sqrt(length(valid))
      
      # 95% Confidence Intervals
      ci_lower <- est_mean - (1.96 * est_se)
      ci_upper <- est_mean + (1.96 * est_se)
      
      # Store as a dataframe for this tau
      results_list[[paste0("tau_", tau_val)]] <- data.frame(
        Variable = names(est_mean),
        Coefficient = est_mean,
        CI_Lower = ci_lower,
        CI_Upper = ci_upper,
        Tau = tau_val
      )
    }
    setTxtProgressBar(pb, t_idx)
  }
  close(pb)
  
  # Combine all taus into one big dataframe
  final_df <- do.call(rbind, results_list)
  return(final_df)
}

# --- 5. EXECUTE MODELS ---

message("Running Model 1: Nondurables...")
df_nondur <- run_qr_with_ci(form_nondur, "Nondurables")
saveRDS(df_nondur, "results_nondurables_ci.rds")

message("\nRunning Model 2: Total Consumption...")
df_total <- run_qr_with_ci(form_total, "Total Consumption")
saveRDS(df_total, "results_total_ci.rds")


# --- 6. PLOTTING FUNCTION (With Ribbons) ---
create_ci_plot <- function(data, var_list, title_main, file_prefix, color_hex) {
  
  filtered <- data %>% filter(Variable %in% var_list)
  
  p <- ggplot(filtered, aes(x = Tau, y = Coefficient, group = Variable)) +
    # Reference Line
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    # Confidence Interval Ribbon
    geom_ribbon(aes(ymin = CI_Lower, ymax = CI_Upper), fill = color_hex, alpha = 0.2) +
    # Main Line & Points
    geom_line(color = color_hex, size = 1) + 
    geom_point(color = color_hex, size = 0.5) +
    # Facet
    facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
    # Styling
    labs(
      title = title_main,
      subtitle = "Shaded area represents 95% Confidence Interval",
      y = "Coefficient Estimate", x = "Quantile (Tau)"
    ) +
    theme_minimal() +
    theme(
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = unit(1.5, "lines")
    )
  
  # Save
  ggsave(paste0(file_prefix, ".png"), p, width = 12, height = 8, bg = "white")
  print(p) # Show preview
}

# --- 7. GENERATE 8 PLOTS (4 per Model) ---
all_vars <- unique(df_nondur$Variable)

# Define Groups
vars_econ <- c("(Intercept)", "total_income", "assets_0", "hh_age", "family_size", "marital2", "unemp")
vars_region <- str_subset(all_vars, "^region")
vars_years <- str_subset(all_vars, "^year")
vars_years_early <- vars_years[vars_years <= "year1999"]
vars_years_late  <- vars_years[vars_years >= "year2000"]

message("\nGenerating Nondurable Plots (Blue)...")
create_ci_plot(df_nondur, vars_econ, "Nondurables: Econ Factors", "nondur_1_econ", "#2E86C1")
create_ci_plot(df_nondur, vars_region, "Nondurables: Regions", "nondur_2_regions", "#2E86C1")
create_ci_plot(df_nondur, vars_years_early, "Nondurables: Early Years", "nondur_3_years_early", "#2E86C1")
create_ci_plot(df_nondur, vars_years_late, "Nondurables: Late Years", "nondur_4_years_late", "#2E86C1")

message("\nGenerating Total Consumption Plots (Red)...")
create_ci_plot(df_total, vars_econ, "Total Consumption: Econ Factors", "total_1_econ", "#C0392B")
create_ci_plot(df_total, vars_region, "Total Consumption: Regions", "total_2_regions", "#C0392B")
create_ci_plot(df_total, vars_years_early, "Total Consumption: Early Years", "total_3_years_early", "#C0392B")
create_ci_plot(df_total, vars_years_late, "Total Consumption: Late Years", "total_4_years_late", "#C0392B")
message("\nDONE! 8 Plots saved with Confidence Intervals.")