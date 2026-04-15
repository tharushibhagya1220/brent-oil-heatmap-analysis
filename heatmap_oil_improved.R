# ============================================================
# Oil Price Heatmap — IMPROVED VERSION (Bug Fixed)
# ============================================================

# ---- Step 1: Load Libraries --------------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

# ---- Step 2: Load Data -------------------------------------
setwd("C:/Users/Asus/Desktop/Fuel dashboard/Heat Map")
df <- read.csv("oil_jet_fuel_prices.csv", stringsAsFactors = FALSE)

# ---- Step 3: Clean & Prepare -------------------------------

# 3a. Convert month to proper Date type
df$month <- as.Date(paste0(df$month, "-01"))

# 3b. Extract year and month number
df$year <- year(df$month)

# KEY FIX: convert to character first, then factor — prevents ordered mismatch
df$month_num <- factor(
  as.character(month(df$month, label = TRUE, abbr = TRUE)),
  levels = c("Jan","Feb","Mar","Apr","May","Jun",
             "Jul","Aug","Sep","Oct","Nov","Dec")
)

# 3c. Order conflict_phase chronologically
phase_order <- c(
  "Pre-Pandemic Baseline",
  "COVID-19 Collapse",
  "Recovery & Surge",
  "Ukraine War Shock",
  "Stabilisation",
  "Gaza-Israel Conflict",
  "Pre-Iran Escalation",
  "US-Iran War Conflict"
)
df$conflict_phase <- factor(df$conflict_phase, levels = phase_order)

# ---- FIX 1: Unique year labels (resolve year repetition) ---
duplicated_years <- df %>%
  group_by(year) %>%
  summarise(n_phases = n_distinct(conflict_phase)) %>%
  filter(n_phases > 1) %>%
  pull(year)

df <- df %>%
  mutate(
    year_label = if_else(
      year %in% duplicated_years,
      paste0(year, "\n(", as.character(conflict_phase), ")"),
      as.character(year)
    ),
    sort_key = as.numeric(conflict_phase) * 10000 + year
  )

# ---- FIX 2: Complete grid — fill missing months with NA ----
existing_combos <- df %>%
  distinct(conflict_phase, year, year_label, sort_key)

all_months <- c("Jan","Feb","Mar","Apr","May","Jun",
                "Jul","Aug","Sep","Oct","Nov","Dec")

# Build full grid using unnest (avoids crossing() factor clash)
full_grid <- existing_combos %>%
  mutate(month_num = list(all_months)) %>%
  tidyr::unnest(month_num) %>%
  mutate(month_num = factor(month_num, levels = all_months))

# Join — both month_num are now identical plain factors
df_join <- df %>%
  select(conflict_phase, year, month_num,
         brent_crude_usd_barrel, year_label, sort_key)

df_full <- full_grid %>%
  left_join(df_join,
            by = c("conflict_phase","year","month_num",
                   "year_label","sort_key"))

# Restore factor levels
df_full$conflict_phase <- factor(df_full$conflict_phase, levels = phase_order)

df_full$year_label <- factor(
  df_full$year_label,
  levels = df_full %>%
    arrange(sort_key) %>%
    distinct(year_label) %>%
    pull(year_label)
)

# ---- Step 5: Build Heatmap ---------------------------------
p <- ggplot(df_full,
            aes(x = month_num,
                y = reorder(year_label, sort_key),
                fill = brent_crude_usd_barrel)) +

  geom_tile(color = "white", linewidth = 0.5) +

  scale_fill_gradient2(
    low      = "#FFF176",
    mid      = "#FF6F00",
    high     = "#B71C1C",
    midpoint = 80,
    na.value = "#EEEEEE",
    name     = "Brent Crude\n(USD/barrel)"
  ) +

  geom_text(
    data = df_full %>% filter(!is.na(brent_crude_usd_barrel)),
    aes(label = round(brent_crude_usd_barrel, 0)),
    size = 2.5, color = "white", fontface = "bold"
  ) +

  labs(
    title    = "Global Events vs Brent Oil Prices (2019\u20132026): A Heatmap Analysis",
    subtitle = "Monthly Brent crude prices (USD/barrel) mapped across 8 geopolitical conflict phases\nGrey cells = data not available for that month in this phase",
    x        = "Month",
    y        = "Year",
    caption  = "Source: EIA / Platts / IATA"
  ) +

  facet_grid(conflict_phase ~ ., scales = "free_y", space = "free_y") +

  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 15, hjust = 0,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 10, color = "grey40", hjust = 0,
                                    margin = margin(b = 10)),
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 10),
    axis.text.y      = element_text(face = "bold", size = 9),
    axis.title       = element_text(size = 11),
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 9),
    panel.grid       = element_blank(),
    plot.caption     = element_text(color = "grey40", size = 10,
                                    face = "italic", margin = margin(t = 8)),
    strip.text.y     = element_text(angle = 0, hjust = 0.5,
                                    face = "bold", size = 8.5,
                                    color = "white",
                                    margin = margin(l = 4, r = 4)),
    strip.background = element_rect(fill = "#1A237E", color = NA),
    panel.spacing    = unit(0.35, "lines"),
    plot.margin      = margin(t = 12, r = 10, b = 12, l = 10)
  )

# ---- Step 6: Save ------------------------------------------
ggsave("oil_price_heatmap_improved.png",
       plot   = p,
       width  = 14,
       height = 11,
       dpi    = 300,
       bg     = "white")

cat("Done! Saved as oil_price_heatmap_improved.png\n")
