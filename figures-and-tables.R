# libraries and data ---------------------------------------------------------------------

library(tidyverse)
library(ggrepel)

# fonts
library(showtext) 
showtext_auto() 
sysfonts::font_add("LM", "fonts/CMU_Serif_Roman.ttf")

options(scipen = 999)
options(dplyr.summarise.inform = FALSE)

theme_set(ggthemes::theme_pander(base_size = 18, base_family = "serif") + 
            theme(plot.caption = element_text(size = 16, hjust = 0),
                  axis.title   = element_text(hjust = 0.5)))

# data
fraser <- readRDS("data/fraser.rds") %>%            # full Fraser 1970-2015
  mutate(size_of_government = 10 - size_of_government,
         regulation         = 10 - regulation)

institutions <- readRDS("data/institutions.rds")    # selection of QoG and VoD
qog <- readRDS("data/qog.rds")                      # Quality of Govt data

# smaller dataset -------------------------------------------------------------------------

fraser_work <- fraser %>% 
  select(-c(siso, economic_freedom_summary_index, rank, quartile, 
            size_of_government, legal_system__property_rights, sound_money, 
            freedom_to_trade_internationally, regulation, 
            cowcode)) %>% 
  filter(year %% 5 == 0)

## rearrange the fraser
fraser_small <- fraser_work %>% 
  pivot_longer(cols = -c(country, year), names_to = "indic") %>% 
  pivot_wider(names_from = country, values_from = value) 

countries <- fraser_small %>% select(-year, -indic) %>% names()

fraser_small_summary <- tibble(
  countries,
  missing = map_dbl(fraser_small %>% select(-year, -indic), ~sum(is.na(.x))),
  obs     = map_dbl(fraser_small %>% select(-year, -indic), ~sum(!is.na(.x))),
  perc    = missing/(missing + obs)
)

## keep only the countries with enough data
countries <- fraser_small_summary %>% filter(perc < 0.35) %>% pull(countries)

fraser_small <- fraser_small %>% select(year, indic, all_of(countries))

# summary stats ---------------------------------------------------------------------

tbl_fraser_summary_stats <- fraser_work %>%
  filter(country %in% countries) %>% 
  select(-year, -country) %>% 
  sjmisc::descr() %>% 
  select(Indicator = var, `Percent Missing` = NA.prc, Mean = mean, Std.Dev. = sd, Range = range, Skew = skew) %>% 
  mutate_if(is.numeric, ~round(.x, 2)) %>% 
  mutate(Indicator = str_replace_all(Indicator, "_", " "))

# cluster tree ----------------------------------------------------------------------

fraser_cluster <- fraser_small %>% 
  select(-year, -indic) %>% 
  t() %>% 
  dist() %>% 
  hclust(method = "ward.D")

fig_tree <- fraser_cluster %>% 
  ggdendro::ggdendrogram(rotate = TRUE, size = 1) +
  annotate(geom = "text", x = 35, y = 150, label = "Liberal", family = "LM") +
  annotate(geom = "text", x = 23.5, y = 140, label = "Coordinated", family = "LM") +
  annotate(geom = "text", x = 7.5, y = 165, label = "Developing", family = "LM") 


# frazer cluster stats

fraser_clustered <- fraser %>% 
  filter(year %% 5 == 0) %>% 
  filter(country %in% countries) %>% 
  group_by(year) %>% 
  mutate(cluster = cutree(fraser_cluster, k = 3)) %>% 
  ungroup() %>% 
  mutate(cluster = case_when(
    cluster == 1 ~ "Developing",
    cluster == 2 ~ "Liberal",
    cluster == 3 ~ "Coord."
  ))

# try different years for robustness check
fig_fraser_stats <- fraser_clustered %>% 
  select(cluster, # year,
         `Economic freedom`   = economic_freedom_summary_index, 
         `Size of government` = size_of_government, 
         `Property rights`    = legal_system__property_rights, 
         `Sound money`        = sound_money, 
         `Freedom to trade`   = freedom_to_trade_internationally, 
         Regulation           = regulation) %>% 
  #filter(year == 2010) %>% 
  pivot_longer(cols = -c(cluster), names_to = "indic") %>% 
  mutate(cluster = factor(cluster, levels = c("Liberal", "Coord.", "Developing"))) %>% 
  #select(-year) %>% 
  ggplot(aes(x = cluster, y = value, group = cluster)) +
    geom_violin() +
    stat_summary(fun = "mean", geom = "point") +
    facet_wrap(~indic) +
    labs(x = "", y = "")


# fraser table stats -----------------------------------------------------------------

variables <- c(`Economic freedom`     = "economic_freedom_summary_index", 
               `Size of government`   = "size_of_government", 
               `Property rights`      = "legal_system__property_rights", 
               `Sound money`          = "sound_money", 
               `Freedom to trade`     = "freedom_to_trade_internationally", 
               `Regulation`           = "regulation")

tbl_fraser_stats <- 
  map_df(variables, ~
         fraser_clustered %>% 
         rstatix::pairwise_t_test(
           reformulate(termlabels = 'cluster', response = .x), 
           pool.sd = FALSE,
           detailed = TRUE
         ),
       .id = "Variable") %>% 
  mutate(p.value   = ifelse(p < 0.001, "<.001", as.character(round(p, 3))),
         estimate  = abs(round(estimate, 2)),
         estimate1 = round(estimate1, 2),
         estimate2 = round(estimate2, 2)) %>% 
  select(Variable, 
         `Group 1` = group1, `Group 2` = group2, 
         `Mean 1` = estimate1, `Mean 2` = estimate2,
         Difference = estimate, 
         `P-value` = p.value) 

# outcomes figure stats -------------------------------------------------------------

fraser_outcomes <- fraser_clustered %>% 
  rename(iso = siso) %>% 
  left_join(institutions %>% 
              select(iso, year,
                     liberal_democracy, corruption, 
                     state_capacity, fiscal_capacity, govt_public_interest,
                     income, life_expectancy, human_capital,
                     income_1800),
            by = c("iso", "year")) %>% 
  left_join(qog %>% 
              select(iso, year, wdi_gini, wdi_gdpgr, wdi_unempilo),
            by = c("iso", "year"))


fig_outcomes_stats <- fraser_outcomes %>% 
  filter(year >= 2000) %>% 
  select(cluster, 
         `Liberal democracy [a]`                     = liberal_democracy, 
         `Political corruption [a]`                  = corruption, 
         `State capacity [e]`                        = state_capacity,
         `Real GDP per capita,\n2011 US dollars [c]` = income, 
         `Life expectancy [d]`                       = life_expectancy, 
         `Income in 1800 [c]`                        = income_1800,
         `Gini [b]`                                  = wdi_gini,
         `GDP per capita\ngrowth [b]`                = wdi_gdpgr,
         `Unemployment [b]`                          = wdi_unempilo) %>% 
  pivot_longer(cols = -c(cluster), names_to = "indic") %>% 
  mutate(cluster = factor(cluster, levels = c("Liberal", "Coord.", "Developing"))) %>% 
  ggplot(aes(x = cluster, y = value, group = cluster)) +
    geom_violin() +
    stat_summary(fun = "mean", geom = "point") +
    facet_wrap(~indic, scales = "free") +
    labs(x = "", y = "",
         caption = "Sources of data:\na. Varieties of democracy\nb. World Bank\nc. Maddison Project\nd. Institute for Health Metrics and Evaluation\ne. Murphy and O'Reilley 2020")

# outcomes stats table --------------------------------------------------------------

variables <- c(
  `Real GDP per capita, 2011$ [c]` = "income",
  `GDP per capita growth [b]`      = "wdi_gdpgr", 
  `Income in 1800 [c]`             = "income_1800",
  `Income inequality (Gini) [b]`   = "wdi_gini",
  `Unemployment [b]`               = "wdi_unempilo", 
  `Political corruption [a]`       = "corruption", 
  `Liberal democracy [a]`          = "liberal_democracy",
  `State capacity [e]`             = "state_capacity",
  `Life expectancy [d]`            = "life_expectancy")

tbl_outcomes_stats <- 
  map_df(variables, ~
         fraser_outcomes %>% 
         rstatix::pairwise_t_test(
           reformulate(termlabels = 'cluster', response = .x), 
           pool.sd = FALSE,
           detailed = TRUE
         ),
       .id = "Variable") %>% 
  mutate(p.value   = ifelse(p < 0.001, "<.001", as.character(round(p, 3))),
         estimate  = abs(round(estimate, 2)),
         estimate1 = round(estimate1, 2),
         estimate2 = round(estimate2, 2)) %>% 
  select(Variable, 
         `Group 1` = group1, `Group 2` = group2, 
         `Mean 1` = estimate1, `Mean 2` = estimate2,
         Difference = estimate, 
         `P-value` = p.value) 

# fraser evolution simple -----------------------------------------------------------

fig_evolution_simple <- fraser_outcomes %>% 
  group_by(year, cluster) %>% 
  summarise(size_of_government = mean(size_of_government, na.rm = TRUE),
            regulation         = mean(regulation,         na.rm = TRUE)
  ) %>% 
  ggplot(aes(x = size_of_government, y = regulation, label = year, linetype = cluster)) +
    geom_text_repel(alpha = 0.5) +
    geom_path() +
    # facet_wrap(~cluster) +
    # xlim(0,10) +
    # ylim(0,10) +
    labs(x = "Size of government", y = "Regulation")

# fraser evolution complex -----------------------------------------------------------

fig_evolution_complex <- fraser_outcomes %>% 
  group_by(year, cluster) %>%
  summarise(size_of_government = mean(size_of_government, na.rm = TRUE),
            regulation         = mean(regulation,         na.rm = TRUE)
  ) %>%
  mutate(year_label = ifelse(year == 1970 | year == 2015, year, NA)) %>%
  ggplot(aes(x = size_of_government, y = regulation, color = cluster)) +
    geom_path(data = fraser_outcomes,
              aes(x = size_of_government, y = regulation, group = country, color = cluster),
              alpha = 0.3) +
    geom_path(size = 1) +
    geom_text_repel(aes(label = year_label)) +
    #facet_wrap(~cluster) +
    scale_shape_manual(values = c(16, 17, 18)) +
    labs(x = "Size of government", y = "Regulation") +
    annotate(geom = "text", x = 7.2, y = 2.5, label = "Coordinated", color = "tomato", size = 5) +
    annotate(geom = "text", x = 2, y = 1, label = "Liberal", color = "steel blue", size = 5) +
    annotate(geom = "text", x = 2.5, y = 5, label = "Developing", color = "forest green", size = 5) +
    theme(legend.position = "none")

# evolution of selected countries -----------------------------------------------------------

fig_evolution_countries <- fraser_clustered %>% 
  filter(country %in% c("United States", "United Kingdom", "Netherlands", "Italy",
                        "Denmark", "Sweden", "Germany", "France", "Japan",
                        "New Zealand", "Australia", "Switzerland",
                        "Greece", "Spain", "Singapore")) %>% 
  mutate(year_label = ifelse(year == 1970 | year == 2015, year, NA),
         cluster    = ifelse(cluster == "Coord.", "Coordinated", cluster)) %>%
  ggplot(aes(x = size_of_government, y = regulation, label = year_label, linetype = cluster)) +
    geom_path() +
    geom_text_repel(alpha = 0.7) +
    facet_wrap(~country, ncol = 3) +
    labs(x = "Size of government", y = "Regulation", linetype = "") +
    theme(legend.position = "bottom")

# PSA -----------------------------------------------------------------------

fig_psa1 <- fraser_clustered %>%
  mutate(
         govt_spending    = 10 - government_consumption, 
         transfers        = 10 - transfers_and_subsidies, 
         govt_enterprises = 10 - government_enterprises_and_inves,
         cluster    = ifelse(cluster == "Coord.", "Coordinated", cluster)
         ) %>% 
  select(country, year, cluster,
         govt_spending    , 
         transfers        , 
         govt_enterprises ) %>% 
  group_by(year, cluster) %>% 
   # summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
    summarise(
      year = year,
      country = country,
      cluster = cluster,
      govt_spending    = mean(govt_spending, na.rm = TRUE),
      transfers        = mean(transfers, na.rm = TRUE),
      govt_enterprises = mean(govt_enterprises, na.rm = TRUE)) %>%
    ungroup() %>% 
  select(country, year, cluster,
         `Govt. consum. spending`      = govt_spending, 
         `Transfers and subsidies`     = transfers, 
         `Govt. enterprises`           = govt_enterprises) %>%
  pivot_longer(cols = -c(country, year, cluster), names_to = "indic") %>% 
  ggplot(aes(x = year, y = value)) +
    geom_line() +
    facet_wrap( ~ indic + cluster, ncol = 3) +
    labs(x = "", y = "")

fig_psa2 <- fraser_clustered %>%
  mutate(
    credit_regs      = 10 - credit_market_regulations,
    labor_regs       = 10 - labor_market_regulations,
    business_regs    = 10 - business_regulations,
    cluster    = ifelse(cluster == "Coord.", "Coordinated", cluster)
  ) %>% 
  select(country, year, cluster,
         credit_regs      ,
         labor_regs       ,
         business_regs ) %>% 
  group_by(year, cluster) %>% 
  # summarise_if(is.numeric, mean, na.rm = TRUE) %>% 
  summarise(
    year = year,
    country = country,
    cluster = cluster,
    credit_regs      = mean(credit_regs, na.rm = TRUE),
    labor_regs       = mean(labor_regs, na.rm = TRUE),
    business_regs    = mean(business_regs, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(country, year, cluster,
         `Credit regs`                 = credit_regs,
         `Labor regs`                  = labor_regs,
         `Business regs`               = business_regs ) %>%
  pivot_longer(cols = -c(country, year, cluster), names_to = "indic") %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line() +
  facet_wrap( ~ indic + cluster, ncol = 3) +
  labs(x = "", y = "")

# save objects -----------------------------------------------------------------------

save(
  tbl_fraser_summary_stats,
  fig_tree,
  fig_fraser_stats,
  tbl_fraser_stats,
  fig_outcomes_stats,
  tbl_outcomes_stats,
  fig_evolution_simple,
  fig_evolution_complex,
  fig_evolution_countries,
  fig_psa1,
  fig_psa2,
  
  file = "objects.RData"
)
