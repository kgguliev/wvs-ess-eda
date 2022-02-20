  library(data.table)
  library(tidyverse)
  library(haven)

# Read ---- 

ess_data <- read_sas("./data/ess9e03_1.sas7bdat")

wvs_data <- fread("./data/WVS_Cross-National_Wave_7_csv_v2_0.csv")

# ess_formats <- read_sas("./data/ESS9e03_1_formats.sas")
# 
# ess_miss <- read_sas("./data/ESS_miss.sas")
# 
# ess_ms <- read_sas("./data/ESS9e03_1_ms.sas")

str(ess_data)

unique(ess_data[["cntry"]])

labs <- labelled::var_label(ess_data) %>% 
  enframe(name = "code", value = "text")

# Hungary and Germany ----

ess_subset <- ess_data %>%
  filter(cntry %in% c("DE", "HU", "FR", "PL")) %>% 
  select(cntry, idno, lrscale) %>% 
  filter(!lrscale > 10)

ess_subset %>% 
  group_by(cntry) %>% 
  summarise(n())

table(ess_subset$lrscale, ess_subset$cntry)

ggplot(ess_subset, aes(lrscale)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~ cntry)

# WVS Data ------

wvs_subset <- wvs_data %>% 
  filter(B_COUNTRY_ALPHA %in% c("RUS", "DEU", "USA")) %>% 
  select(A_YEAR, B_COUNTRY_ALPHA, Q57:Q63, Q240) %>% 
  filter(Q240 >= 1, Q57 >= 1, Q58 >= 1, Q59 >= 1, Q60 >= 1, Q61 >= 1, Q62 >= 1, Q63 >= 1) %>% 
  mutate(general_trust = Q57,
         Q58 = 5 - Q58,
         Q59 = 5 - Q59,
         Q60 = 5 - Q60,
         Q61 = 5 - Q61,
         Q62 = 5 - Q62,
         Q63 = 5 - Q63) %>% 
  mutate(
    ingroup = rowMeans(.[, c("Q58", "Q59", "Q60")]),
    outgroup = rowMeans(.[, c("Q61", "Q62", "Q63")])
  )

psych::alpha(wvs_subset[, c("Q58", "Q59", "Q60")])$total$std.alpha
psych::alpha(wvs_subset[, c("Q61", "Q62", "Q63")])$total$std.alpha

table(wvs_subset$general_trust, wvs_subset$B_COUNTRY_ALPHA)

table(wvs_subset$B_COUNTRY_ALPHA, wvs_subset$Q240)

wvs_subset %>% 
  group_by(B_COUNTRY_ALPHA) %>% 
  summarise(n())

# EDA ----

ggplot(wvs_subset, aes(Q240)) +
  geom_histogram(binwidth = 1, aes(fill = B_COUNTRY_ALPHA)) +
  facet_wrap(~ B_COUNTRY_ALPHA)

ggplot(wvs_subset, aes(ingroup, fill = B_COUNTRY_ALPHA)) +
  geom_density(alpha = 0.5)

ggplot(wvs_subset, aes(outgroup, fill = B_COUNTRY_ALPHA)) +
  geom_density(alpha = 0.5)

ggplot(wvs_subset, aes(B_COUNTRY_ALPHA, ingroup, fill = B_COUNTRY_ALPHA)) +
  geom_boxplot()

ggplot(wvs_subset, aes(B_COUNTRY_ALPHA, outgroup, fill = B_COUNTRY_ALPHA)) +
  geom_boxplot()


# Test -----

sample_fit <- lm(Q240 ~ ingroup + outgroup + general_trust +
                   ingroup:B_COUNTRY_ALPHA + outgroup:B_COUNTRY_ALPHA +
                   general_trust:B_COUNTRY_ALPHA,
                 data = wvs_subset)

summary(sample_fit)

me <- margins::margins(sample_fit)

summary(me)
