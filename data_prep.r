library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")
library("dplyr")



data_final <- read_csv("./data/Query1.csv")

# Normalize population
data_final$original_total_pop <- data_final$total_pop

data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))

# Get the rent burden feature


# Break up into Severe burden, rent burden and no burden

data_final$renters = data_final$households - data_final$owner_occupied_housing_units

data_final$severe_burden = data_final$rent_over_50_percent / data_final$renters

data_final$rent_burden = (data_final$rent_40_to_50_percent +
                            data_final$rent_35_to_40_percent +
                            data_final$rent_30_to_35_percent) / data_final$renters
data_final$no_rent_burden = (data_final$rent_25_to_30_percent +
                               data_final$rent_20_to_25_percent +
                               data_final$rent_15_to_20_percent + 
                               data_final$rent_10_to_15_percent +
                               data_final$rent_10_to_15_percent +
                               data_final$rent_under_10_percent) / data_final$renters

data_final$test= data_final$severe_burden + 
  data_final$rent_burden + 
  data_final$no_rent_burden + 
  (data_final$rent_burden_not_computed/data_final$households)

# Check to make sure that there we are looking at all the households renting
data_final %>% select(test) %>% summary()

# Get cases/deaths per 1000
data_final$confirmed_cases_per1000 = data_final$confirmed_cases / 
  (data_final$original_total_pop)*1000
data_final$deaths_per1000 = data_final$deaths / 
  (data_final$original_total_pop)*1000

# Get the percentages of each population group
data_final$white_pop_per = data_final$white_pop / data_final$original_total_pop
data_final$black_pop_per = data_final$black_pop / data_final$original_total_pop
data_final$asian_pop_per = data_final$asian_pop / data_final$original_total_pop
data_final$hispanic_pop_per = data_final$hispanic_pop / data_final$original_total_pop
data_final$amerindian_pop_per = data_final$amerindian_pop / data_final$original_total_pop
data_final$other_race_pop_per = data_final$other_race_pop / data_final$original_total_pop
data_final$two_or_more_races_pop_per = data_final$two_or_more_races_pop / data_final$original_total_pop
data_final$not_hispanic_pop_per = data_final$not_hispanic_pop / data_final$original_total_pop





temp <- data_final %>% select(total_pop, original_total_pop, no_rent_burden, rent_burden,
                      severe_burden, median_income,
                      deaths_per1000, confirmed_cases_per1000,
                      white_pop_per, black_pop_per, asian_pop_per, hispanic_pop_per,
                      amerindian_pop_per, other_race_pop_per, two_or_more_races_pop_per,
                      not_hispanic_pop_per, commuters_by_public_transportation,
                      county_name, state, county_fips_code)
temp
temp %>% write.csv(file = "datatable.csv")

temp %>% summary()

# Get Standard Deviation
temp_numeric <- temp %>% select(total_pop, original_total_pop, no_rent_burden,
                                rent_burden, severe_burden, median_income,
                                deaths_per1000, confirmed_cases_per1000,
                                white_pop_per, black_pop_per, asian_pop_per,
                                hispanic_pop_per, amerindian_pop_per,
                                other_race_pop_per, two_or_more_races_pop_per,
                                not_hispanic_pop_per,
                                commuters_by_public_transportation)
apply(temp_numeric,2,sd)

# Investigate Outliers
# Invwargate rent Burden
p1 <- ggplot(temp, aes(x = severe_burden, y = rent_burden)) + geom_point()
p2 <- ggplot(temp, aes(x = severe_burden, y = no_rent_burden)) + geom_point()
p3 <- ggplot(temp, aes(x = rent_burden, y = no_rent_burden)) + geom_point()

g <- ggarrange(p1, p2, p3) +
  ggtitle("Title")
g
annotate_figure(g, top = text_grob("Rent Burden Plot Chart"))
ggsave("./charts/RentBurdenPlots.jpg", width = 6.5, height = 3)

# Investigate Population vs cases and deaths

