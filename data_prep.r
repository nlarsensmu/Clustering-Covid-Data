library("tidyverse")
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("gridExtra")
library("knitr")
library("ggpubr")



data_final <- read_csv("./data/Query1.csv")

# Normalize population
data_final$original_total_pop <- data_final$total_pop

data_final <- data_final %>% mutate_at(c("total_pop"), ~(scale(.) %>% as.vector))

# Get the rent burden feature


# Break up into Severe burden, rent burden and no burden

data_final$severe_burden = data_final$rent_over_50_percent / data_final$households

data_final$rent_burden = (data_final$rent_40_to_50_percent +
                            data_final$rent_35_to_40_percent +
                            data_final$rent_30_to_35_percent) / data_final$households
data_final$no_rent_burden = (data_final$rent_25_to_30_percent +
                               data_final$rent_20_to_25_percent +
                               data_final$rent_15_to_20_percent + 
                               data_final$rent_10_to_15_percent +
                               data_final$rent_10_to_15_percent +
                               data_final$rent_under_10_percent) / data_final$households

data_final$test= data_final$severe_burden + data_final$rent_burden + data_final$no_rent_burden + (data_final$rent_burden_not_computed/data_final$households)

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


hist(x=x,breaks=10)




col_names <- colnames(temp)
x <- as.numeric(unlist(temp[col_names[1]]))
p <- ggplot(data_final, mapping = aes(x=x)) + 
  geom_histogram(bins = 20) + ggtitle(col_names[1])

plots <- vector('list', ncol(temp))
for (i in 1:length(col_names)){
  x <- as.numeric(unlist(temp[col_names[i]]))
  filename <- sprintf(".\\charts\\%s.png", col_names[i])
  print(filename)
  ggplot(data_final, mapping = aes(x=x)) + 
    xlab(col_names[i]) +
    geom_histogram(bins = 20) + ggtitle(col_names[i])
  
  ggsave(filename = filename, width = 3, height = 3)
}
plots
plots[3]

temp %>% select(no_rent_burden, rent_burden, severe_burden)

summary(temp)
