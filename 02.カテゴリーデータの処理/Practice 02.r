### Practice 02 answer ###

# 1

library(tidyverse)
library(readxl)

food <- readxl::read_excel("./raw_data_practice_02.xlsx")
food <- food %>% dplyr::rename(Country = Group, Tonkatsu = Food_1_Tonkatsu,
                               Sushi = Food_2_Sushi, Takoyaki = Food_3_Takoyaki, Ramen = Food_4_Ramen)
food <- as.data.frame(food)

# 2

food$Country <- gsub('_1', '', food$Country)
food$Country <- gsub('_2', '', food$Country)

food

food_sum <- aggregate(. ~ Country, food, sum, na.rm = F)

Country_order_food <- factor(food_sum$Country, levels = c('Korea', 'China', 'Thai', 'UK', 'France', 'US'))

food_sum <- food_sum %>% arrange(Country_order_food)
food_sum

## 3

rownames(food_sum) <- food_sum$Country
food_sum <- food_sum %>% select(2,3,4,5)
food_sum

food_sum_m <- as.matrix(food_sum)
food_sum_p <- prop.table(food_sum_m, 1)
food_sum_pp <- food_sum_p*100
food_sum_pp

## 4

mar.default <- c(5, 4, 4, 8) + 0.1
par(mar = mar.default + c(0, 5, 1, 1))
barplot((t(food_sum_pp)), horiz=TRUE, las=1, beside=F, xlim=c(0, 100), 
        legend.text = T, args.legend = list(x = "topright", inset = c(-0.6, 0.025)),
        col=c("red", "orange", "yellow", "green"))

## 4

library(RVAideMemoire)

food_sum_stat <- as.matrix(food_sum)

# Fisher & post-hoc

fisher.test(food_sum_stat, simulate.p.value = T)
fisher_posthoc_result_food <- fisher.multcomp(food_sum_stat, p.method = 'fdr')
fisher_posthoc_result_food

write.csv(fisher_posthoc_result_food$p.value, "./Results_food_fisher.csv")

# Chi-square & post-hoc

food_sum_stat

chisq.test(food_sum_stat)
chisq_posthoc_result_food <- chisq.multcomp(food_sum_stat, p.method = 'fdr')
chisq_posthoc_result_food

write.csv(chisq_posthoc_result_food$p.value, "./Results_food_chisq.csv")