library(digitize)
library(tidyverse)
setwd('C:/Users/mcs94276/Desktop/Digitize')

stage1 = digitize(image_filename = '0222_Test.jpg')
stage1$stage <- 1
write.csv(stage1,'stage1.csv')

stage2 = digitize(image_filename = '0222_Test.jpg')
write.csv(stage2,'stage2.csv')
stage2$stage <- 2

stage3 = digitize(image_filename = '0222_Test.jpg')
write.csv(stage3,'stage3.csv')
stage3$stage <- 3

stage4 = digitize(image_filename = '0222_Test.jpg')
write.csv(stage4,'stage4.csv')
stage4$stage <- 4

stage_1 <- stage1 %>%
  mutate(epsilon_a_s = x, sigma_q =  y * 10000 / 145.038, sigma_c = 0)

stage_2 <- stage2 %>%
  mutate(epsilon_a_s = x, sigma_q =  y * 10000 / 145.038, sigma_c = 870 / 145.038)

stage_3 <- stage3 %>%
  mutate(epsilon_a_s = x, sigma_q =  y * 10000 / 145.038, sigma_c = 2321 / 145.038)

stage_4 <- stage4 %>%
  mutate(epsilon_a_s = x, sigma_q =  y * 10000 / 145.038, sigma_c = 3771 / 145.038)

all_stages <- rbind(stage_1,stage_2,stage_3,stage_4)

write.csv(all_stages,'all_stages.csv')

ggplot(all_stages) +
  geom_path(aes(x = epsilon_a_s, y = sigma_q, colour = as.factor(round(sigma_c,0)))) +
  geom_point(aes(x = epsilon_a_s, y = sigma_q, colour = as.factor(round(sigma_c,0)))) +
  scale_colour_ordinal(option = 'C', name = 'Confining Pressure (MPa)') +
  theme_minimal()
