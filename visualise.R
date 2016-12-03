library(ggplot2)
library(scales)
library(choroplethr)
library(dplyr)




# What is the distribution of shifts to Trump across counties?
ggplot(data=votes, aes(x=trump_shift, weights=total_votes)) +
  geom_histogram(color="black", fill="white", binwidth=0.01) +
  geom_vline(xintercept = 0, colour="red") +
  scale_x_continuous(label=percent, name="Shift in 2-candidate vote to GOP") +
  scale_y_continuous(label=comma, name="Number of votes in county") +
  theme_minimal()

ggplot(data=votes, aes(x=trump_shift)) +
  geom_histogram(color="black", fill="white", binwidth=0.01) +
  geom_vline(xintercept = 0, colour="red") +
  scale_x_continuous(label=percent, name="Shift in 2-candidate vote to GOP") +
  scale_y_continuous(label=comma, name="Number of counties") +
  theme_minimal()

# What is the relationship between Population growth (decline) and misery growth?
ggplot(data=cd_white_misery, aes(x=Pop_growth, y=Misery_growth)) +
  geom_point() +
  stat_smooth(method="lm") +
  theme_minimal()
summary(lm(data=cd_white_misery, Misery_growth~Pop_growth))


# What is the relationship between misery growth and the trump shift?
ggplot(data=misery_votes, aes(x=Misery_growth, y=trump_shift, weight=total_votes)) +
  geom_point() +
  stat_smooth(method="lm") +
  theme_minimal()
summary(lm(data=misery_votes, trump_shift ~ Misery_growth + Death_rate_12_14, weights=total_votes))




