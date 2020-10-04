# install.packages("remotes")
remotes::install_github("allisonhorst/palmerpenguins")


library(palmerpenguins)
data(package = 'palmerpenguins')

head(penguins)

head(penguins_raw)

summary(penguins)

summary(penguins_raw)

colnames(penguins)

str(penguins)

summary(penguins$`Body Mass (g)`)

library(tidyr)
library(dplyr)
library(ggplot2)

df <- penguins
df %>% drop_na()

summary(df)

names(df)

ggplot(aes(x = species, y = body_mass_g), data = df) + geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', shape = 4)
