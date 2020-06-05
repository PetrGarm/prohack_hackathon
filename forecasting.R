library(ggplot2)
library(mvtsplot)
library(magrittr)
library(tidyverse)
library(forecast)
library(lmtest)
library(caret)
library(mice)
library(missForest)
library(Hmisc)
library(readxl)
library(xlsx)

train <- read.csv("C:/Users/petrg/Desktop/Prohack McKinsey/prohack_dataset_avOqBYc/train.csv")
test <- read.csv("C:/Users/petrg/Desktop/Prohack McKinsey/prohack_dataset_avOqBYc/test.csv")
sample_submit <- read.csv("C:/Users/petrg/Desktop/Prohack McKinsey/prohack_dataset_avOqBYc/sample_submit.csv")
df <- as.data.frame(train)
df_test <- as.data.frame(test)

library(reshape2)

data_wide <- dcast(df, galactic.year~galaxy, value.var="y")

ggplot(df %>% subset(galaxy == 'Andromeda I')) + 
  geom_point(aes(x=galactic.year, y=existence.expectancy.index))

diffs <- c(0,diff(unique(df$galactic.year)))
table <- table(df$galactic.year)

base_year <- df$galactic.year[1]
unique_years <- unique(df$galactic.year)

prefix_diffs <- rep(0, length(diffs))
prefix_diffs[1] <- diffs[1]
for(i in (2:(length(diffs)))) {
  prefix_diffs[i] <- prefix_diffs[i-1] + diffs[i]
}

human.years <- rep(1995, table[1])
j = 1
counts <- c(rep(0, table[1]))
years <- seq(1995, 1995 + length(unique_years) - 1)
for (i in (2:length(diffs))) {
  j = j + 1
  num = table[j]
  
  counts <- c(counts, rep(prefix_diffs[i], num))
  human.years <- c(human.years, rep(years[i], num))
}

df["human.year"] <- human.years

ggplot(df %>% subset(galaxy == 'Andromeda I'), aes(x=human.year, y=existence.expectancy.index)) + 
  geom_point() + 
  geom_smooth(method = "lm")


ggplot(df %>% subset(galaxy %in% unique(df$galaxy)[120]), aes(x=human.year, y=existence.expectancy.index)) +
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x,4)) + 
  facet_wrap(~galaxy)

ggplot(df %>% subset(galaxy %in% unique(df$galaxy)[120]), aes(x=human.year, y=y)) + 
  geom_point() + 
  geom_smooth(method = "lm", formula = y ~ poly(x,4)) + 
  facet_wrap(~galaxy)


ts_temp <- ts(subset(df, galaxy %in% unique(df$galaxy)[120]) %>% select(y), 
                            start=c(1995,1), frequency = 1)
tsdisplay(ts_temp)

train <- tsibble::as_tsibble(df[c(81, seq(2,80),1)], key=galaxy, index=human.year)



data_wide <- dcast(train, human.year~galaxy, value.var='Vulnerable.employment....of.total.employment.')

mvtsplot(data_wide[, 2:182], xtime=data_wide$human.year, levels=9, margin=TRUE)


model.1 <- lm(y ~ . + human.year + as.factor(galaxy) - galaxy - galactic.year, data = df)

#NA count
colSums(is.na(train)) / dim(train)[1] * 100 


## Try to fill NA ##
#imputed_Data  <- missForest(df[-c(2)], verbose = TRUE)

imputed_Data$OOBerror

df_imputed = imputed_Data$ximp
df_imputed["galaxy"] = df$galaxy

train_imp <- tsibble::as_tsibble(df_imputed[c(81, seq(2,80),1)], key=galaxy, index=human.year)


View(df_imputed)
data_wide_imp <- dcast(train_imp, human.year~galaxy, value.var='Vulnerable.employment....of.total.employment.')

mvtsplot(data_wide_imp[, 2:182], xtime=data_wide_imp$human.year, levels=9, margin=TRUE)


model.3 <- lm(y ~ ., data=df_imputed[-80])
summary(model.3)


#imputed_Data_test <- missForest(test[-c(2)], verbose = TRUE)
test_data = imputed_Data_test$ximp
test_data['galaxy'] = test$galaxy

y_hat = predict(model.3, test_data)

test_data['y_hat'] = y_hat
# check what we got 
ggplot(data = test_data, aes(x=y_hat)) + 
  geom_histogram(bins = 50, aes(y=..density..), colour="black", fill="lightblue") +
  geom_density(alpha=.2, fill="#FF6666") 

ggplot(data = train, aes(x=y)) + 
  geom_histogram(bins = 50, aes(y=..density..), colour="black", fill="lightgreen") +
  geom_density(alpha=.2, fill="#FF6666")

View(test_data)

sample_submit <- read.csv("C:/Users/petrg/Desktop/Prohack McKinsey/prohack_dataset_avOqBYc/sample_submit.csv")
sample_submit$pred = y_hat
sample_submit$existence.expectancy.index= test_data$existence.expectancy.index
write.xlsx(sample_submit,
          'C:\\Users\\petrg\\Desktop\\Prohack McKinsey\\prohack_dataset_avOqBYc\\submit.xlsx',
          row.names = FALSE)


# optimized file
submit <- read_excel("C:/Users/petrg/Desktop/Prohack McKinsey/prohack_dataset_avOqBYc/submit.xlsx",
                     range = "A1:C891")
View(submit)

write.csv(submit,
          'C:\\Users\\petrg\\Desktop\\Prohack McKinsey\\prohack_dataset_avOqBYc\\final_submit.csv',
          row.names = FALSE)
