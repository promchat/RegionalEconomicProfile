setwd("D:/Promit Chatterjee_UPenn_970401442/CPLN 620/Lab 2")

rm(list=ls())

library(tidyverse)

ATL_qwi <- read.csv("Data/Atlanta/Atlanta QWI 2011 2021.csv")

USA_qwi <- read.csv("Data/USA/National QWI 2011 2021.csv")

Avg_emp_earn_ATL <- ATL_qwi %>% group_by(industry_label.value, year) %>% summarise(avg_emp = mean(EmpTotal), avg_earn = mean(EarnS))

Avg_emp_earn_USA <- USA_qwi %>% group_by(industry_label.value, year) %>% summarise(avg_emp = mean(EmpTotal), avg_earn = mean(EarnS))


Avg_emp_earn_ATL <- pivot_wider(data = Avg_emp_earn_ATL, names_from = year, values_from = c("avg_emp", "avg_earn"), values_fill = 0)

Avg_emp_earn_ATL <- Avg_emp_earn_ATL %>% mutate(emp_grow = round((avg_emp_2021 - avg_emp_2011)/avg_emp_2011,2),
                                                earn_grow = round((avg_earn_2021 - avg_earn_2011)/avg_earn_2011,2))


Avg_emp_earn_USA <- pivot_wider(data = Avg_emp_earn_USA, names_from = year, values_from = c("avg_emp", "avg_earn"), values_fill = 0)

Avg_emp_earn_USA <- Avg_emp_earn_USA %>% mutate(emp_grow = round((avg_emp_2021 - avg_emp_2011)/avg_emp_2011,2),
                                                earn_grow = round((avg_earn_2021 - avg_earn_2011)/avg_earn_2011,2))


test <- test %>% mutate(pct_emp_2011 = avg_emp_2011/sum(avg_emp_2011, na.rm = T), pct_emp_2021 = avg_emp_2021/sum(avg_emp_2021, na.rm = T))

Avg_emp_earn_ATL$pct_emp_2011 <- 0

Avg_emp_earn_ATL$pct_emp_2021 <- 0

Avg_emp_earn_ATL$pct_emp_2011 <- Avg_emp_earn_ATL$avg_emp_2011/sum(Avg_emp_earn_ATL$avg_emp_2011, na.rm = T)

Avg_emp_earn_ATL$pct_emp_2021 <- Avg_emp_earn_ATL$avg_emp_2021/sum(Avg_emp_earn_ATL$avg_emp_2021, na.rm = T)

options(scipen = 999)

Avg_emp_earn_USA$pct_emp_2011 <- 0

Avg_emp_earn_USA$pct_emp_2021 <- 0

Avg_emp_earn_USA$pct_emp_2011 <- Avg_emp_earn_USA$avg_emp_2011/sum(Avg_emp_earn_USA$avg_emp_2011, na.rm = T)

Avg_emp_earn_USA$pct_emp_2021 <- Avg_emp_earn_USA$avg_emp_2021/sum(Avg_emp_earn_USA$avg_emp_2021, na.rm = T)

test <- Avg_emp_earn_ATL

test$LQ <- 0

test$LQ <- 