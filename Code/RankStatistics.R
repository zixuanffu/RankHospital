rm(list = ls())
pacman::p_load(REBayes, data.table)
source("Code/SelectX.R")

load("Results/2013-2022/ZsLsR.Rda")
alpha1 <- 0.4
tp_1 <- data.table(Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = qKW(Z$fs, 1 - alpha1), tail = "R"))
tp_1[, Order := order(V1)]
View(tp_1)
alpha2 <- 0.5
tp_2 <- data.table(Lfdr.GLmix_temp(Z$S, Z$fs, sqrt(Z$s), cnull = qKW(Z$fs, 1 - alpha2), tail = "R"))
tp_2[, Order := order(V1)]
View(tp_2)

View(tp_1$Order)
View(tp_2$Order)

Lfdr.GLmix_temp
# tail probability...is tricky...not as simple as I thought...
