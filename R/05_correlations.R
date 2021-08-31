# make correlation matrix

M <- cor(cst[, c(6:8)])

x11()
corrplot::corrplot(M, method = "number")