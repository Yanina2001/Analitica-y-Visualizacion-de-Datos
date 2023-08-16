library(ggplot2)

dat <- iris
head(dat)

df_Setosa = dat[dat$class == "Iris-setosa",]
df_Versicolor = dat[dat$class == "Iris-versicolor",]
df_Virginica = dat[dat$class == "Iris-virginica",]

theList <- as.data.frame(t(c(df_Setosa[-1])), row.names = NULL)

df_Iris <- rbind(df_Setosa, df_Versicolor, df_Virginica)
dat = dat[,-5]
df_Iris <- sapply(df_Iris, as.numeric)

df_Iris$mahalnobis <- mahalanobis(dat, colMeans(dat), cov(dat))
print(df_Iris)

num.outliers <- 3
mah.ordenacion <- order(mahalanobis(dat , colMeans(dat), cov(dat)), decreasing=TRUE)
outlier3 <- rep(FALSE , nrow(dat))
outlier3[mah.ordenacion[1:num.outliers]] <- TRUE
colorear.outlier <- outlier3 * 50

plot(dat , pch=0)
points(dat , pch=colorear.outlier)