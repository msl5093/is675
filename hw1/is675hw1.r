wbcd <­ read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

library(class, lib = "~/Rlibs")
library(gmodels, lib = "~/Rlibs")
library(car, lib = "~/Rlibs")

png("~/swe2016/is675/40401/knn3.png")
scatterplot(wbcd$texture_mean[1:50] ~ wbcd$perimeter_mean[1:50] | wbcd$diagnosis[1:50])
png("~/swe2016/is675/40401/knn4.png")
scatterplot(wbcd$area_mean ~ wbcd$perimeter_mean | wbcd$diagnosis)
png("~/swe2016/is675/40401/knn5.png")
scatterplot(wbcd$texture_mean ~ wbcd$perimeter_mean | wbcd$diagnosis)

# drop the id feature from the data frame
wbcd <­ wbcd[­1]
table(wbcd$diagnosis)

# recode diagnosis feature as a factor and provide more descriptive name by passing the
# feature to the factor function
wbcd$diagnosis <­ factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

# examine diagnosis feature using prop.table() function rounding the values by passing the
entire prop.table() function into round()
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# examine summary statistics for the remaining numeric features in the data frame
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

# create normalize function to apply min­max normalization each of the remaining numeric
#features as kNN is highly dependent upon the scale/range of numeric features
normalize <­ function(x) {
    return ((x ­ min(x)) / (max(x) ­ min(x)))
}

# apply the normalize function to each of the numeric features by using lapply() which accepts
# each feature as a list and batch apply the normalize function
# save the results in a new data frame, wbcd_n
wbcd_n <­ as.data.frame(lapply(wbcd[2:31], normalize))

# review the data in wbcd_n using the summary() to view the summary statistics by examining
# one of the numeric features
# numeric values now range from 0 to 1
summary(wbcd_n$area_mean)

# load the first 469 rows of the normalized wbcd_n data frame into a training data frame (leaving
# no value after ',' ensures that all columns are included)
# similarly create a test data frame using the final 100 rows
wbcd_train <­ wbcd_n[1:469, ]
wbcd_test <­ wbcd_n[470:569, ]

# create data frames that contain the target feature (diagnosis) which we are trying to predict,
# diagnosis is the first feature so we will include only that column
wbcd_train_labels <­ wbcd[1:469, 1]
wbcd_test_labels <­ wbcd[470:569, 1]

# apply the knn function and store the results in a data frame for later comparison
# we use a k value of 21 because this is roughly the square root of 469, our total number of
# records in the train data frame
wbcd_test_pred <­ knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)

# construct cross table to compare wbcd_test_pred against the wbcd_test_labels
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)

# apply z­score normalization to see if we can achieve more accurate results than those using a min­max normalization
wbcd_z <­ as.data.frame(scale(wbcd[­1]))
wcbd_train <­ wbcd_z[1:469, ]
wbcd_test <­ wbcd_z[470:569, ]
wbcd_train_labels <­ wbcd[1:469, 1]
wbcd_test_labels <­ wbcd[470:569, 1]

# try a few different k values
wbcd_test_pred <­ knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 21)
wbcd_test_pred <­ knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 11)
wbcd_test_pred <­ knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 15)
wbcd_test_pred <­ knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = 27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq = FALSE)