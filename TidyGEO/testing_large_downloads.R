library(GEOquery)
options(timeout = 600)

to_test <- c("GSE116436", "GSE56047", "GSE80205", "GSE39655", "GSE73103", "GSE49149")

# GSE116436 ---------------------------------------------------------------



start_time <- Sys.time()
browser()
t1 <- getGEO(to_test[1])
end_time <- Sys.time()
print(end_time - start_time)

# Loaded
# Time difference of 43.7487 mins

e <- t1$GSE116436_series_matrix.txt.gz
m <- pData(e)

# GSE56047 ----------------------------------------------------------------


start_time <- Sys.time()
t2 <- getGEO(to_test[2])
end_time <- Sys.time()
print(end_time - start_time)

# Loaded
# Time difference of 26.99181 mins

e2 <- t2$`GSE56047-GPL10558_series_matrix.txt.gz`
m2 <- pData(e2)

# GSE80205 ----------------------------------------------------------------


start_time <- Sys.time()
t3 <- getGEO(to_test[3])
end_time <- Sys.time()

# Did not load
# Error in .subset2(x, i) : subscript out of bounds

#e3 <- t3
#m3 <- pData(e3)

# GSE39655 ----------------------------------------------------------------


start_time <- Sys.time()
t4 <- getGEO(to_test[4])
end_time <- Sys.time()
print(end_time - start_time)

# Did not load
# Error: cannot allocate vector of size 2.6 Mb
# Time difference of 48.80416 mins

e4 <- t4
m4 <- pData(e4)

# GSE73103 ----------------------------------------------------------------


start_time <- Sys.time()
t5 <- getGEO(to_test[5])
end_time <- Sys.time()
print(end_time - start_time)

# Loaded
# Time difference of 1.117518 hours

e5 <- t5$GSE73103_series_matrix.txt.gz
m5 <- pData(e5)

# GSE49149 ----------------------------------------------------------------


start_time <- Sys.time()
t6 <- getGEO(to_test[6])
end_time <- Sys.time()
print(end_time - start_time)

e6 <- t6
m6 <- pData(t6)