N <- 16
mpi <- data.frame(regular = rep(NA, N),
                  local = rep(NA, N),
                  localwithred = rep(NA, N),
                  matrix.small = rep(NA, N),
                  matrix.med = rep(NA, N),
                  matrix.big = rep(NA, N))

get.mean <- function(file) {
  apply(read.csv(file, header = FALSE), 1, mean)
}

mpi$regular <- get.mean("mpi.csv")
mpi$local <- get.mean("mpi_local.csv")
mpi$localwithred <- get.mean("mpi_local_withred.csv")

mpi$matrix.small <- get.mean("./MPI_mides/mida1.csv")
mpi$matrix.med <- get.mean("./MPI_mides/mida2.csv")
mpi$matrix.big <- get.mean("./MPI_mides/mida3.csv")

mpi

concurrent <- data.frame(mida1 = get.mean("./1node1task/mida1.csv"),
                         mida2 = get.mean("./1node1task/mida2.csv"),
                         mida3 = get.mean("./1node1task/mida3.csv"))

concurrent

M <- 1:48
mpi.omp <- data.frame(n1 = rep(NA, length(M)),
                      n2 = rep(NA, length(M)),
                      n3 = rep(NA, length(M)),
                      n4 = rep(NA, length(M)),
                      n5 = rep(NA, length(M)),
                      n6 = rep(NA, length(M)),
                      n7 = rep(NA, length(M)),
                      n8 = rep(NA, length(M)),
                      n9 = rep(NA, length(M)),
                      n10 = rep(NA, length(M)),
                      n11 = rep(NA, length(M)),
                      n12 = rep(NA, length(M)),
                      n13 = rep(NA, length(M)),
                      n14 = rep(NA, length(M)),
                      n15 = rep(NA, length(M)),
                      n16 = rep(NA, length(M)))

for (i in 1:16) {
  filename <- paste("./OpenMP/n", i, sep = "")
  filename <- paste(filename, ".csv", sep = "")
  mpi.omp[,i] <- get.mean(filename) 
}

mpi.omp

T1 <- mpi.omp$n1[1] # 1 node and 1 task

plot(rownames(mpi.omp),
     T1/mpi.omp$n2,
     type = "l",
     ylab = "Speedup(p)",
     xlab = "Nombre de tasks",
     xlim = c(0, 52))

for (k in 2:ncol(mpi.omp)) {
  lines(rownames(mpi.omp),
       T1/mpi.omp[,k],
       type = "l",
       col = k)
}

abline(v = apply(T1/mpi.omp[,1:16], 2, which.max),
       lty = 2)
abline(h = max(T1/mpi.omp[,1:16]),
       col = 2)

which.max(apply(T1/mpi.omp[,1:16], 2, max))

text(48 + (1:16)%%2,
     T1/mpi.omp[48, 1:16],
     labels = 1:16)
