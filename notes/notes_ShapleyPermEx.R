d <- 3
Xall <- function(n) matrix(runif(d*n,-pi,pi),nc=d)
Xset <- function(n, Sj, Sjc, xjc) matrix(runif(n*length(Sj),-pi,pi),nc=length(Sj))
Nv=1e4
No = 1e3
Ni = 3

perms <- gtools::permutations(d, d, 1:d) #gets permutations of 1:d
m <- nrow(perms) # d!
# first Nv rows are for the variance calc
# adds an additional row for each inner loop w/in
# each outer loop for each perm for each predictor
X <- matrix(NA, ncol = d, nrow = Nv + m * (d - 1) * No *
              Ni)

X[1:Nv, ] <- Xall(Nv) # draws Nv samples from the joint distribution

# loops thru each permutation
for (p in 1:m) {
  pi <- perms[p, ] # takes the p-th permutation
  # gets the position of the elements in the permutation,
  # ie. of 1 is actually the third element in permutation pi
  pi_s <- sort(pi, index.return = TRUE)$ix
  for (j in (1:(d - 1))) {
    Sj <- pi[c(1:j)]  # takes the first j elements of a perm
    Sjc <- pi[-c(1:j)] # the remaining element

    # takes a sample from the joint dist conditional on Sjc
    xjcM <- matrix(Xset(No, Sjc, NULL, NULL), nrow = No)
    for (l in 1:No) {
      xjc <- xjcM[l, ] #takes the l-th sample from the outer loop
      xj <- Xset(Ni, Sj, Sjc, xjc) # takes Ni samples from index set of Sj, conditional on Sjc and xjc
      xx <- cbind(xj, matrix(xjc, nrow = Ni, ncol = length(xjc),
                             byrow = T)) # fixes the xjc value, samples the other values
      X[(Nv + (p - 1) * (d - 1) * No * Ni + (j - 1) *
           No * Ni + (l - 1) * Ni + 1):(Nv + (p - 1) *
                                          (d - 1) * No * Ni + (j - 1) * No * Ni + l *
                                          Ni), ] <- xx[, pi_s]
    }
  }
}