desired_v_shapley = function(df, desired_impt, shapley_values){
  data.frame(variable = colnames(df),desired = desired_impt, shapley = shapley_values) %>%
    pivot_longer( -variable, names_to="impt", values_to="value") %>%
    ggplot(aes(variable,value)) +
    geom_bar(aes(fill = impt),stat = "identity",position = "dodge")
}


wts_v_optim_wts = function(df, old_weights, optim_wts){
  data.frame(variable = colnames(df),old_weights = old_weights,
             optimized_weights = optim_wts) %>%
    pivot_longer( -variable, names_to="weight", values_to="value") %>%
    ggplot(aes(variable,value)) +
    geom_bar(aes(fill = weight),stat = "identity",position = "dodge")
}

plotRanks <- function(a, b, labels.offset=0.1, arrow.len=0.1)
{
  old.par <- par(mar=c(1,1,1,1))

  # Find the length of the vectors
  len.1 <- length(a)
  len.2 <- length(b)

  # Plot two columns of equidistant points
  plot(rep(1, len.1), 1:len.1, pch=20, cex=0.8,
       xlim=c(0, 3), ylim=c(0, max(len.1, len.2)),
       axes=F, xlab="", ylab="") # Remove axes and labels
  points(rep(2, len.2), 1:len.2, pch=20, cex=0.8)

  # Put labels next to each observation
  text(rep(1-labels.offset, len.1), 1:len.1, a)
  text(rep(2+labels.offset, len.2), 1:len.2, b)

  # Now we need to map where the elements of a are in b
  # We use the match function for this job
  a.to.b <- match(a, b)

  # Now we can draw arrows from the first column to the second
  arrows(rep(1.02, len.1), 1:len.1, rep(1.98, len.2), a.to.b,
         length=arrow.len, angle=20)
  par(old.par)
}