desired_v_shapley = function(df, desired_impt, shapley_values,optim_shapley){
  data.frame(variable = colnames(df),desired = desired_impt, e_shapley = shapley_values, optimized = optim_shapley) %>%
    pivot_longer( -variable, names_to="impt", values_to="value") %>%
    ggplot(aes(variable,value,fill = impt)) +
    geom_bar(stat = "identity",position=position_dodge())+
    geom_text(aes(label=round(value,2)), vjust=1.6, color = 'white',
              position = position_dodge(.9), size=3)+
    xlab('Dimension')+
    ylab('Importance')+
    theme_light()+
    scale_fill_discrete(name = "Importance",
                        labels = c("Desired", "Original\nShapley\nEffect","Optimized\nShapley\nEffect"))
}


wts_v_optim_wts = function(df, old_weights, optim_wts){
  data.frame(variable = colnames(df),old_weights = old_weights,
             optimized_weights = optim_wts) %>%
    pivot_longer( -variable, names_to="weight", values_to="value") %>%
    ggplot(aes(variable,value, fill = weight)) +
    geom_bar(stat = "identity",position = position_dodge())+
    geom_text(aes(label=round(value,2)), vjust=1.6, color = 'white',
              position = position_dodge(.9), size=3.5)+
    xlab('Dimension')+
    ylab('Weight')+
    theme_light()+
    scale_fill_discrete(name = "Weights",
                        labels = c("Original", "Optimized"))
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


#https://stackoverflow.com/questions/35717353/split-violin-plot-with-ggplot2
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin,
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })
geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ...,
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}
