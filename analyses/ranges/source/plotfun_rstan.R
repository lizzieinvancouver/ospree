### Started 19 May 2021 by Cat
## Trying to fix the point estimate for mean line in the histogram - it's too short right now!!

library(reticulate)

quietgg <- function(gg) {
  suppressMessages(suppressWarnings(print(gg)))
  invisible(gg)
}

stan_plot <- function(object, pars, include = TRUE, unconstrain = FALSE,
                      ...) {
  
  inc_warmup <- FALSE
  .check_object(object, unconstrain)
  thm <- rstanvis_multiparam_theme()
  plot_data <- .make_plot_data(object, pars, include, inc_warmup, unconstrain)
  
  color_by_rhat <- FALSE # FIXME 
  dots <- list(...)
  defs <- list(point_est = "median", show_density = FALSE,
               show_outer_line = TRUE, ci_level = 0.8, outer_level = 0.95,
               fill_color = rstanvis_aes_ops("fill"), 
               outline_color = rstanvis_aes_ops("color"), 
               est_color = rstanvis_aes_ops("color"))
  args <- names(defs)
  dotenv <- list()
  for (j in seq_along(args)) {
    if (args[j] %in% names(dots))
      dotenv[[args[j]]] <- dots[[args[j]]]
    else dotenv[[args[j]]] <- defs[[j]]
  }
  if (!(dotenv[["point_est"]] %in% c("mean", "median")))
    stop("Point estimate should be either 'mean' or 'median'", call. = FALSE)
  if (color_by_rhat)
    stop("'color_by_rhat' not yet available", call. = FALSE)
  if (dotenv[["ci_level"]] > dotenv[["outer_level"]])
    stop("'ci_level' should be less than 'outer_level'", call. = FALSE)
  ci_level <- dotenv[["ci_level"]]
  outer_level <- dotenv[["outer_level"]]
  message("ci_level: ", ci_level," (",100 * ci_level, "% intervals)")
  message("outer_level: ", outer_level," (",100 * outer_level, "% intervals)")
  outer_level <- dotenv[["outer_level"]]
  probs.use <- c(0.5 - outer_level / 2, 0.5 - ci_level / 2, 0.5,
                 0.5 + ci_level / 2, 0.5 + outer_level / 2)
  samp <- plot_data$samp
  nparams <- plot_data$nparams
  statmat <- as.matrix(aggregate(samp$value, by = list(parameter = samp$parameter),
                                 FUN = function(x,...) c(mean(x), quantile(x,...)),
                                 probs = probs.use))
  param_names <- rownames(statmat) <- statmat[, 1L]
  statmat <- apply(statmat[, -1L, drop=FALSE], 1:2, as.numeric)
  colnames(statmat) <- c("mean", "2.5%", "25%", "50%", "75%", "97.5%")
  y <- as.numeric(seq(plot_data$nparams, 1, by = -1))
  xlim.use <- c(min(statmat[,2L]), max(statmat[,6L]))
  xlim.use <- xlim.use + diff(xlim.use) * c(-.05, .05)
  xy.df <- data.frame(params = rownames(statmat), y, statmat)
  colnames(xy.df) <- c("params", "y", "mean", "ll", "l", "m", "h", "hh")
  if (dotenv[["point_est"]] == "mean") xy.df$m <- xy.df$mean
  
  p.base <- ggplot2::ggplot(xy.df)
  p.name <- ggplot2::scale_y_continuous(breaks = y, labels = param_names,
                                        limits = c(0.5, nparams + 1))
  p.all <- p.base + ggplot2::xlim(xlim.use) + p.name + thm
  show_density <- dotenv[["show_density"]]
  outline_color <- dotenv[["outline_color"]] %ifNULL% rstanvis_aes_ops("color")
  fill_color <- dotenv[["fill_color"]]
  est_color <- dotenv[["est_color"]]
  if (dotenv[["show_outer_line"]] || show_density) {
    p.ci <-
      ggplot2::geom_segment(
        mapping = ggplot2::aes_string(
          x = "ll",
          xend = "hh",
          y = "y",
          yend = "y"
        ),
        color = outline_color
      )
    p.all <- p.all + p.ci
  }
  if (show_density) { # plot kernel density estimate
    npoint.den <- 512
    y.den <- x.den <- matrix(0, nrow = npoint.den, ncol = nparams)
    for(i in 1:nparams){
      d.temp <- density(samp[samp$parameter == param_names[i], "value"],
                        from = statmat[i,2L],
                        to = statmat[i,6L],
                        n = npoint.den)
      x.den[,i] <- d.temp$x
      y.max <- max(d.temp$y)
      y.den[,i] <- d.temp$y / y.max * 0.8 + y[i]
    }
    df.den <- data.frame(x = as.vector(x.den), y = as.vector(y.den),
                         name = rep(param_names, each = npoint.den))
    p.den <-
      ggplot2::geom_line(
        data = df.den,
        mapping = ggplot2::aes_string("x", "y", group = "name"),
        color = outline_color
      )
    
    #shaded interval
    y.poly <- x.poly <- matrix(0, nrow = npoint.den + 2, ncol = nparams)
    for(i in 1:nparams){
      d.temp <- density(samp[samp$parameter == param_names[i], "value"],
                        from = statmat[i, 3L],
                        to = statmat[i, 5L],
                        n = npoint.den)
      x.poly[,i] <- c(d.temp$x[1L], as.vector(d.temp$x), d.temp$x[npoint.den])
      y.max <- max(d.temp$y)
      y.poly[,i] <- as.vector(c(0, as.vector(d.temp$y) / y.max * 0.8, 0) + y[i])
    }
    df.poly <- data.frame(x = as.vector(x.poly), y = as.vector(y.poly),
                          name = rep(param_names, each = npoint.den + 2))
    p.poly <- ggplot2::geom_polygon(data = df.poly, mapping=ggplot2::aes_string("x", "y", group = "name", fill = "y"))
    p.col <- ggplot2::scale_fill_gradient(low = fill_color, high = fill_color, guide = "none")
    
    #point estimate
    if (color_by_rhat) {
      rhat_colors <- dotenv[["rhat_colors"]]
      p.point <- ggplot2::geom_segment(ggplot2::aes_string(x = "m", xend = "m", y = "y", yend = "y + 1",
                                                           color = "rhat_id"), size = 1.5)
      p.all + p.poly + p.den + p.col + p.point + rhat_colors #+ rhat_lgnd
    } else {
      p.point <- ggplot2::geom_segment(ggplot2::aes_string(x = "m", xend = "m", y = "y", yend = "y + 1"),
                                       colour = est_color, size = 1.5)
      p.all + p.poly + p.den + p.col + p.point
    }
  } else {
    p.ci.2 <- ggplot2::geom_segment(ggplot2::aes_string(x = "l", xend = "h", y = "y", yend = "y"),
                                    colour = fill_color, size = 2)
    if (color_by_rhat) {
      p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y", fill = "rhat_id"),
                                     color = "black", shape = 21, size = 4)
      p.all + p.ci.2 + p.point + rhat_colors # + rhat_lgnd
    } else {
      p.point <- ggplot2::geom_point(ggplot2::aes_string(x = "m", y = "y"), size = 4,
                                     color = fill_color, fill = est_color, shape = 21)
      p.all + p.ci.2 + p.point
    }
  }
}
