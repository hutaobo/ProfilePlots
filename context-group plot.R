## context-group plot

require(ggplot2)
theme_change <- theme(plot.background = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.background = element_blank(),
                      axis.line = element_blank(),
                      panel.border = element_rect(colour = "black", fill = NA, size = 0.2),
                      strip.background = element_rect(fill = "#cccccc", colour = "black", size = 0.2))
new_alter_color <- c(`C>A` = "#e12500", `G>T` = "#e12500", `C>G` = "#17b000", `G>C` = "#17b000", `C>T` = "#006bfb", `G>A` = "#006bfb", `T>A` = "#00dbfb", `A>T` = "#00dbfb", `T>C` = "#ff75dd", `A>G` = "#ff75dd", `T>G` = "#fdb462", `A>C` = "#fdb462")

signaturePlotByContext <- function(data, alteration = "alteration", context = "context", value = "value", max = NULL)
{
  new_alteration <- c("C>A", "C>A", "C>A", "C>A", "C>A", "C>A", "C>A", "G>T", "C>A", "C>A", "G>T", "G>T", "C>A", "G>T", "G>T", "G>T", "C>G", "C>G", "C>G", "C>G", "C>G", "C>G", "C>G", "G>C", "C>G", "C>G", "G>C", "G>C", "C>G", "G>C", "G>C", "G>C", "C>T", "C>T", "C>T", "C>T", "C>T", "C>T", "C>T", "G>A", "C>T", "C>T", "G>A", "G>A", "C>T", "G>A", "G>A", "G>A", "T>A", "T>A", "T>A", "T>A", "T>A", "T>A", "T>A", "A>T", "T>A", "T>A", "A>T", "A>T", "T>A", "A>T", "A>T", "A>T", "T>C", "T>C", "T>C", "T>C", "T>C", "T>C", "T>C", "A>G", "T>C", "T>C", "A>G", "A>G", "T>C", "A>G", "A>G", "A>G", "T>G", "T>G", "T>G", "T>G", "T>G", "T>G", "T>G", "A>C", "T>G", "T>G", "A>C", "A>C", "T>G", "A>C", "A>C", "A>C")
  new_alteration <- factor(new_alteration, levels = c("A>C", "C>A", "T>A", "A>T", "G>T", "T>G", "C>G", "G>C", "T>C", "C>T", "G>A", "A>G"))
  new_context <- c("A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "A.G", "G.A", "G.C", "C.C", "A.C", "T.A", "G.A", "C.A", "A.A", "A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "A.G", "G.A", "G.C", "C.C", "A.C", "T.A", "G.A", "C.A", "A.A", "A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "A.G", "G.A", "G.C", "C.C", "A.C", "T.A", "G.A", "C.A", "A.A", "A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "A.G", "G.A", "G.C", "C.C", "A.C", "T.A", "G.A", "C.A", "A.A", "A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "A.G", "G.A", "G.C", "C.C", "A.C", "T.A", "G.A", "C.A", "A.A", "A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "A.G", "G.A", "G.C", "C.C", "A.C", "T.A", "G.A", "C.A", "A.A")
  new_context <- factor(new_context, levels = c("A.A", "C.C", "A.G", "C.A", "A.C", "G.A", "C.G", "A.T", "T.A", "G.C"))

  data <- data.frame(alteration = data[, alteration], context = data[, context], value = data[, value])
  data <- within(data, alteration <- factor(alteration, levels = c("CA", "CG", "CT", "TA", "TC", "TG")))
  data <- within(data, context <- factor(context, levels = c("A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "C.T", "G.A", "G.C", "G.G", "G.T", "T.A", "T.C", "T.G", "T.T")))
  data <- data[order(data$alteration, data$context), ] # ordered data by context and alteration
  data$new_alter <- new_alteration
  data$new_cont <- new_context

  if(is.null(max)) {
    max <- max(data$value)
  }

  p <- ggplot(data, aes(x = new_alter, y = value, fill = new_alter)) +
    geom_bar(stat = "identity") +
    facet_grid(. ~ new_cont, scales = "free_x", space = "free_x") +
    theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 0, vjust = 0.5)) +
    scale_fill_manual(values = new_alter_color) +
    ggtitle("Plot By Context") +
    theme(legend.key.size = unit(0.3, "cm")) +
    theme(panel.spacing = unit(0.05, "lines")) +
    theme_change +
    scale_y_continuous(expand = c(0, max * 0.02), limits = c(0, max * 1.08))
}