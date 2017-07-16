## alteration-group plot

theme_change <- theme(plot.background = element_blank(), 
                      panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank(), 
                      panel.background = element_blank(), 
                      axis.line = element_blank(), 
                      panel.border = element_rect(colour = "black", fill = NA, size = 0.2), 
                      strip.background = element_rect(fill = "#cccccc", colour = "black", size = 0.2))
alteration_color <- c(CA = "#e12500", CG = "#17b000", CT = "#006bfb", TA = "#00dbfb", TC = "#ff75dd", TG = "#fdb462", after = "white")
signaturePlotByAlteration <- function(data, alteration = "alteration", context = "context", value = "value", max = NULL)
{
  data <- data.frame(alteration = data[, alteration], context = data[, context], value = data[, value])
  data <- within(data, alteration <- factor(alteration, levels = c("CA", "CG", "CT", "TA", "TC", "TG")))
  data <- within(data, context <- factor(context, levels = c("A.A", "A.C", "A.G", "A.T", "C.A", "C.C", "C.G", "C.T", "G.A", "G.C", "G.G", "G.T", "T.A", "T.C", "T.G", "T.T")))
  if(is.null(max)) {
    max <- max(data$value)
  }
  
  require(zoo)
  dashed_line = data.frame(alteration = rep(c("CT", "TC"), each = 3), xintercept = rep(c(4.5, 8.5, 12.5), times = 2))
  data2 <- data[order(data$alteration, data$context), ] # ordered data by context and alteration
  data2$position <- rep(1:16, times = 6) # position of each bar
  data2$rank <- as.vector(t(rollapply(data2$value, rank, by = 4, width = 4)))
  data2 <- data2[order(data2$alteration), ]
  data2$context_3rd <- substr(data2$context, 3, 3)
  arrow <- data2[data2$alteration %in% c("CT", "TC") & data2$context_3rd == "G" & data2$rank == 4, ]
  arrow$x <- arrow$position
  arrow$y <- arrow$value + max(data$value) * 0.08
  arrow$xend = arrow$position
  arrow$yend <- arrow$value + max(data$value) * 0.006
  
  p = ggplot(data, aes(x = context, y = value, fill = alteration)) + 
    geom_bar(stat = "identity") + 
    facet_grid(. ~ alteration, scales = "free_x", space = "free_x") + 
    theme(axis.text.x = element_text(size = 5, angle = 90, hjust = 0, vjust = 0.5)) + 
    scale_fill_manual(values = alteration_color) + 
    ggtitle("Plot By Alteration") + 
    theme(legend.key.size = unit(0.3, "cm")) + 
    theme(panel.background = element_blank()) + 
    theme(panel.spacing = unit(0.05, "lines")) + 
    theme_change + 
    scale_y_continuous(expand = c(0, max * 0.02), limits = c(0, max * 1.08)) + 
    geom_vline(data = dashed_line, aes(xintercept = xintercept), linetype = "dashed", size = 0.5) + 
    geom_segment(data = arrow, aes(x = x, y = y, xend = xend, yend = yend), arrow = arrow(length = unit(0.3, "cm"), type = "closed"))
}