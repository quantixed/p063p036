library(ggplot2)
library(cowplot)

# for D in um^2/s
# D = KT / 3 pi µ diam
seeq <- function(x) {
  return((1.38e-23 * 310.15) / (3 * pi * 0.044 * (x * 10e-9)) * 10e12)
}

generate_lines <- function(x) {
  df <- data.frame(x1 = c(-Inf, x, x),
             y1 = c(seeq(x), seeq(x), -Inf))
  return(df)
}

# assume inv is 30 nm diameter
# GFP is ~3 nm
# GBPen is 3.6 nm
# FKBP is 3.4 nm
# so 3.6 + (3 * 3.4) is 13.8, gives 27.6 increase in diameter

inv <- generate_lines(30 + 6)
furry <- generate_lines(57.6 + 6)

p2 <- ggplot() + 
  geom_function(fun = seeq, colour = "#228833") +
  geom_line(aes(x = x1, y = y1), linetype = 3, data = inv, colour = "#ab6ded") +
  geom_line(aes(x = x1, y = y1), linetype = 3, data = furry, colour = "#92278f") +
  lims(x = c(0,70)) +
  scale_y_continuous(limits = c(0,1), n.breaks = 6) +
  labs(y = "Diffusion coefficient (µm2/s)", x = "Diameter (nm)") +
  theme_cowplot(9) +
  theme(legend.position = "none")

p2

ggsave ("Model_Diffusion.pdf", p2, path = "Output/Plots", width = 3.5, height = 4, units = "cm", bg = NULL)

# if MS130_131.R has been run
Dsummary <- Pool %>% 
  group_by(Cond) %>%
  summarise(mean = mean(DiffusionCoeff))
Dsummary$diam_nm <- (1.38e-23 * 310.15) / (3 * pi * 0.044 * (Dsummary$mean / 10e12)) / 10e-9

Dsummary
