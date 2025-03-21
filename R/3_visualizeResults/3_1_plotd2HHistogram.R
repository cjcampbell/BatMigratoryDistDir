
library(ggpubr)
# Plot histogram of d2H values.

df <- readRDS(file.path(wd$bin, "mydata_minDistDir.rds"))

p <- df %>%
  dplyr::mutate(commonName = case_when(Species == "LACI"~ "Hoary", Species == "LABO" ~ "Eastern red", Species == "LANO" ~ "Silver-haired")) %>%
  dplyr::mutate( commonName = factor(commonName, levels = c("Silver-haired", "Hoary", "Eastern red")) ) %>%
  ggplot(aes(d2H)) +
  geom_histogram() +
  facet_grid(~commonName) +
  scale_x_continuous(expression(bold(paste(delta^2 ~ H[fur], " (", "\u2030", ", VSMOW)") ))) +
  scale_y_continuous(expression(bold("Frequency"))) +
  theme_pubclean() +
  theme(
    strip.background = element_rect(fill = "white", color = "white")
  )
ggsave(p, filename = file.path(wd$figs, "histogram_dDVals.png"), width = 6, height = 3)
