## ----setup --------------------------------------------------------
library(tidyverse)
library(sf)
library(ggpubr)
library(gridExtra)

mydata_minDistDir <- readRDS(file.path(wd$bin,"mydata_minDistDir.rds") )


# Plot --------------------------------------------------------------------


plotBar <- function(df, ymax, title = NULL, xlab = NULL, ...) {

  o <- list(...)

  df <- mutate(df, Month = as.numeric(as.character(Month)))

  labs <- df %>%
    dplyr::filter(!is.na(dist), !is.na(reference)) %>%
  dplyr::group_by(Species, Month) %>%
  dplyr::summarise(n=n())

  panel_lab <-  df %>%
    dplyr::filter(!is.na(dist), !is.na(reference)) %>%
  dplyr::group_by(Species) %>%
  dplyr::summarise(n=paste0("italic(n)==", n()))

  p <- df %>%
  ggplot() +
  geom_bar(aes(x=Month,fill = dist), stat = "count") +
    scale_fill_manual(
      "Minimum distance traveled",
      values = c("#fcbf49", "#f77f00", "#d62828"),
      breaks = c("short", "mid", "long"),
      labels = c("Short\n(<100km)", "Mid\n(100-500km)", "Long\n(>500km)")
      ) +
  geom_text(
    data = labs,
    aes(x=Month, label = n, y = n),
    vjust = -0.1, size = 3, color = "grey50"
    ) +
  geom_text(
    data = panel_lab,
    aes(label = n),
    x=1, y = ymax, vjust = 1, hjust = 0, parse = T
    ) +
  facet_grid(
    reference~Species,
    scales = "free_y"
    ) +
  theme_pubr() +
  scale_x_continuous(limits = c(0.5,12.5), labels = 1:12, breaks = 1:12) +
  scale_y_continuous("Count individuals", expand = c(0,0), limits = c(0,ymax)) +
  theme(
    legend.position = "bottom",
    legend.title.align = 0.5,
    legend.text.align = 0.5,
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "plot",
    panel.background = element_rect(fill = "white", colour = NA),
    strip.background = element_rect(fill = "white", colour = NA),
    strip.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  guides(
    fill = guide_legend(title.position="top", title.hjust = 0.5)
    ) +
    o
  if(!is.null(title)) { p <- p + ggtitle(title) }
  if(!is.null(xlab)) {  p <- p + xlab(xlab) } else {p <- p + xlab(NULL) }
  return(p)

}

p1 <- dplyr::filter(mydata_minDistDir, !is.na(dist), reference == 1) %>%
  plotBar(ymax=100, title = "Reference Individuals")
p2 <- dplyr::filter(mydata_minDistDir, !is.na(dist), reference == 0) %>%
  plotBar(ymax=550, title = "Unknown-origin Individuals", xlab = "Month")

ggpubr::ggarrange(common.legend = T, legend = "bottom", ncol = 1,
  p1,
  p2
  )


