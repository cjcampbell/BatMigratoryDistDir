library(ggpubr)
library(ggdendro)
library(viridisLite)
library(grid)
library(gridExtra)
theme_set(ggpubr::theme_pubclean())
wss <- readRDS(file.path(wd$bin, "wss.rds"))
if(!exists("mydata_clustered")) mydata_clustered <- readRDS(file.path(wd$bin, "mydata_clustered.rds"))
if(!exists("mydata_transformed")) mydata_transformed <- readRDS( file.path(wd$bin, "mydata_transformed.rds") )
keyToClusterOrder <- mydata_clustered %>% left_join(mydata_transformed) %>% dplyr::select(OriginCluster, ID, Species) %>% count(Species, OriginCluster)
load(file.path(wd$bin, "quant_clustered_simmatrices.Rdata"))
# Plots showing cluster outputs -------------------------------------------

bigPlots <- lapply(1:3, function(i){

  # Plot wss.
  wssPlot <- wss %>%
    dplyr::filter(Species == SoI[i]) %>%
    ggplot() +
      aes(x=clusters, y = wss) +
      geom_point() +
      geom_path() +
      scale_x_continuous(breaks = 1:10) +
      scale_y_continuous("WSS")

  # Plot trees.
  key <- dplyr::filter(keyToClusterOrder , Species == SoI[i] )
  tree <- clustered_simmatrices[[i]]$tree
  k <- 4
  cluster <- cutree(tree, k = k)
  clustab <- table(cluster)[unique(cluster[tree$order])]
  m <- c(0, cumsum(clustab))
  which <- 1L:k

  rectDeets <- lapply(1:4, function(n) {
    data.frame(
      l = m[which[n]] + 0.66,
      t= par("usr")[3L],
      r = m[which[n] + 1] + 0.33,
      b = mean(rev(tree$height)[(k - 1):k]),
      n = m[n+1]-m[n],
      id = n
    )
  }) %>%
    bind_rows

  p <- ggdendro::ggdendrogram(clustered_simmatrices[[i]]$tree, labels = FALSE) +
    geom_rect(
      data = rectDeets,
      aes(xmin = l, xmax = r, ymin=b, ymax = -5, color = id),
      fill = NA
    ) +
    scale_color_viridis_c() +
    geom_text(
      data= rectDeets,
      aes(y = -8, x = l + (r-l)/2 + 0.66, label = paste0("n = ", n)),
      hjust = 0.5, vjust = 1
    ) +
    scale_x_continuous(labels = NULL, expand = c(0.05,0.05)) +
    theme(
      legend.position = "none",
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    )
  p <- gridExtra::arrangeGrob(p, left = "Height")

  #p2 <- ggarrange(wssPlot + ggtitle(" "), p+ggtitle(" "), widths = c(1,4), heights = c(1,4))

  return(list(wssPlot, p))
})

p3 <- ggarrange(
  plotlist = list(
    bigPlots[[3]][[1]],bigPlots[[3]][[2]]),
    bigPlots[[1]][[1]],bigPlots[[1]][[2]],
    bigPlots[[2]][[1]],bigPlots[[2]][[2]],
  nrow = 3, ncol =2, labels = "AUTO",
  widths = c(1,3)
)
ggsave(p3, file = file.path(wd$figs, "wss_TreePlots.png"), width = 9, height = 8)

