library(igraph)


node<- read.csv("/Users/ivanho/Documents/Data Wranggling/Github/dwv-assignment week 11/data/nodes.csv")
edge<- read.csv("/Users/ivanho/Documents/Data Wranggling/Github/dwv-assignment week 11/data/edges.csv")

net <- graph_from_data_frame(d = edge, vertices = node, directed = FALSE)


V(net)$color <- ifelse(V(net)$side == "Axis", "coral1",
                       ifelse(V(net)$side == "Allies", "cadetblue3", "grey70"))


ends_mat <- ends(net, E(net))
side1 <- V(net)[ends_mat[, 1]]$side
side2 <- V(net)[ends_mat[, 2]]$side

E(net)$color <- ifelse(side1 == side2, "orange", "grey75")
E(net)$width <- ifelse(side1 == side2, 4, 1)


lay <- layout_as_star(net, center = "GER", order = order(V(net)$side))


label_angle <- -atan2(lay[, 2], lay[, 1])
label_dist  <- ifelse(V(net)$name == "GER", 0, 1.5)


draw_plot <- function() {
  par(mar = c(0, 0, 2, 12), xpd = TRUE)   # wide right margin for legend
  
  plot.igraph(net,
              layout              = lay,
              vertex.label        = V(net)$name,   # short codes
              vertex.label.font   = 2,
              vertex.label.color  = "black",
              vertex.label.cex    = 0.9,
              vertex.label.dist   = label_dist,
              vertex.label.degree = label_angle,
              vertex.size         = ifelse(V(net)$name == "GER", 16, 10),
              vertex.color        = V(net)$color,
              vertex.shape        = ifelse(V(net)$switched == "Yes", "square", "circle"),
              edge.color          = E(net)$color,
              edge.width          = E(net)$width,
              main = "WWII Europe: Germany at the centre of the war")
  

  legend(x = 1.5, y = 1.1,
         legend = paste(V(net)$name, "=", V(net)$country),
         title  = "Countries",
         pch    = ifelse(V(net)$switched == "Yes", 22, 21),
         pt.bg  = V(net)$color,
         pt.cex = 1.3, cex = 0.65, bty = "n")
  
  legend(x = -1.6, y = -0.9,
         legend = c("Axis", "Allies", "Neutral",
                    "Switched sides", "Same-side war"),
         pch    = c(21, 21, 21, 22, NA),
         pt.bg  = c("coral1", "cadetblue3", "grey70", "white", NA),
         lty    = c(NA, NA, NA, NA, 1), lwd = 3,
         col    = c("black", "black", "black", "black", "orange"),
         pt.cex = 1.5, cex = 0.7, bty = "n")
}

draw_plot()