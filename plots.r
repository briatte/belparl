m = sapply(mget(ls(pattern = "net_(ch|se)")), function(x) x %n% "modularity")
m = cbind(names(m), m, 
          sapply(mget(ls(pattern = "net_(ch|se)")), function(x) x %n% "modularity_walktrap"),
          sapply(mget(ls(pattern = "net_(ch|se)")), function(x) x %n% "modularity_louvain"))
m = data.frame(id = m[, 1],
               ch = ifelse(substr(m[, 1], 5, 6) == "se", "Sénat", "Chambre"),
               legislature = gsub("\\D", "", m[, 1]),
               Empirical.Modularity = as.numeric(m[, 2]),
               Maximized.Walktrap = as.numeric(m[, 3]),
               Maximized.Louvain = as.numeric(m[, 4]))
m$Maximization.Ratio = m$Empirical.Modularity / apply(m[, 5:6 ], 1, max)

qplot(data = melt(m, c("id", "ch", "legislature")),
      x = legislature, group = ch, y = value, geom = c("line", "point")) +
  facet_grid(ch ~ variable) +
  theme_linedraw(16) +
  labs(y = NULL, x = NULL) +
  theme(panel.grid = element_blank())

ggsave("plots/modularity.pdf", width = 12, height = 6)
ggsave("plots/modularity.png", width = 12, height = 6, dpi = 72)

# qplot(data = m, x = legislature, y = ratio, group = ch,
#       geom = c("line", "point")) +
#   facet_grid(. ~ ch) +
#   theme_linedraw(16) +
#   labs(y = "Empirical / Maximized\n", x = NULL) +
#   theme(legend.position = "none", panel.grid = element_blank())

# by = lapply(mget(ls(pattern = "net_(ch|se)")), function(x) {
#   y = data.frame(id = gsub("(.*),(.*)", "\\1", x %n% "title"),
#                  legislature = gsub("\\D", "", x %n% "title"),
#                  party = x %v% "party",
#                  degree = x %v% "degree",
#                  distance = x %v% "distance",
#                  clustering = x %v% "clustering")
#   y = ddply(y, .(id, legislature, party), summarise,
#             degree = mean(degree),
#             distance = mean(distance),
#             clustering = mean(clustering))
#   return(y)
# })
# by = rbind.fill(by)
# 
# qplot(data = melt(subset(by, party %in% names(colors)), c("id", "legislature", "party")),
#       y = value, x = legislature, group = party, color = party, geom = "line") + 
#   facet_grid(variable ~ id, scales = "free_y") +
#   scale_color_manual("", values = colors, breaks = order)
