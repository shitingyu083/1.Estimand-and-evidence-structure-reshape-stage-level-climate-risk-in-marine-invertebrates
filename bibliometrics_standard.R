options(stringsAsFactors = FALSE)
library(ggplot2)
library(igraph)
library(grid)
library(gridExtra)
library(png)

args <- commandArgs(trailingOnly = TRUE)
data_dir <- if (length(args) >= 1) args[1] else Sys.getenv("BIBLIOMETRICS_DATA_DIR")
out_dir <- if (length(args) >= 2) args[2] else Sys.getenv("BIBLIOMETRICS_OUT_DIR")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
records <- read.csv(file.path(data_dir, "openalex_records.csv"), check.names = FALSE)
year_q <- read.csv(file.path(data_dir, "bibliometrics_year_query.csv"), check.names = FALSE)
edges <- read.csv(file.path(data_dir, "bibliometrics_keyword_edges.csv"), check.names = FALSE)
kws <- read.csv(file.path(data_dir, "bibliometrics_keywords.csv"), check.names = FALSE)
relevant <- grepl("acidification|warming|invertebrate|climate|carbon|larva|embryo|development|settlement|metamorphosis|life history|recruit|growth|survival|calcif|physiol|reproduction|benthic", kws$keyword)
pal <- c(acidification = "#0072B2", warming = "#D55E00", combined = "#6F63A8")
theme_cols <- c("Climate driver / system" = "#0072B2", "Life-history stage" = "#D55E00", "Response / process" = "#6F63A8")
theme_labels <- c("Climate driver / system" = "Climate", "Life-history stage" = "Life stage", "Response / process" = "Process")
theme_nat <- theme_classic(base_size = 9) + theme(plot.title = element_text(face = "bold", hjust = 0, size = 10), plot.subtitle = element_text(size = 8, colour = "#4A417A"), legend.position = "bottom", axis.title = element_text(size = 8), axis.text = element_text(size = 7), plot.margin = margin(5, 7, 5, 5))

p_a <- ggplot(year_q, aes(year, document_count, colour = query_label)) + geom_line(linewidth = 0.7) + geom_point(size = 1.3) + scale_colour_manual(values = pal, labels = c(acidification = "Acidification", warming = "Warming", combined = "Combined exposure"), name = NULL) + scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025)) + labs(title = "Annual publication trend", subtitle = "OpenAlex relevance-ranked records; n = 600 query returns", x = "Publication year", y = "Records per query") + theme_nat

# Display thresholds affect only readability; complete keyword and edge tables
# remain archived and are used for the audit below.
nodes <- subset(kws, document_count >= 3 & relevant)
edges_b <- subset(edges, keyword_1 %in% nodes$keyword & keyword_2 %in% nodes$keyword & cooccurrence >= 2)
g <- graph_from_data_frame(edges_b[, c("keyword_1", "keyword_2", "cooccurrence")], directed = FALSE, vertices = nodes["keyword"])
g <- delete_vertices(g, V(g)[degree(g) == 0])
V(g)$freq <- nodes$document_count[match(V(g)$name, nodes$keyword)]
V(g)$community <- membership(cluster_louvain(g, weights = E(g)$cooccurrence))
set.seed(20260906)
lay <- layout_with_fr(g, weights = E(g)$cooccurrence, niter = 1500)
lay[, 1] <- scales::rescale(lay[, 1], to = c(-1, 1)); lay[, 2] <- scales::rescale(lay[, 2], to = c(-1, 1))
png(file.path(out_dir, "Fig1_b_keyword_cooccurrence_network.png"), width = 2400, height = 1900, res = 300)
par(mar = c(0, 0, 3, 0), family = "sans")
plot(g, layout = lay, vertex.size = 3 + 1.7 * sqrt(V(g)$freq), vertex.label = ifelse(V(g)$freq >= 5, tools::toTitleCase(V(g)$name), NA), vertex.label.cex = 0.60, vertex.label.color = "#3B335F", vertex.color = c("#0072B2", "#D55E00", "#6F63A8")[((V(g)$community - 1) %% 3) + 1], vertex.frame.color = "white", edge.width = 0.25 + 1.2 * sqrt(E(g)$cooccurrence / max(E(g)$cooccurrence)), edge.color = adjustcolor("#978DC6", 0.42), main = "Keyword co-occurrence network")
mtext("All 360 deduplicated records; displayed nodes >= 3 records and edges >= 2 co-occurrences; node size = keyword frequency", side = 3, line = 0.2, cex = 0.72, col = "#4A417A")
dev.off()

term_list <- strsplit(records$keywords, ";", fixed = TRUE)
term_year <- do.call(rbind, lapply(seq_along(term_list), function(i) data.frame(keyword = tolower(trimws(term_list[[i]])), year = records$year[i])))
term_year <- term_year[term_year$keyword %in% kws$keyword[relevant], ]
theme_time <- aggregate(year ~ keyword, term_year, mean)
theme_time$document_count <- kws$document_count[match(theme_time$keyword, kws$keyword)]
theme_time <- theme_time[order(-theme_time$document_count), ][1:min(18, nrow(theme_time)), ]
theme_time$group <- ifelse(grepl("acidification|warming|invertebrate|climate|carbon", theme_time$keyword), "Climate driver / system", ifelse(grepl("larva|embryo|development|settlement|metamorphosis|life|recruit", theme_time$keyword), "Life-history stage", "Response / process"))
p_c <- ggplot(theme_time, aes(year, document_count, colour = group, size = document_count)) + geom_point(alpha = 0.85) + geom_text(aes(label = tools::toTitleCase(keyword)), size = 2.5, colour = "#3B335F", nudge_y = 3, check_overlap = TRUE) + scale_colour_manual(values = theme_cols, labels = theme_labels, name = "Theme", guide = "none") + scale_size(range = c(2, 6), guide = "none") + scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025)) + labs(title = "Thematic evolution", subtitle = "Mean publication year; size = frequency", x = "Mean publication year", y = "Documents") + theme_nat

# A burst-like, descriptive temporal view. OpenAlex does not provide a
# reproducible citation-burst history in this extract, so this is explicitly
# labelled theme emergence rather than a citation-burst test.
term_list <- strsplit(records$keywords, ";", fixed = TRUE)
term_year <- do.call(rbind, lapply(seq_along(term_list), function(i) data.frame(keyword = tolower(trimws(term_list[[i]])), year = records$year[i])))
term_year <- term_year[term_year$keyword %in% kws$keyword[relevant] & is.finite(term_year$year), ]
em <- aggregate(year ~ keyword, term_year, function(x) c(first = min(x), last = max(x), peak = as.numeric(names(which.max(table(x))))))
em <- do.call(data.frame, em)
names(em) <- c("keyword", "first_year", "last_year", "peak_year")
em$document_count <- kws$document_count[match(em$keyword, kws$keyword)]
recent <- aggregate(year ~ keyword, term_year[term_year$year >= 2020, ], length)
names(recent)[2] <- "recent_count"
em <- merge(em, recent, by = "keyword", all.x = TRUE)
em$recent_count[is.na(em$recent_count)] <- 0
em <- em[em$document_count >= 6, ]
em$recent_share <- em$recent_count / em$document_count
em <- em[order(-em$recent_share, -em$document_count), ][1:min(15, nrow(em)), ]
em$keyword <- factor(em$keyword, levels = rev(em$keyword))
em$group <- ifelse(grepl("acidification|warming|invertebrate|climate|carbon", em$keyword), "Climate driver / system", ifelse(grepl("larva|embryo|development|settlement|metamorphosis|life|recruit", em$keyword), "Life-history stage", "Response / process"))
p_emergence <- ggplot(em, aes(y = keyword, colour = group)) +
  geom_segment(aes(x = first_year, xend = last_year, yend = keyword), linewidth = 0.48, alpha = 0.78) +
  geom_point(aes(x = peak_year, size = document_count), shape = 21, fill = "white", stroke = 0.28) +
  scale_colour_manual(values = theme_cols, name = "Theme", labels = theme_labels, guide = guide_legend(order = 1, nrow = 1, override.aes = list(size = 3))) +
  scale_size(range = c(2.2, 6), name = "Frequency (n)", breaks = c(20, 60), guide = guide_legend(order = 2, nrow = 1, override.aes = list(fill = "white", colour = "#0072B2"))) + scale_x_continuous(breaks = seq(2000, 2025, 5), limits = c(2000, 2025), expand = expansion(mult = c(0.02, 0.03))) +
  labs(title = "Theme emergence across publication years", subtitle = "Line = first-to-last occurrence; point = peak year", x = "Publication year", y = NULL) + theme_nat + theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6), legend.title = element_text(size = 6, face = "bold"), legend.key.width = unit(7, "mm"), legend.spacing.x = unit(1, "mm"))

top <- subset(kws, document_count >= 6 & relevant)
top <- top[order(top$document_count, decreasing = TRUE), ][1:min(15, nrow(top)), ]
top$keyword <- gsub("effects of global warming on oceans", "global warming (ocean)", top$keyword, fixed = TRUE)
top$keyword <- gsub("carbon dioxide in earth's atmosphere", "atmospheric carbon dioxide", top$keyword, fixed = TRUE)
top$keyword <- factor(top$keyword, levels = rev(top$keyword))
top$group <- ifelse(grepl("acidification|warming|invertebrate|climate|carbon", top$keyword), "Climate driver / system", ifelse(grepl("larva|embryo|development|settlement|metamorphosis|life|recruit", top$keyword), "Life-history stage", "Response / process"))
p_d <- ggplot(top, aes(document_count, keyword, fill = group)) + geom_col(width = 0.7) + geom_text(aes(label = document_count), hjust = -0.15, size = 2.5) + scale_fill_manual(values = c("Climate driver / system" = "#0072B2", "Life-history stage" = "#D55E00", "Response / process" = "#6F63A8"), name = NULL, guide = "none") + scale_x_continuous(expand = expansion(mult = c(0, 0.12))) + labs(title = "Most frequent keywords", subtitle = "Theme-relevant terms; threshold >= 6", x = "Documents", y = NULL) + theme_nat

net_img <- readPNG(file.path(out_dir, "Fig1_b_keyword_cooccurrence_network.png"))
if (length(dim(net_img)) == 3 && dim(net_img)[3] > 3) net_img <- net_img[, , 1:3]
edge_df <- data.frame(x = lay[match(head_of(g, E(g)), 1:nrow(lay)), 1], y = lay[match(head_of(g, E(g)), 1:nrow(lay)), 2], xend = lay[match(tail_of(g, E(g)), 1:nrow(lay)), 1], yend = lay[match(tail_of(g, E(g)), 1:nrow(lay)), 2], weight = E(g)$cooccurrence)
node_df <- data.frame(x = lay[, 1], y = lay[, 2], keyword = V(g)$name, freq = V(g)$freq, community = factor(V(g)$community))
node_df$group <- ifelse(grepl("acidification|warming|invertebrate|climate|carbon", node_df$keyword), "Climate driver / system", ifelse(grepl("larva|embryo|development|settlement|metamorphosis|life|recruit", node_df$keyword), "Life-history stage", "Response / process"))
theme_cols <- c("Climate driver / system" = "#0072B2", "Life-history stage" = "#D55E00", "Response / process" = "#6F63A8")
p_b <- ggplot() + geom_segment(data = edge_df, aes(x, y, xend = xend, yend = yend, linewidth = weight), colour = "#C3BDDF", alpha = 0.45) + geom_point(data = node_df, aes(x, y, size = freq, colour = group), alpha = 0.9) + geom_text(data = subset(node_df, freq >= 5), aes(x, y, label = tools::toTitleCase(keyword)), size = 2.25, colour = "#3B335F", nudge_y = 0.035, check_overlap = TRUE) + scale_linewidth(range = c(0.10, 0.65), name = "Co-occurrence", guide = "none") + scale_size(range = c(2.5, 9), name = "Frequency (n)", breaks = c(20, 60), guide = guide_legend(order = 2, nrow = 1)) + scale_colour_manual(values = theme_cols, name = "Theme", labels = theme_labels, guide = guide_legend(order = 1, nrow = 1, override.aes = list(size = 4))) + coord_equal(xlim = range(lay[,1]) * 1.18, ylim = range(lay[,2]) * 1.18, expand = FALSE) + labs(title = "Keyword network", subtitle = "Colour = theme; size = frequency; lines = co-occurrence", x = NULL, y = NULL) + theme_void(base_size = 9) + theme(plot.title = element_text(face = "bold", hjust = 0, size = 10), plot.subtitle = element_text(size = 7, colour = "#4A417A"), legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 6), legend.title = element_text(size = 6, face = "bold"), legend.key.width = unit(7, "mm"), legend.spacing.x = unit(1, "mm"), plot.margin = margin(5, 7, 5, 5))
combined <- arrangeGrob(p_a, p_b, p_c, p_d, ncol = 2)
ggsave(file.path(out_dir, "Fig1_bibliometrics_standard_panels.pdf"), combined, width = 190, height = 135, units = "mm", bg = "white")
ggsave(file.path(out_dir, "Fig1_bibliometrics_standard_panels.png"), combined, width = 190, height = 135, units = "mm", dpi = 300, bg = "white")
audit <- data.frame(input_records = nrow(records), records_with_year = sum(!is.na(records$year)), keyword_rows = nrow(kws), edge_rows = nrow(edges), network_nodes = vcount(g), network_edges = ecount(g), plotted_top_keywords = nrow(top))
write.csv(audit, file.path(out_dir, "bibliometrics_plot_data_audit.csv"), row.names = FALSE)
print(audit)
