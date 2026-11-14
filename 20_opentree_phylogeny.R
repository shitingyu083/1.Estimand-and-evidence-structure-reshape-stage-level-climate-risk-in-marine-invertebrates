# OpenTree synthetic-tree helper shared by Figure 1 and Supplementary Figures S2 and S8.
if (!exists("root_dir")) source(file.path("R", "00_setup.R"))
if (!requireNamespace("ape", quietly = TRUE)) {
  stop("Package 'ape' is required to render the archived OpenTree Newick tree.")
}

opentree_matches <- readr::read_csv(guess_max=100000, 
  file.path(root_dir, "data", "source_data", "opentree_tnrs_matches.csv"),
  show_col_types = FALSE, progress = FALSE
) %>%
  dplyr::filter(IncludedInInducedTree, !is.na(OttId)) %>%
  dplyr::mutate(TipLabel = paste0("ott", as.integer(OttId))) %>%
  dplyr::group_by(TipLabel, OttId) %>%
  dplyr::summarise(
    AcceptedName = paste(sort(unique(RequestedAcceptedName)), collapse = " | "),
    MatchedName = paste(sort(unique(MatchedName)), collapse = " | "),
    Phylum = dplyr::first(WoRMSPhylum),
    Class = dplyr::first(WoRMSClass),
    MatchMode = paste(sort(unique(MatchMode)), collapse = " | "),
    .groups = "drop"
  )

opentree_tree <- ape::read.tree(
  file.path(root_dir, "data", "source_data", "opentree_induced_subtree.nwk")
)
if (is.null(opentree_tree) || length(opentree_tree$tip.label) < 3L) {
  stop("The archived OpenTree Newick file is empty or invalid.")
}
if (is.null(opentree_tree$edge.length)) {
  opentree_tree$edge.length <- rep(1, nrow(opentree_tree$edge))
}
opentree_tree <- ape::reorder.phylo(opentree_tree, order = "cladewise")

missing_tip_metadata <- setdiff(opentree_tree$tip.label, opentree_matches$TipLabel)
if (length(missing_tip_metadata)) {
  stop("Missing OpenTree tip metadata for: ", paste(missing_tip_metadata, collapse = ", "))
}

opentree_coordinates <- function(tree) {
  n_tip <- length(tree$tip.label)
  ape::plot.phylo(
    tree, type = "phylogram", use.edge.length = TRUE,
    show.tip.label = FALSE, plot = FALSE, direction = "rightwards"
  )
  layout <- get("last_plot.phylo", envir = ape::.PlotPhyloEnv)
  node_x <- layout$xx
  node_y <- layout$yy
  horizontal <- tibble::tibble(
    Parent = tree$edge[, 1], Child = tree$edge[, 2],
    x = node_x[tree$edge[, 1]], xend = node_x[tree$edge[, 2]],
    y = node_y[tree$edge[, 2]], yend = node_y[tree$edge[, 2]]
  )
  vertical <- horizontal %>%
    dplyr::group_by(Parent) %>%
    dplyr::summarise(
      x = dplyr::first(x), xend = dplyr::first(x),
      ymin = min(y), ymax = max(y), .groups = "drop"
    ) %>%
    dplyr::filter(ymax > ymin) %>%
    dplyr::transmute(Parent, x, xend, y = ymin, yend = ymax)
  tips <- tibble::tibble(
    TipLabel = tree$tip.label,
    x = node_x[seq_len(n_tip)],
    y = node_y[seq_len(n_tip)]
  ) %>%
    dplyr::left_join(opentree_matches, by = "TipLabel")
  list(horizontal = horizontal, vertical = vertical, tips = tips)
}

opentree_xy <- opentree_coordinates(opentree_tree)
write_csv_safe(opentree_xy$horizontal, "data/source_data/Fig1a_opentree_edges.csv")
write_csv_safe(opentree_xy$tips, "data/source_data/Fig1a_opentree_tip_metadata.csv")

phyla <- sort(unique(opentree_xy$tips$Phylum))
phyla_palette <- setNames(
  c("#0072B2", "#D55E00", "#6F63A8", "#978DC6", "#E3864A", "#5A9FD0", "#4A417A")[seq_along(phyla)],
  phyla
)

make_opentree_plot <- function(compact = TRUE, tip_range = NULL) {
  tip_size <- if (compact) 0.75 else 1.08
  tip_labels <- opentree_xy$tips %>%
    dplyr::mutate(Label = sub(" \\|.*$", "", MatchedName),
                  label_x = max(opentree_xy$horizontal$xend, na.rm = TRUE) + 1.6)
  if (!is.null(tip_range)) tip_labels <- tip_labels %>% filter(y>=tip_range[1],y<=tip_range[2])
  phylum_band <- tip_labels %>%
    dplyr::group_by(Phylum) %>%
    dplyr::summarise(ymin = min(y) - 0.5, ymax = max(y) + 0.5, .groups = "drop") %>%
    dplyr::mutate(xmin = 0, xmax = max(opentree_xy$horizontal$xend, na.rm = TRUE) * 1.08)
  p <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = phylum_band,
      ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Phylum),
      alpha = 0.055, colour = NA, inherit.aes = FALSE) +
    ggplot2::geom_segment(
      data = opentree_xy$horizontal,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = 0.22, colour = "#4A417A", lineend = "square"
    ) +
    ggplot2::geom_segment(
      data = opentree_xy$vertical,
      ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
      linewidth = 0.28, colour = "#4A417A", lineend = "square"
    ) +
    ggplot2::geom_point(
      data = opentree_xy$tips,
      ggplot2::aes(x = x, y = y, colour = Phylum),
      size = tip_size, alpha = 0.92
    ) +
    {if (!compact) ggplot2::geom_segment(data=tip_labels,
      ggplot2::aes(x=x,xend=label_x-.4,y=y,yend=y),linewidth=.12,linetype="dotted",colour="#C3BDDF")} +
    {if (!compact) ggplot2::geom_text(
      data = tip_labels, ggplot2::aes(x = label_x, y = y, label = Label),
      hjust = 0, size = 2.3, colour = col("grey_dark"),
      family = "Arial", fontface = "italic",
      check_overlap = FALSE,
      inherit.aes = FALSE)} +
    {if (!compact && is.null(tip_range)) ggplot2::geom_text(
      data = phylum_band %>% dplyr::mutate(y = (ymin + ymax) / 2),
      ggplot2::aes(x = xmax * 0.22, y = y, label = Phylum),
      hjust = 0, fontface = "bold", size = 1.8, colour = col("grey_dark"),
      inherit.aes = FALSE)} +
    ggplot2::scale_colour_manual(values = phyla_palette, name = "Phylum") +
    ggplot2::scale_fill_manual(values = phyla_palette, guide = "none") +
    ggplot2::scale_x_continuous(limits = c(0, if (compact) NA_real_ else max(opentree_xy$horizontal$xend, na.rm = TRUE) + if(is.null(tip_range)) 25 else 65),
      expand = ggplot2::expansion(mult = c(0.01, if (compact) 0.03 else 0.02))) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.01, 0.01))) +
    ggplot2::labs(
      x = "OpenTree topological depth (unit branch lengths)", y = NULL,
      caption = paste0(
        "OpenTree synthetic induced subtree; ", nrow(opentree_xy$tips),
        " OTT tips; taxonomy only; no phylogenetic covariance model"
      )
    ) +
    ggplot2::theme_classic(base_size = er_base_size, base_family = "Arial") +
    ggplot2::coord_cartesian(ylim=if(is.null(tip_range)) NULL else tip_range+c(-.5,.5),clip = "on") +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(), axis.title.x = ggplot2::element_text(size = 6.2),
      axis.text.x = ggplot2::element_text(size = 6.5),
      legend.position = "bottom", legend.direction = "horizontal",
      legend.text = ggplot2::element_text(size = 6.5),
      legend.title = ggplot2::element_text(size = 6.0, face = "bold"),
      legend.key.width = grid::unit(2.6, "mm"),
      plot.caption = ggplot2::element_text(size = 6.5, hjust = 0),
      plot.margin = ggplot2::margin(2, 3, 2, 3)
    ) +
    ggplot2::guides(fill = "none",
                    colour = ggplot2::guide_legend(nrow = 2, byrow = TRUE))
  p
}

# Complete topology for the Supplementary Material: one tree in three
# consecutive vertical ranges, so that all 138 labelled tips fit one page.
make_opentree_three_part <- function(ranges = list(c(93,138),c(47,92),c(1,46))) {
  parts <- lapply(ranges, function(r) {
    make_opentree_plot(compact=FALSE, tip_range=r) +
      ggplot2::labs(x=paste0("Tips ", r[2], "–", r[1], " (top to bottom)"), colour="Phylum", caption=NULL) +
      ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.ticks.x=ggplot2::element_blank(),
        axis.line.x=ggplot2::element_blank(), plot.margin=ggplot2::margin(2,2,1,2),
        legend.key.height=grid::unit(2.5,"mm"), legend.title=ggplot2::element_text(size=7,face="bold"),
        legend.text=ggplot2::element_text(size=7),
        axis.title.x=ggplot2::element_text(size=7, face="bold", hjust=0)) +
      ggplot2::guides(colour=ggplot2::guide_legend(nrow=1, override.aes=list(size=2)))
  })
  patchwork::wrap_plots(parts, nrow=1, guides="collect") +
    patchwork::plot_annotation(caption=paste0("OpenTree synthetic induced subtree; ", nrow(opentree_xy$tips),
      " OTT tips shown in three consecutive vertical ranges (read left to right); dotted lines connect tips to names; ",
      "taxonomy only, no phylogenetic covariance model."),
      theme=ggplot2::theme(plot.caption=ggplot2::element_text(size=7, hjust=0, family="Arial"))) &
    ggplot2::theme(legend.position="bottom")
}
