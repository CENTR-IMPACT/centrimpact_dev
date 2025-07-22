# Utility functions for visualization plots

#' Create indicators plot
#' @param indicator_data Data frame of indicators
#' @return ggplot object
create_indicators_plot <- function(indicator_data, remove_axis_labels = FALSE) {
  cat("\n=== Starting indicators plot generation ===\n")
  req(indicator_data)
  cat("Indicators data found, proceeding with visualization\n")
  numeric_cols <- sapply(indicator_data, is.numeric)
  value_col <- names(indicator_data)[numeric_cols][1]
  if (is.na(value_col)) {
    stop("No numeric column found in indicators data for visualization")
  }
  non_numeric_cols <- !sapply(indicator_data, is.numeric)
  if (any(non_numeric_cols)) {
    indicator_col <- names(indicator_data)[non_numeric_cols][1]
    plot_data <- data.frame(
      indicator = as.character(indicator_data[[indicator_col]]),
      value = as.numeric(indicator_data[[value_col]]),
      stringsAsFactors = FALSE
    )
  } else {
    plot_data <- data.frame(
      indicator = rownames(indicator_data),
      value = as.numeric(indicator_data[[value_col]]),
      stringsAsFactors = FALSE
    )
  }
  indicators_preview <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = indicator,
      r = value
    )
  ) +
  centrimpactvis::geom_indicators(
      show.legend = FALSE,
      show_reference_circles = FALSE,
      show_reference_lines = FALSE,
      scale_factor = 30,
      color = "#edeae2", alpha = 0.7,
      fill = "#edeae2", alpha = 0.7
    ) +
    ggplot2::theme_void()

  p <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = indicator,
      r = value,
      fill = indicator
    )
  ) +
    centrimpactvis::geom_indicators(
      show.legend = FALSE,
      show_reference_circles = TRUE,
      show_reference_lines = TRUE,
      scale_factor = 30
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 45,
        hjust = 1,
        size = 14,
        family = "IBM Plex Mono"
      ),
      axis.text.y = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
  if (remove_axis_labels) {
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank())
  }
  return(list(
    preview = indicators_preview,
    main = p
  ))
}

#' Create alignment plots
#' @param alignment_data Data frame of alignment medians
#' @param color_palette Color palette
#' @return list with preview and main ggplot objects
create_alignment_plots <- function(alignment_data, color_palette, remove_axis_labels = FALSE) {
  if (is.null(alignment_data)) {
    stop("No alignment analysis results available")
  }
  median_frame_long <- alignment_data %>%
    tidyr::pivot_longer(
      cols = c("partner", "researcher", "overall"),
      names_to = "role",
      values_to = "value"
    )
  median_frame_long$role <- factor(median_frame_long$role,
    levels = c("partner", "researcher", "overall")
  )
  first_group <- "partner"
  last_group <- "overall"
  left_labels <- median_frame_long %>% dplyr::filter(role == first_group)
  right_labels <- median_frame_long %>% dplyr::filter(role == last_group)
  
  # base_plot now does NOT include geom_line or geom_point
  base_plot <- ggplot2::ggplot(
    data = median_frame_long,
    ggplot2::aes(x = role, y = value, group = alignment, color = alignment)
  ) +
    ggplot2::theme(
      legend.position = "none",
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank()
    )

  # Preview plot: add white, semi-transparent lines and points
  preview_plot <- base_plot +
    ggplot2::geom_line(color = "#edeae2", alpha = 0.7, linewidth = 0.25) +
    ggplot2::geom_point(color = "#edeae2", alpha = 0.7) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank()
    )

  # Main plot: add colored lines and points as before
  main_plot <- base_plot +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = color_palette) +
    ggrepel::geom_text_repel(
      data = left_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = "lato",
      fontface = "italic",
      size = 5,
      nudge_x = -0.5,
      direction = "y",
      hjust = 1,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    ggrepel::geom_text_repel(
      data = right_labels,
      ggplot2::aes(label = alignment, x = role, y = value),
      family = "lato",
      fontface = "italic",
      size = 5,
      nudge_x = 0.5,
      direction = "y",
      hjust = 0,
      segment.size = 0.25,
      segment.color = "#4A4A4A",
      box.padding = 0.5
    ) +
    ggplot2::geom_text(
      data = median_frame_long,
      ggplot2::aes(label = round(value, 2)),
      size = 4,
      nudge_y = 0.015,
      family = "ibmplexmono"
    ) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(
        size = 8,
        family = "ibmplexmono",
        face = "italic",
        color = "#4A4A4A"
      ),
      axis.text.x = ggplot2::element_text(
        size = 10,
        family = "ibmplexmono",
        face = "italic",
        color = "#4A4A4A"
      ),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.25, color = "#E0E0E0"),
      plot.margin = ggplot2::margin(30, 30, 30, 30),
      text = ggplot2::element_text(family = "lato")
    )
  return(list(preview = preview_plot, main = main_plot))
}

#' Create dynamics plot
#' @param domain_scores Data frame of domain-level scores (for bars and labels)
#' @param dimension_scores Data frame of dimension-level scores (for stamen lines/points)
#' @return list with preview and main ggplot objects
create_dynamics_plot <- function(
  domain_scores,
  dimension_scores = NULL,
  color_palette = NULL,
  remove_axis_labels = FALSE
) {
  if (is.null(domain_scores)) {
    stop("No domain_scores data available. Please provide domain_scores data frame.")
  }

  # Ensure required columns exist in domain_scores
  if (!"domain" %in% names(domain_scores)) {
    stop("domain_scores must have a 'domain' column.")
  }
  if (!"domain_score" %in% names(domain_scores)) {
    stop("domain_scores must have a 'domain_score' column.")
  }

  # Prepare color columns if not present
  if (!"color_fill" %in% names(domain_scores)) {
    if (!is.null(color_palette)) {
      domain_scores$color_fill <- rep(color_palette, length.out = nrow(domain_scores))
    } else {
      domain_scores$color_fill <- "#edeae2"
    }
  }
  if (!"color_border" %in% names(domain_scores)) {
    domain_scores$color_border <- domain_scores$color_fill
  }

  # Define the correct domain order
  domain_order <- c(
    "Contexts",
    "Partnership Processes",
    "Interventions and Research",
    "Engaged Learning",
    "Outcomes"
  )
  
  # For domain labels around the plot - join domain_scores with dimension_scores if available
  if (!is.null(dimension_scores) && all(c("dimension", "domain") %in% names(dimension_scores))) {
    # Ensure dimensions are in their original order
    dimension_order <- unique(dimension_scores$dimension)
    dimension_scores$dimension <- factor(dimension_scores$dimension, levels = dimension_order)
    
    # Convert domain to character to avoid factor/ordered factor mismatch
    dimension_scores$domain <- as.character(dimension_scores$domain)
    
    # Get all domains, even those without dimensions
    plot_domains <- data.frame(domain = domain_order) %>%
      dplyr::left_join(
        domain_scores %>% 
          dplyr::select(domain, domain_score, color_fill, color_border) %>%
          dplyr::mutate(domain = as.character(domain)),
        by = "domain"
      )
    
    # Fill in any missing domains with default values and ensure proper factor levels
    plot_domains <- plot_domains %>%
      dplyr::mutate(
        domain_score = ifelse(is.na(domain_score), 0, domain_score),
        color_fill = ifelse(is.na(color_fill), "#edeae2", color_fill),
        color_border = ifelse(is.na(color_border), "#edeae2", color_border),
        domain = factor(domain, levels = domain_order)  # Convert back to factor with correct order
      )
  } else {
    # If no dimension_scores, just use domain_scores with proper factor levels
    plot_domains <- domain_scores %>%
      dplyr::mutate(domain = factor(domain, levels = domain_order))
  }
  
  # Create a mapping of domain to x-position (0 to n-1)
  domain_x_map <- data.frame(
    domain = domain_order,
    x = seq_along(domain_order) - 1  # 0-based indexing for polar coordinates
  )
  
  # Add x-positions for polar coordinates
  plot_domains <- plot_domains %>%
    dplyr::left_join(domain_x_map, by = "domain")
  
  # Add x-positions to dimension_scores if available
  if (!is.null(dimension_scores) && all(c("dimension", "domain") %in% names(dimension_scores))) {
    dimension_scores <- dimension_scores %>%
      dplyr::left_join(domain_x_map, by = "domain") %>%
      dplyr::group_by(domain) %>%
      dplyr::mutate(
        dim_in_domain = seq_along(dimension) - 1,
        dim_count = n(),
        # Center dimensions within their domain
        x = x + (dim_in_domain + 0.5) / dim_count - 0.5
      ) %>%
      dplyr::ungroup()
  }
  
  # Add domain labels
  plot_domains$domain_label <- as.character(plot_domains$domain)

  # Reference lines and labels
  ref_lines <- c(0.25, 0.5, 0.75, 1)
  ref_labels <- data.frame(
    y = ref_lines,
    label = sprintf("%.2f", ref_lines)
  )

  # Set up domain color mapping if color_palette is provided
  domain_levels <- unique(plot_domains$domain)
  palette_named <- NULL
  if (!is.null(color_palette)) {
    palette_named <- setNames(rep(color_palette, length.out = length(domain_levels)), domain_levels)
  }

  # Build main plot with correct geom order: bars (petals), then stamen (segments/points), then domain labels
  main_plot <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = ref_lines,
      color = "#E0E0E0", linewidth = 0.25
    ) +
    ggplot2::geom_bar(
      data = if (exists("plot_domains")) plot_domains else domain_scores,
      ggplot2::aes(
        x = x,
        y = domain_score,
        fill = domain
      ),
      stat = "identity", width = 1, show.legend = FALSE
    )

  # Add stamen geoms if dimension_scores is provided and valid
  if (!is.null(dimension_scores) && all(c("dimension", "dimension_score", "domain", "x") %in% names(dimension_scores))) {
    main_plot <- main_plot +
      # Stamen lines
      ggplot2::geom_segment(
        data = dimension_scores,
        ggplot2::aes(
          x = x,
          y = 0,
          xend = x,
          yend = dimension_score,
          color = domain
        ),
        linewidth = 2,
        alpha = 0.9,
        show.legend = FALSE
      ) +
      # Stamen points
      ggplot2::geom_point(
        data = dimension_scores,
        ggplot2::aes(
          x = x, 
          y = dimension_score, 
          color = domain
        ),
        shape = 19,  # Solid circle
        size = 6,
        alpha = 0.9,
        show.legend = FALSE
      ) +
      # Dimension labels
      ggplot2::geom_text(
        data = dimension_scores,
        ggplot2::aes(
          x = x,
          y = dimension_score + 0.05,  # Slightly above the point
          label = dimension,
          color = domain
        ),
        size = 2.5,
        vjust = 0,
        hjust = 0.5,
        angle = 0,
        show.legend = FALSE
      )
  }

  # Add reference line labels
  main_plot <- main_plot +
    ggplot2::geom_text(
      data = ref_labels,
      ggplot2::aes(x = 0.5, y = y, label = label),
      family = "ibmplexmono",
      fontface = "italic",
      size = 8,
      color = "#4A4A4A",
      hjust = 0.5,
      vjust = 0.5,
      show.legend = FALSE
    )
  
  # Add domain labels outside the plot
  if (exists("plot_domains")) {
    domain_labels <- plot_domains
  } else {
    domain_labels <- domain_scores %>%
      dplyr::group_by(domain) %>%
      dplyr::summarise(
        x = first(x),
        domain_score = first(domain_score),
        color_fill = first(color_fill),
        color_border = first(color_border),
        .groups = 'drop'
      )
  }
  
  # Add domain labels at the outer edge
  main_plot <- main_plot +
    ggplot2::geom_text(
      data = domain_labels,
      ggplot2::aes(
        x = x,
        y = 1.1,  # Position outside the main plot area
        label = domain,
        color = domain
      ),
      family = "ibmplexmono",
      fontface = "bold",
      size = 3.5,
      vjust = 0.5,
      hjust = 0.5,
      show.legend = FALSE
    )

  # Add color/fill scales with different alpha for fill
  if (!is.null(palette_named)) {
    # Create 50% lighter versions of colors for the fill
    lighten_color <- function(color, factor = 0.5) {
      col <- grDevices::col2rgb(color)
      col <- col + (255 - col) * (1 - factor)
      grDevices::rgb(t(col), maxColorValue = 255)
    }
    
    fill_palette <- sapply(palette_named, lighten_color, USE.NAMES = TRUE)
    
    main_plot <- main_plot +
      ggplot2::scale_fill_manual(values = fill_palette, guide = "none") +
      ggplot2::scale_color_manual(values = palette_named, guide = "none")
  } else {
    main_plot <- main_plot +
      ggplot2::scale_color_identity(guide = "none") +
      ggplot2::scale_fill_identity(guide = "none")
  }

  main_plot <- main_plot +
    ggplot2::scale_y_continuous(
      limits = c(0, 1.2),  # Extra space for domain labels
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_number(accuracy = 0.01),
      expand = ggplot2::expansion(mult = c(0, 0.1))  # Add space at the top for labels
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(domain_order) - 0.5,
      labels = NULL,
      expand = c(0, 0)
    ) +
    ggplot2::coord_polar(start = -pi/2, clip = "off") +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.5, color = "#E0E0E0", linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "ibmplexmono"),
      legend.position = "none",
      plot.margin = ggplot2::margin(10, 10, 40, 10)  # Add margin for domain labels
    )

  # For preview, just show the base plot without stamen lines/points
  preview_plot <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = ref_lines,
      color = "#E0E0E0", linewidth = 0.25
    ) +
    ggplot2::geom_bar(
      data = if (exists("plot_domains")) plot_domains else domain_scores,
      ggplot2::aes(
        x = x,
        y = domain_score,
        fill = domain
      ),
      stat = "identity", width = 0.9, show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = ref_labels,
      ggplot2::aes(x = 0.5, y = y, label = label),
      family = "ibmplexmono",
      fontface = "italic",
      size = 8,
      color = "#4A4A4A",
      hjust = 0.5,
      vjust = 0.5,
      show.legend = FALSE
    ) +
    ggplot2::geom_text(
      data = if (exists("plot_domains")) plot_domains else domain_scores,
      ggplot2::aes(
        y = domain_score + 0.1,  # Slightly lower position
        x = x,
        label = domain_label
      ),
      family = "ibmplexmono", 
      fontface = "italic", 
      size = 3,
      show.legend = FALSE,
      vjust = 0.5,
      hjust = 0.5
    )

  if (!is.null(palette_named)) {
    preview_plot <- preview_plot +
      ggplot2::scale_fill_manual(values = palette_named, guide = "none")
  } else {
    preview_plot <- preview_plot +
      ggplot2::scale_fill_identity(guide = "none")
  }

  preview_plot <- preview_plot +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.25, 0.5, 0.75, 1),
      labels = scales::label_number(accuracy = 0.01)
    ) +
    ggplot2::scale_x_discrete() +
    ggplot2::coord_polar(start = -pi/2, clip = "off") +
    ggplot2::theme(
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_line(linewidth = 0.5, color = "#E0E0E0", linetype = "dashed"),
      panel.grid.minor = ggplot2::element_blank(),
      text = ggplot2::element_text(family = "ibmplexmono"),
      legend.position = "none"
    )

  return(list(preview = preview_plot, main = main_plot))
}

#' Create cascade effects plot using Vogel spiral and dynamic rings
#'
#' @param cascade_df Data frame with columns Degree (1,2,3...) and Score (numeric for each degree)
#' @param color_palette Optional vector of hex colors for each layer (length must match # of Degrees)
#' @return A list with two ggplot2 objects: preview and main
#' @import ggplot2
#' @import ggforce
create_cascade_plot <- function(cascade_df, color_palette = NULL) {
  # Validate and standardize column names
  if (!all(c("Degree", "Score") %in% names(cascade_df))) {
    if (all(c("layer_number", "layer_score") %in% names(cascade_df))) {
      cascade_df$Degree <- cascade_df$layer_number
      cascade_df$Score <- cascade_df$layer_score
    } else {
      stop("Cascade results must have columns 'Degree' and 'Score', or 'layer_number' and 'layer_score'.")
    }
  }

  # Sort degrees and prepare proportions
  cascade_df <- cascade_df[order(as.numeric(cascade_df$Degree)), ]
  n_total <- 200
  scores <- as.numeric(cascade_df$Score)
  n_layers <- length(scores)

  # Distribute total points across layers proportionally
  n_points <- round(n_total * scores / sum(scores))
  n_points[length(n_points)] <- n_total - sum(n_points[-length(n_points)])  # adjust remainder
  layer_vec <- rep(1:n_layers, times = n_points)

  # Generate Vogel spiral coordinates
  t <- seq(1, n_total)
  r <- sqrt(t)
  golden_angle <- pi * (3 - sqrt(5))
  theta <- t * golden_angle
  x <- r * cos(theta)
  y <- r * sin(theta)

  vogel_df <- data.frame(x = x, y = y, layer = layer_vec)

  # Assign colors
  if (!is.null(color_palette)) {
    vogel_df$color <- color_palette[vogel_df$layer]
  } else {
    default_cols <- c("#8A7A8F", "#B49291", "#E0D0A6")  # fallback colors
    vogel_df$color <- default_cols[vogel_df$layer]
  }

  # Calculate max radius for each layer
  layer_radii <- stats::aggregate(sqrt(vogel_df$x^2 + vogel_df$y^2),
                                  by = list(layer = vogel_df$layer),
                                  FUN = max)
  names(layer_radii)[2] <- "radius"

  # Create concentric ring data
  circle_df <- data.frame(
    x0 = 0,
    y0 = 0,
    r = layer_radii$radius + 2  # slight padding
  )

  # Optional: Degree labels at top of each ring
  label_df <- circle_df
  label_df$label <- paste("Degree", 1:nrow(label_df))
  label_df$x <- 0
  label_df$y <- label_df$r

  # Build base plot
  base_plot <- ggplot2::ggplot(vogel_df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(ggplot2::aes(color = factor(layer)),
                        size = 5, alpha = 0.9, show.legend = FALSE) +
    ggplot2::scale_color_manual(values = unique(vogel_df$color)) +
    ggforce::geom_circle(data = circle_df,
                         ggplot2::aes(x0 = x0, y0 = y0, r = r),
                         color = "#4A4A4A", linetype = "dotted",
                         inherit.aes = FALSE) +
    ggplot2::geom_text(data = label_df,
                       ggplot2::aes(x = x, y = y, label = label),
                       inherit.aes = FALSE,
                       vjust = -0.5, size = 3,
                       color = "#4A4A4A") +
    ggplot2::coord_fixed() +
    ggplot2::theme_void()

  # Lightweight preview version
  preview_plot <- base_plot

  # Full main version with minimal theme
  main_plot <- base_plot +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )

  return(list(preview = preview_plot, main = main_plot))
}
