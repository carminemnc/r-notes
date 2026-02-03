#' @title datasage Visualization Toolkit
#' @description Minimal R visualization tools with theme support and Google Fonts
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom graphics par layout axis box text
#' @importFrom ggplot2 theme_void theme element_rect element_text

# Theme Configurations ----

#' @export
DARK_THEME <- list(
  bg = "#242728", fg = "#eaeaea",
  colors = c("#0085a1", "#eaeaea", "#D7263D", "#8A9A5B", "#0197F6", 
             "#FFF94F", "#E9724C", "#E27396", "#D81E5B", "#7ec995")
)

#' @export
LIGHT_THEME <- list(
  bg = "#eaeaea", fg = "#242728",
  colors = c("#0085a1", "#242728", "#D7263D", "#8A9A5B", "#0197F6", 
             "#610C9F", "#E9724C", "#E27396", "#806D40", "#7ec995")
)

# Functions ----

#' Apply Theme to Base R and ggplot2
#' @param theme Character, "dark" or "light"
#' @return Invisibly returns theme list
#' @export
apply_theme <- function(theme = "dark") {
  t <- if(theme == "dark") DARK_THEME else LIGHT_THEME
  
  # Base R
  par(bg = t$bg, fg = t$fg, col.axis = t$fg, col.lab = t$fg, 
      col.main = t$fg, col.sub = t$fg, cex.axis = 0.67, 
      cex.lab = 0.75, cex.main = 1.0, family = "Noto Serif")
  
  # ggplot2
  if(requireNamespace("ggplot2", quietly = TRUE)) {
    ggplot2::theme_set(
      ggplot2::theme_void() +
        ggplot2::theme(
          plot.background = ggplot2::element_rect(fill = t$bg, color = NA),
          panel.background = ggplot2::element_rect(fill = t$bg, color = NA),
          text = ggplot2::element_text(color = t$fg, family = "Noto Serif"),
          plot.title = ggplot2::element_text(color = t$fg, size = 14),
          legend.background = ggplot2::element_rect(fill = t$bg, color = NA),
          legend.text = ggplot2::element_text(color = t$fg),
          legend.title = ggplot2::element_text(color = t$fg)
        )
    )
  }
  
  invisible(t)
}

#' Create Flexible Plot Layout
#' @param layout_spec List of c(row, col, rowspan, colspan) vectors (0-indexed)
#' @param width Numeric, unused
#' @param height Numeric, unused
#' @export
create_layout <- function(layout_spec, width = 10, height = 6) {
  max_row <- max(sapply(layout_spec, function(x) x[1] + x[3]))
  max_col <- max(sapply(layout_spec, function(x) x[2] + x[4]))
  mat <- matrix(0, max_row, max_col)
  for(i in seq_along(layout_spec)) {
    spec <- layout_spec[[i]]
    mat[(spec[1]+1):(spec[1]+spec[3]), (spec[2]+1):(spec[2]+spec[4])] <- i
  }
  par(mfrow = c(1,1))
  layout(mat)
  par(oma = c(0, 0, 0, 0))
}

#' Setup Google Font
#' @param name Character, Google Font name
#' @return Invisibly returns font name
#' @export
setup_google_font <- function(name = "Noto Serif") {
  if(!requireNamespace("sysfonts", quietly = TRUE)) {
    stop("Install: install.packages(c('sysfonts', 'showtext'))")
  }
  sysfonts::font_add_google(name, name)
  showtext::showtext_auto()
  invisible(name)
}

#' Clean Plot Axes
#' @param sides Character vector: "top", "right", "bottom", "left"
#' @export
clean_axes <- function(sides = c("top", "right", "bottom", "left")) {
  side_map <- c(top = 3, right = 4, bottom = 1, left = 2)
  for(s in sides[sides %in% names(side_map)]) {
    axis(side_map[s], labels = FALSE, tick = FALSE)
  }
  box(bty = "n")
}

#' Add Insights Box to Plot
#' @param text Character, text to display
#' @param position Character, "right" or "left"
#' @param cex Numeric, text size (1.17 matches Python fontsize=14)
#' @param col Character, text color
#' @export
insights_box <- function(text, position = "right", cex = 1.17, col = NULL) {
  if(is.null(col)) col <- par("fg")
  pos <- switch(position,
                right = list(x = par("usr")[2] * 1.15, y = mean(par("usr")[3:4]), adj = c(0, 0.5)),
                left = list(x = par("usr")[1] * 0.85, y = mean(par("usr")[3:4]), adj = c(1, 0.5)),
                list(x = par("usr")[2] * 1.15, y = mean(par("usr")[3:4]), adj = c(0, 0.5))
  )
  text(pos$x, pos$y, text, cex = cex, col = col, adj = pos$adj, xpd = TRUE)
}