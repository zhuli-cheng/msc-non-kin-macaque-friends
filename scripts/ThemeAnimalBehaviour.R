theme_AnimalBehaviour <- function(base_size = 16) {
  theme_classic(base_size = base_size) %+replace%
    theme(
      panel.grid = element_blank(),
      
      # FULL BOX
      panel.border = element_rect(
        color = "black",
        fill = NA,
        linewidth = 0.8
      ),
      
      axis.line = element_line(color = "black"),
      
      # TICKS INSIDE
      axis.ticks.length = unit(-0.25, "cm"),
      axis.ticks = element_line(color = "black"),
      
      # TEXT (NOT BOLD)
      axis.text = element_text(size = base_size, face = "plain"),
      axis.title = element_text(size = base_size + 2, face = "plain"),
      strip.text = element_text(size = base_size + 2, face = "plain"),
      
      legend.title = element_text(size = base_size, face = "plain"),
      legend.text = element_text(size = base_size - 1, face = "plain")
    )
}

scale_clean_x <- function(...) {
  scale_x_continuous(
    ...,
    labels = function(x) format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
    )
}

scale_clean_y <- function(...) {
  scale_y_continuous(
    ...,
    labels = function(x) format(x, trim = TRUE, scientific = FALSE, drop0trailing = TRUE)
    )
}

