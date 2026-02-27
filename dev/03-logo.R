# tools/smile_icon_fixed.R
suppressPackageStartupMessages({
  library(ggplot2)
  library(svglite)
  library(ggimage)
})

# ---------- Marken & Layout ----------
brand_blue <- "#0B8BD9"
brand_grey <- "#5F6C7B"
brand_red  <- "#F05A4A"
alpha_cvix <- 0.14

# ---------- Smile mit deutlichem Skew ----------
m <- seq(-0.65, 0.65, length.out = 700)

curv      <- 0.55            # U-Krümmung
m_min     <- 0.08            # gewünschtes Minimum (rechts von 0)
skew_lin  <- -2 * curv * m_min  # sorgt für Minimum bei m_min
iv0       <- 0.22
wiggle    <- 0.004

iv <- iv0 + skew_lin*m + curv*m^2 + wiggle*sin(6*m)

# Knotenpunkte (optional; auf size=0 setzen, wenn unerwünscht)
knots <- c(-0.55,-0.40,-0.25,-0.12,0.02,0.18,0.33,0.50)
nodes <- data.frame(m = knots, iv = approx(m, iv, knots)$y)

# ATM-Ribbon (CVIX-Fenster)
atm_w   <- 0.06
atm_seq <- seq(-atm_w, atm_w, length.out = 200)
atm_iv  <- approx(m, iv, atm_seq)$y
cvix    <- data.frame(x = atm_seq,
                      y_lo = atm_iv - 0.018,
                      y_hi = atm_iv + 0.018)

# Bereich + Abstände so wählen, dass nichts clippt
xr <- c(-0.66, 0.66)
yr <- c(min(iv) - 0.05, max(iv) + 0.08)

p <- ggplot() +
  # ATM-Fenster
  geom_ribbon(data = cvix, aes(x = x, ymin = y_lo, ymax = y_hi),
              fill = brand_blue, alpha = alpha_cvix) +
  # fette Smile
  geom_line(aes(x = m, y = iv),
            linewidth = 1.1, color = brand_blue, lineend = "round") +
  # dezente "Nodes" (falls gewünscht)
  geom_point(data = nodes, aes(m, iv),
             size = 3.0, shape = 21, stroke = 0,
             color = brand_blue, fill = brand_blue) +
  # Achsen
  annotate("segment", x = xr[1], xend = xr[2], y = yr[1], yend = yr[1],
           colour = brand_grey, linewidth = 0.4) +
  annotate("segment", x = xr[1], xend = xr[1], y = yr[1], yend = yr[2],
           colour = brand_grey, linewidth = 0.4) +
  # Pfeilspitzen mit echten Arrows (clip aus!)
  annotate("segment", x = xr[2]-0.02, xend = xr[2], y = yr[1], yend = yr[1],
           colour = brand_grey, linewidth = 1.1,
           arrow = arrow(type = "closed", length = unit(5, "pt"))) +
  annotate("segment", x = xr[1], xend = xr[1], y = yr[2]-0.02, yend = yr[2],
           colour = brand_grey, linewidth = 1.1,
           arrow = arrow(type = "closed", length = unit(5, "pt"))) +
  # Labels mit Abstand
  annotate("text", x = xr[2]-0.01, y = yr[1]-0.022, label = "moneyness",
           hjust = 1, vjust = 1, color = brand_grey, size = 7) +
  annotate("text", x = xr[1]+0.072, y = yr[2]-0.010, label = "IV",
           hjust = 0, vjust = 1, color = brand_grey, size = 7) +
  # schlanker ATM-Tick minimal UNTER der Achse, ohne Rundkappe
  annotate("segment", x = 0, xend = 0.024,
           y = yr[1] - 0.002, yend = yr[1] - 0.002,
           linewidth = 2.2, colour = brand_red, lineend = "butt") +
  # dezentes Integral
  # annotate("text", x = 0.48, y = yr[2]-0.012, label = "\u222B",
  #          colour = brand_grey, size = 15) +
  coord_cartesian(xlim = xr, ylim = yr, expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(26, 30, 30, 28))  # mehr Rand gegen Clipping

# Ausgabe
svglite::svglite("man/figures/rqmoms_smile_icon.svg",
                 width = 2.6, height = 2.6, bg = "transparent"); print(p); dev.off()
ggsave("man/figures/rqmoms_smile_icon.png", p,
       width = 680, height = 680, units = "px", dpi = 320, bg = "transparent")

######################### HEX
# --- drop-in replacement saver: adds margin & prefers ragg for PNG ---
save_sticker2 <- function(
    filename,
    sticker = ggplot2::last_plot(),
    width  = 43.9, height = 50.8, units = "mm",
    bg = "transparent", dpi = 300,
    pad_mm = 3,           # <<< extra transparent margin around the hex
    use_ragg = TRUE,      # prefer ragg::agg_png for PNG output
    ...
) {
  # add transparent margin so the hex border isn't on the image edge
  if (pad_mm > 0) {
    sticker <- sticker +
      ggplot2::theme(
        plot.margin = ggplot2::margin(pad_mm, pad_mm, pad_mm, pad_mm, units),
        plot.background = ggplot2::element_rect(fill = bg, colour = NA)
      )
  }

  dev <- NULL
  # PNG: pick ragg if available (avoids Cairo quirks & is crisp)
  if (tools::file_ext(filename) == "png" && isTRUE(use_ragg) &&
      requireNamespace("ragg", quietly = TRUE)) {
    dev <- ragg::agg_png
  }
  # Save (no Cairo forcing; let device decide)
  ggplot2::ggsave(
    filename = filename, plot = sticker,
    width = width, height = height, units = units,
    bg = bg, dpi = dpi, device = dev, ..., limitsize = FALSE
  )
}


# --- sticker2: same API as your sticker(), but uses save_sticker2() ---
sticker2 <- function(
    subplot, s_x = 0.8, s_y = 0.75, s_width = 0.4, s_height = 0.5,
    package, p_x = 1, p_y = 1.4, p_color = "#FFFFFF", p_family = "Aller_Rg",
    p_fontface = "plain", p_size = 8, h_size = 1.2, h_fill = "#1881C2",
    h_color = "#87B13F", spotlight = FALSE, l_x = 1, l_y = 0.5,
    l_width = 3, l_height = 3, l_alpha = 0.4, url = "", u_x = 1,
    u_y = 0.08, u_color = "black", u_family = "Aller_Rg", u_size = 1.5,
    u_angle = 30, white_around_sticker = FALSE, ..., filename = paste0(package, ".png"),
    asp = 1, dpi = 300,
    # NEW:
    pad_mm = 3,            # transparent margin (mm)
    use_ragg = TRUE        # prefer ragg for PNG
) {
  hex <- ggplot2::ggplot() + geom_hexagon(size = h_size, fill = h_fill, color = NA)

  if (inherits(subplot, "character")) {
    d <- data.frame(x = s_x, y = s_y, image = subplot)
    sticker <- hex +
      geom_image(ggplot2::aes_(x = ~x, y = ~y, image = ~image), d, size = s_width, asp = asp)
  } else {
    sticker <- hex + geom_subview(subview = subplot, x = s_x, y = s_y, width = s_width, height = s_height)
  }

  sticker <- sticker + geom_hexagon(size = h_size, fill = NA, color = h_color)

  if (spotlight)
    sticker <- sticker + geom_subview(subview = spotlight(l_alpha), x = l_x, y = l_y, width = l_width, height = l_height)

  sticker <- sticker + geom_pkgname(package, p_x, p_y, color = p_color,
                                    family = p_family, fontface = p_fontface, size = p_size, ...)
  sticker <- sticker + geom_url(url, x = u_x, y = u_y, color = u_color,
                                family = u_family, size = u_size, angle = u_angle)

  if (white_around_sticker)
    sticker <- sticker + white_around_hex(size = h_size)

  sticker <- sticker + theme_sticker(size = h_size)

  # if no file requested, return the ggplot object (like original)
  if (is.null(filename)) {
    class(sticker) <- c("sticker", class(sticker))
    return(invisible(sticker))
  }

  save_sticker2(filename, sticker, dpi = dpi, pad_mm = pad_mm, use_ragg = use_ragg)
  class(sticker) <- c("sticker", class(sticker))
  invisible(sticker)
}


# Same arguments as sticker(), plus pad_px:
sticker2(
  subplot   = "man/figures/rqmoms_smile_icon.png",
  s_x       = 1.00, s_y = 1.00, s_width = 0.8, s_height = 0.8,
  package   = "rqmoms", p_size = 25, p_color = "white",
  h_fill    = "#0C1A27", h_color = "#0B8BD9", h_size = 1.0,
  dpi       = 900,
  filename  = "man/figures/logo-hex.png"
)

usethis::use_logo("man/figures/logo-hex.png")
