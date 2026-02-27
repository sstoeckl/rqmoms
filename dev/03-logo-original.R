# tools/make_hex_from_original.R
dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(hexSticker)
})

src_png <- "man/figures/logo.png"   # <— dein vorhandenes PNG hierhin legen
out_png <- "man/figures/logo-hex.png"

if (!file.exists(src_png)) {
  stop("Bitte dein Originalbild als ", src_png, " ablegen.")
}

hexSticker::sticker(
  subplot   = src_png,
  s_x       = 1.0,     # evtl. leicht anpassen (Position)
  s_y       = 0.80,
  s_width   = 0.6,    # Größe des Bildes im Hex
  s_height  = 0.6,
  package   = "rqmoms",
  p_size    = 18,
  p_color   = "white",
  h_fill    = "#0AABA9",
  h_color   = "#0766A7",
  dpi       = 320,
  filename  = out_png,
  white_around_sticker = FALSE
)

message("Hex erzeugt: ", normalizePath(out_png))

