# build-logo.R ---------------------------------------------------------------
#
# Makes the package logo and the favicons from the two SVG files next to
# this script. Run it from the root of the package:
#
#   Rscript pkgdown/logo/build-logo.R
#
# It writes:
#
#   man/figures/logo.png   the hex, for pkgdown and the README
#   pkgdown/favicon/*      the favicons, from the bare mark
#
# It needs chromote, rsvg, magick and jsonlite. None of them is a
# dependency of the package, because this script is not part of the
# package. pkgdown/ is in .Rbuildignore, so none of this reaches CRAN.
#
# Change the design in the SVG files, then run this again. Do not edit the
# PNG files by hand: the next run replaces them.

logo_dir <- "pkgdown/logo"
fav_dir  <- "pkgdown/favicon"
font     <- "pkgdown/assets/fonts/poppins-500.woff2"

if (!file.exists(file.path(logo_dir, "rewind-hex.svg"))) {
  stop("Run this from the root of the package.", call. = FALSE)
}

dir.create("man/figures", showWarnings = FALSE, recursive = TRUE)
dir.create(fav_dir, showWarnings = FALSE, recursive = TRUE)


# --- The hex, through a headless browser -------------------------------------
#
# The wordmark is set in Poppins, the typeface of the documentation site.
# Poppins is not a system font on this machine, so rsvg would draw the word
# in a fallback face and give no warning. A browser can load the site's own
# copy of the font instead.
#
# The font goes in as a data URI. A page opened from a file cannot load a
# font from another file, because the browser treats each file as a
# separate origin.

logo_w <- 480L
logo_h <- round(logo_w * 116 / 100)   # the aspect ratio of the hexagon

# Base64 on one line. jsonlite::base64_enc() wraps its output at 72
# characters. A line break inside url(data:...) ends the URL, so the
# browser throws away the whole @font-face rule and gives no error. The
# text then falls back to the default font, and the logo looks nearly right.
b64 <- function(bytes) gsub("[\r\n]", "", jsonlite::base64_enc(bytes))

svg_markup <- paste(readLines(file.path(logo_dir, "rewind-hex.svg")), collapse = "\n")
font_b64 <- b64(readBin(font, "raw", file.size(font)))

html <- paste0(
  "<!doctype html><html><head><style>",
  "@font-face { font-family: 'Poppins'; font-weight: 500;",
  " src: url(data:font/woff2;base64,", font_b64, ") format('woff2'); }",
  "html, body { margin: 0; padding: 0; background: transparent; }",
  "svg { display: block; width: ", logo_w, "px; height: ", logo_h, "px; }",
  "</style></head><body>", svg_markup, "</body></html>"
)

b <- chromote::ChromoteSession$new()

# A transparent page, so the corners outside the hexagon stay transparent.
b$Emulation$setDefaultBackgroundColorOverride(color = list(r = 0, g = 0, b = 0, a = 0))

# Draw at twice the size, then reduce. The edges of the hexagon come out
# smoother than a drawing made at the final size.
b$Emulation$setDeviceMetricsOverride(
  width = logo_w, height = logo_h, deviceScaleFactor = 2, mobile = FALSE
)

loaded <- b$Page$loadEventFired(wait_ = FALSE)
b$Page$navigate(paste0("data:text/html;base64,", b64(charToRaw(html))))
b$wait_for(loaded)
invisible(b$Runtime$evaluate("document.fonts.ready.then(() => true)", awaitPromise = TRUE))

# Refuse to write a logo in the wrong typeface. Without this check a font
# that failed to load gives a logo that looks nearly right, and nobody
# notices until it is on the site.
#
# Do not use document.fonts.check() for this. It returns true for a face
# that FAILED to load, because it reports false only for a face that is
# still loading. A corrupt font would thus pass it. Read the status of the
# face itself, and accept only "loaded".
font_ok <- b$Runtime$evaluate(paste0(
  "Array.from(document.fonts).some(function (f) {",
  "  return f.family.replace(/['\"]/g, '') === 'Poppins' && f.status === 'loaded';",
  "})"
))$result$value
if (!isTRUE(font_ok)) {
  b$close()
  stop("Poppins did not load, so the wordmark would use a fallback font.", call. = FALSE)
}

shot <- b$Page$captureScreenshot(format = "png")
b$close()

magick::image_read(jsonlite::base64_dec(shot$data)) |>
  magick::image_resize(paste0(logo_w, "x", logo_h), filter = "Lanczos") |>
  magick::image_write("man/figures/logo.png", format = "png")

message("Wrote man/figures/logo.png (", logo_w, " x ", logo_h, ")")


# --- The favicons, from the bare mark ----------------------------------------
#
# The bare mark has no text, so rsvg draws it exactly and needs no font.
#
# These are the files that pkgdown's page template names. pkgdown copies
# the whole of pkgdown/favicon/ to the root of the site.

mark <- file.path(logo_dir, "rewind-mark.svg")

png_at <- function(size, file) {
  rsvg::rsvg_png(mark, file.path(fav_dir, file), width = size, height = size)
}

png_at(96L,  "favicon-96x96.png")
png_at(180L, "apple-touch-icon.png")
png_at(192L, "web-app-manifest-192x192.png")
png_at(512L, "web-app-manifest-512x512.png")

invisible(file.copy(mark, file.path(fav_dir, "favicon.svg"), overwrite = TRUE))

# One .ico that holds three sizes. The browser picks the one it needs.
ico <- lapply(c(16L, 32L, 48L), function(s) {
  magick::image_read(rsvg::rsvg_png(mark, width = s, height = s))
})
magick::image_write(do.call(c, ico), file.path(fav_dir, "favicon.ico"), format = "ico")

# The paths in the manifest are relative, and that is required. The site is
# at tenmeh.github.io/rewind/. A path that starts with "/" resolves to the
# root of tenmeh.github.io, which is a different site.
writeLines(c(
  "{",
  '  "name": "rewind",',
  '  "short_name": "rewind",',
  '  "icons": [',
  '    { "src": "web-app-manifest-192x192.png", "sizes": "192x192", "type": "image/png", "purpose": "any" },',
  '    { "src": "web-app-manifest-512x512.png", "sizes": "512x512", "type": "image/png", "purpose": "any" }',
  "  ],",
  '  "theme_color": "#1F1D1B",',
  '  "background_color": "#1F1D1B",',
  '  "display": "standalone"',
  "}"
), file.path(fav_dir, "site.webmanifest"))

message("Wrote ", length(list.files(fav_dir)), " files to ", fav_dir)
