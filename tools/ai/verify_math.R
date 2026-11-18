Sys.setlocale('LC_CTYPE', 'Korean_Korea.utf8')
args <- c(normalizePath(getwd(), winslash = '/'))
for (file in list.files('R', pattern='^ai_.*[.]R$', full.names=TRUE)) source(file,encoding='UTF-8')
.ai_math_assets <- normalizePath('inst/assets/katex',winslash='/')
count <- function(x) { m <- gregexpr('class="ai-math"', .ai_markdown_html(x), fixed=TRUE)[[1L]]; if(m[1L]<0) 0L else length(m) }
cases <- c(
  '$M = 2.81$ and $t(40.1) = 0.71, p = .48$' = 2L,
  '$x_i^2$' = 1L,
  '\\(x_i^2\\)' = 1L,
  '\\[\\frac{a}{b}\\]' = 1L,
  '$$\\sum_{i=1}^n x_i$$' = 1L,
  '$$\nx_i^2\n$$' = 1L,
  '`$x$` then $y$' = 1L,
  '``$x$`` then $y$' = 1L,
  '```r\n$x$\n```\n$y$' = 1L,
  '~~~\n$x$\n~~~\n$y$' = 1L,
  '```r\n$x$' = 0L,
  '    $x$\n\n$y$' = 1L,
  '\\$20 and \\$30' = 0L,
  '$20 and $30' = 0L,
  '| value |\n| --- |\n| $P(A|B)$ |' = 1L,
  '$x < y$' = 1L,
  '$x_' = 0L
)
# Convert fixture escape sequences to actual input.
for(i in seq_along(cases)) {
  input <- names(cases)[i]
  input <- gsub('\\n', '\n', input, fixed=TRUE)
  input <- gsub('\\\\', '\\', input, fixed=TRUE)
  actual <- count(input)
  if(actual != cases[i]) stop('Failed: ', input, ' expected ',cases[i], ' got ',actual)
}
stopifnot(grepl('data-tex="x_i^2"', .ai_markdown_html('$x_i^2$'), fixed=TRUE))
stopifnot(grepl('&lt;',.ai_markdown_html('$x < y$'),fixed=TRUE))
stopifnot(file.exists(file.path(.ai_math_assets,'katex.min.js')))
stopifnot(identical(.ai_markdown_html(''), ''))
cat('PASS:',length(cases),'math/code/streaming fixtures; asset path resolved\n')
# Exercise the real viewer writer without requiring a running RStudio session.
ns <- asNamespace('rstudioapi')
old_available <- get('isAvailable',ns); old_viewer <- get('viewer',ns)
assignInNamespace('isAvailable',function(...) TRUE,'rstudioapi')
assignInNamespace('viewer',function(...) invisible(NULL),'rstudioapi')
v <- .ai_viewer('math regression','math preview')
v$push('$M = 2.81$ and \\(x_i^2\\)\n\n$$\\frac{a}{b}$$')
v$status('complete')
out <- file.path(args[1L], 'verification')
dir.create(out, showWarnings=FALSE)
file.copy(list.files(dirname(v$path),full.names=TRUE),out,recursive=TRUE,overwrite=TRUE)
assignInNamespace('isAvailable',old_available,'rstudioapi')
assignInNamespace('viewer',old_viewer,'rstudioapi')
cat('PASS: viewer HTML, snapshot and local assets generated\n')
