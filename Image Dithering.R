library(imager)
nelson <- load.image("C:\\Users\\its_t\\Documents\\CUNY Fall 2019\\9750 - Software Tools and Techniques_Data Science\\horatio_nelson.jpg")
nelson

plot(nelson)

nelson_gray <- grayscale( rm.alpha(nelson) )
plot(nelson_gray)

nelson_threshold <- nelson_gray > 0.5
plot(nelson_threshold)

rand_matrix <- matrix(
  data = runif(length(nelson_gray)),
  ncol = ncol(nelson_gray), nrow=nrow(nelson_gray))
rand_cimg <- as.cimg(rand_matrix)
plot(rand_cimg)

nelson_rand <- nelson_gray > rand_cimg
plot(nelson_rand)

#  Random noise is just too random, but maybe we can get better dithering
# by adding less random noise. What about a checker pattern?

checker_pattern <- rbind(c(1/3, 2/3),
                         c(2/3, 1/3))
plot(as.cimg(checker_pattern))


# rep_mat takes a matrix (mat) and tiles it so that the resulting
# matrix has size nrow_out x ncol_out.
# It's basically a 2d version of rep()
rep_mat <- function(mat, nrow_out, ncol_out) {
  mat[rep(seq_len(nrow(mat)), length.out = nrow_out),
      rep(seq_len(ncol(mat)), length.out = ncol_out)]
}

checker_cimg <- as.cimg(rep_mat(checker_pattern, nrow(nelson_gray), ncol(nelson_gray)))
plot(checker_cimg)

# Do the thresholding with this new checker pattern:

nelson_checker <- nelson_gray > checker_cimg
plot(nelson_checker)

gradient <- as.cimg( rep(seq(0, 1, 0.01), 101), x=101, y=101)
plot(gradient)

# This gives us three columns: black, "checker-gray", and white.
# So, using the checker pattern we can achieve some more
# nuance than with simple thresholding. 

checker_cimg <- as.cimg(rep_mat(checker_pattern,
                nrow(gradient), ncol(gradient)))
gradient_checker <- gradient > checker_cimg
plot(gradient_checker)


# Better patterns, Bayer patterns
# Calculates a non-normalized Bayer pattern matrix of size 2^n
recursive_bayer_pattern <- function(n) {
  if(n <= 0) {
    return(matrix(0))
  }
  m <- recursive_bayer_pattern(n - 1)
  rbind(
    cbind(4 * m + 0, 4 * m + 2),
    cbind(4 * m + 3, 4 * m + 1))
}

# Returns a Bayer pattern of size 2^n normalized so all values
# are between 0.0 and 1.0.
normalized_bayer_pattern <- function(n) {
  pattern <- recursive_bayer_pattern(n)
  (1 + pattern) / ( 1 + length(pattern) )
}

par(mfcol = c(1, 3), mar = c(0, 0, 2, 1), ps = 18)
plot(as.cimg(normalized_bayer_pattern(1)), main = "Bayer 2×2")
plot(as.cimg(normalized_bayer_pattern(2)), main = "Bayer 4×4")
plot(as.cimg(normalized_bayer_pattern(3)), main = "Bayer 8×8")

par(mfcol = c(1, 1))
bayer_cimg <- as.cimg(rep_mat(normalized_bayer_pattern(2),
                              nrow(gradient), ncol(gradient)))
gradient_bayer <- gradient > bayer_cimg
plot(gradient_bayer)

bayer_matrix <- rep_mat(normalized_bayer_pattern(2),
                        nrow(nelson_gray), ncol(nelson_gray))
bayer_cimg <- as.cimg(bayer_matrix)
nelson_bayer <- nelson_gray > bayer_cimg
plot(nelson_bayer)

# This method does not generalize to arbitrary color scales
nelson_bayer_color <- nelson
for(rgb_i in 1:3) {
  color_channel <- nelson_bayer_color[ , , 1, rgb_i, drop = FALSE]
  nelson_bayer_color[ , , 1, rgb_i] <- color_channel > bayer_cimg
}
plot(nelson_bayer_color)



#----
# Image dithering ggplots in R
# rep_mat takes a matrix (mat) and tiles it so that the resulting
# matrix has size nrow_out × ncol_out.
# It's basically a 2d version of rep()
rep_mat <- function(mat, nrow_out, ncol_out) {
  mat[rep(seq_len(nrow(mat)), length.out = nrow_out),
      rep(seq_len(ncol(mat)), length.out = ncol_out)]
}

# Calculates a Bayer pattern matrix of size 2^n
# Source: https://gist.github.com/MehdiNS/bd41bbc6db780c9409157d35d331ac80
recursive_bayer_pattern <- function(n) {
  if(n <= 0) {
    return(matrix(0))
  }
  m <- recursive_bayer_pattern(n - 1)
  rbind(
    cbind(4 * m + 0, 4 * m + 2),
    cbind(4 * m + 3, 4 * m + 1))
}

# Returns a Bayer pattern of size 2^n normalized so all values
# are between 1 / (m + 1) and m / (m + 1) where m is the number 
# of elements in the 2^n × 2^n matrix.
normalized_bayer_pattern <- function(n) {
  pattern <- recursive_bayer_pattern(n)
  (1 + pattern) / ( 1 + length(pattern) )
}

# Returns a  nrow_out × ncol_out cimg image repeatig a 2×2 Bayer pattern
rep_bayer_cimg <- function(nrow_out, ncol_out) {
  bayer_matrix <- rep_mat(normalized_bayer_pattern(2), nrow_out, ncol_out)
  as.cimg(bayer_matrix)
}

# Transforms a cimg image into a dithered black and white image
img_to_bayer_bw <- function(img) {
  img <- grayscale(rm.alpha(img))
  bayer_cimg <- rep_bayer_cimg(nrow(img), ncol(img))
  img >= bayer_cimg
}

# Transforms a cimg image into a dithered color image with 8 colors.
img_to_bayer_color <- function(img) {
  img <- rm.alpha(img)
  bayer_cimg <- rep_bayer_cimg(nrow(img), ncol(img))
  for(rgb_i in 1:3) {
    color_channel <- img[ , , 1, rgb_i, drop = FALSE]
    img[ , , 1, rgb_i] <- color_channel >= bayer_cimg
  }
  img
}

# create the ggplot to transform
library(ggplot2)
ggplot(mtcars, aes(factor(cyl), mpg, fill = factor(cyl))) +
  geom_violin(color = "black") +
  theme_classic() +
  theme(axis.text= element_text(colour="black"))

# Low resolution image
# This function is a hack to read in a ggplot2 plot as a cimg image 
# by saving it as a png to disk and reading it back in.
ggplot_to_cimg <- function(width, height, dpi) {
  tmp_fname <- tempfile(fileext = ".png")
  ggsave(tmp_fname, width = width, height = height, dpi = dpi, antialias = "none")
  load.image(tmp_fname)
}

plot_img <- ggplot_to_cimg( width = 3, height = 2, dpi = 140)
plot(plot_img)

# retro-dithered black and white plot
plot( img_to_bayer_bw(plot_img) )

# dithered eight-color plot
plot( img_to_bayer_color(plot_img) )
