

updateLemireSeedFromR <- function(x = NULL) {
  if (length(x) <= 50L) {
    x <- unlist(dqrng::generateSeedVectors(50L))
  }
  dqrng::dqset.seed(unlist(x)[1:2])
  if (is.atomic(x)) {
    invisible(do_updateLemireSeedFromR(x))
  } else {
    invisible(do_updateLemireSeedFromR(unlist(x)))
  }
}


