

updateLemireSeedFromR <- function(x = NULL) {
  if (is.null(x)) {
    x <- dqrng::generateSeedVectors(50)
  }
  if (is.atomic(x)) {
    do_updateLemireSeedFromR(x)
  } else {
    do_updateLemireSeedFromR(unlist(x))
  }
}


