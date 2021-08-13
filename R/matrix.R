four_lvl_fac <- function(N, m){
  # Check on N type
  if (!is.numeric(N) || N < 1 || length(N) != 1) {
    stop("`N` must be a single positive integer.")
  }
  # Check on N value
  if (log2(N) %% 2 != 0){
    stop("`N` must be a power of two.")
  }
  # Check on m type
  if (!is.numeric(m) || length(m) != 1 || m < 1){
    stop("`m` must be a single positive integer.")
  }
  # Check on m value
  if (m >= log2(N)){
    stop("`m` must be less than log2(N).")
  }

  # Create the pseudo-factor triplets
  return(1)
}
