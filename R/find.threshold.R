reversals <- function(deltas){
  n = length(delta)
  delta.changes = deltas[2:n] - deltas[1:(n-1)]
  non.zero.indices = which(abs(delta.changes) > 0)
  non.zero.changes = delta.changes[non.zero.indices]

  m = length(non.zero.changes)
  reversals =
    which((non.zero.changes[2:m] * non.zero.changes[1:(m-1)]) < 0)

  deltas[ non.zero.indices[reversals+1] + 1 ]
}
