module Sample exposing (ofUniform, toExponential)

type R = R Float

ofUniform : Float -> Float -> Float -> R
ofUniform min max v = R ((v - min) / (max - min))

toExponential : { mean : Float } -> R -> Float
toExponential { mean } (R u) =
  mean * negate (logBase e (1 - u))
