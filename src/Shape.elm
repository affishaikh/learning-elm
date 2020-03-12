module Shape exposing (..)

type Shape = Rectangle Float Float | Square Float

calculateArea : Shape -> Float
calculateArea shape =
   case shape of
   (Rectangle w h) -> w * h
   (Square s) -> s * s

calculatePerimeter : Shape -> Float
calculatePerimeter shape =
   case shape of
   (Rectangle w h) -> 2 * w + 2 * h
   (Square s) -> 4 * s
