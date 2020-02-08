import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

r' x y z = sqrt $ x^2 + y^2 + z^2
efield sign x y = (sign * 2 / r, sign * y / r)
  where r = r' x y 10