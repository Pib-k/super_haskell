module Graphics.FontMetrics where

import Graphics.Gloss

-- | Font metrics of GLUT Roman stroke font (Hershey font), in font units.
data CharMetrics = CharMetrics
    { charWidth :: Float
    , charHeight :: Float
    , charCenterY :: Float
    , charAdvance :: Float
    }
    deriving (Eq, Show)

-- | Total advance width of a string, in font units.
stringAdvance :: String -> Float
stringAdvance = sum . map (charAdvance . charMetrics)

-- | Renders any string scaled to 'targetH'.
centeredString :: String -> Float -> Picture
centeredString str targetH =
    let s = targetH / 152.38
        totalAdv = stringAdvance str * s
        dx = -totalAdv / 2
        dy = -42.86 * s
     in translate dx dy $ scale s s (text str)

{- | Returns GLUT Roman stroke font metrics for any ASCII character.
Sources:
https://www.opengl.org/resources/libraries/glut/spec3/node78.html
https://github.com/freeglut/freeglut/blob/master/src/fg_stroke_roman.c
-}
charMetrics :: Char -> CharMetrics
charMetrics ' ' = CharMetrics 0.0 0.0 0.0 104.76
charMetrics '!' = CharMetrics 9.52 100.0 50.0 26.62
charMetrics '"' = CharMetrics 38.1 33.33 83.33 51.44
charMetrics '#' = CharMetrics 71.43 152.38 42.86 79.49
charMetrics '$' = CharMetrics 66.67 138.1 50.0 76.21
charMetrics '%' = CharMetrics 85.71 100.0 50.0 96.57
charMetrics '&' = CharMetrics 95.24 100.0 50.0 101.76
charMetrics '\'' = CharMetrics 0.0 33.33 83.33 13.62
charMetrics '(' = CharMetrics 33.33 152.38 42.86 47.17
charMetrics ')' = CharMetrics 33.33 152.38 42.86 47.53
charMetrics '*' = CharMetrics 47.62 57.14 42.86 59.44
charMetrics '+' = CharMetrics 85.71 85.71 42.86 97.25
charMetrics ',' = CharMetrics 9.52 28.57 (-4.76) 26.06
charMetrics '-' = CharMetrics 85.71 100.0 42.86 100.75
charMetrics '.' = CharMetrics 9.52 9.52 4.76 26.48
charMetrics '/' = CharMetrics 66.67 114.29 42.86 82.11
charMetrics '0' = CharMetrics 66.67 100.0 50.0 77.07
charMetrics '1' = CharMetrics 23.81 100.0 50.0 66.53
charMetrics '2' = CharMetrics 66.67 100.0 50.0 77.65
charMetrics '3' = CharMetrics 66.67 100.0 50.0 77.05
charMetrics '4' = CharMetrics 71.43 100.0 50.0 80.17
charMetrics '5' = CharMetrics 66.67 100.0 50.0 77.69
charMetrics '6' = CharMetrics 61.9 100.0 50.0 73.8
charMetrics '7' = CharMetrics 66.67 100.0 50.0 77.23
charMetrics '8' = CharMetrics 66.67 100.0 50.0 77.67
charMetrics '9' = CharMetrics 61.9 100.0 50.0 74.06
charMetrics ':' = CharMetrics 9.52 66.67 33.33 26.22
charMetrics ';' = CharMetrics 9.52 85.71 23.81 26.3
charMetrics '<' = CharMetrics 76.19 85.71 42.86 81.61
charMetrics '=' = CharMetrics 85.71 28.57 42.86 97.25
charMetrics '>' = CharMetrics 76.19 85.71 42.86 81.61
charMetrics '?' = CharMetrics 57.14 100.0 50.0 73.9
charMetrics '@' = CharMetrics 61.9 61.9 45.24 74.36
charMetrics 'A' = CharMetrics 76.19 100.0 50.0 80.49
charMetrics 'B' = CharMetrics 66.67 100.0 50.0 83.63
charMetrics 'C' = CharMetrics 71.43 100.0 50.0 84.49
charMetrics 'D' = CharMetrics 66.67 100.0 50.0 85.29
charMetrics 'E' = CharMetrics 61.9 100.0 50.0 78.18
charMetrics 'F' = CharMetrics 61.9 100.0 50.0 78.74
charMetrics 'G' = CharMetrics 71.43 100.0 50.0 89.77
charMetrics 'H' = CharMetrics 66.67 100.0 50.0 89.09
charMetrics 'I' = CharMetrics 0.0 100.0 50.0 21.3
charMetrics 'J' = CharMetrics 47.62 100.0 50.0 60.0
charMetrics 'K' = CharMetrics 66.67 100.0 50.0 79.33
charMetrics 'L' = CharMetrics 57.14 100.0 50.0 71.32
charMetrics 'M' = CharMetrics 76.19 100.0 50.0 97.21
charMetrics 'N' = CharMetrics 66.67 100.0 50.0 88.81
charMetrics 'O' = CharMetrics 76.19 100.0 50.0 88.83
charMetrics 'P' = CharMetrics 66.67 100.0 50.0 85.67
charMetrics 'Q' = CharMetrics 76.19 109.52 45.24 88.09
charMetrics 'R' = CharMetrics 66.67 100.0 50.0 82.37
charMetrics 'S' = CharMetrics 66.67 100.0 50.0 80.83
charMetrics 'T' = CharMetrics 66.67 100.0 50.0 71.95
charMetrics 'U' = CharMetrics 66.67 100.0 50.0 89.49
charMetrics 'V' = CharMetrics 76.19 100.0 50.0 81.61
charMetrics 'W' = CharMetrics 95.24 100.0 50.0 100.52
charMetrics 'X' = CharMetrics 66.67 100.0 50.0 72.37
charMetrics 'Y' = CharMetrics 76.19 100.0 50.0 79.65
charMetrics 'Z' = CharMetrics 66.67 100.0 50.0 73.75
charMetrics '[' = CharMetrics 33.33 152.38 42.86 46.11
charMetrics '\\' = CharMetrics 66.67 114.29 42.86 78.21
charMetrics ']' = CharMetrics 33.33 152.38 42.86 46.39
charMetrics '^' = CharMetrics 76.19 66.67 76.19 90.23
charMetrics '_' = CharMetrics 104.76 4.76 (-30.95) 104.06
charMetrics '`' = CharMetrics 28.57 28.57 85.71 83.57
charMetrics 'a' = CharMetrics 57.14 66.67 33.33 66.6
charMetrics 'b' = CharMetrics 57.14 100.0 50.0 70.46
charMetrics 'c' = CharMetrics 57.14 66.67 33.33 68.92
charMetrics 'd' = CharMetrics 57.14 100.0 50.0 70.26
charMetrics 'e' = CharMetrics 57.14 66.67 33.33 68.52
charMetrics 'f' = CharMetrics 38.1 100.0 50.0 38.66
charMetrics 'g' = CharMetrics 57.14 100.0 16.67 70.98
charMetrics 'h' = CharMetrics 52.38 100.0 50.0 71.02
charMetrics 'i' = CharMetrics 9.52 104.76 52.38 28.86
charMetrics 'j' = CharMetrics 28.57 138.1 35.71 36.23
charMetrics 'k' = CharMetrics 52.38 100.0 50.0 62.52
charMetrics 'l' = CharMetrics 0.0 100.0 50.0 19.34
charMetrics 'm' = CharMetrics 104.76 66.67 33.33 123.96
charMetrics 'n' = CharMetrics 52.38 66.67 33.33 70.88
charMetrics 'o' = CharMetrics 61.9 66.67 33.33 71.74
charMetrics 'p' = CharMetrics 57.14 100.0 16.67 70.8
charMetrics 'q' = CharMetrics 57.14 100.0 16.67 70.74
charMetrics 'r' = CharMetrics 38.1 66.67 33.33 49.5
charMetrics 's' = CharMetrics 52.38 66.67 33.33 62.32
charMetrics 't' = CharMetrics 38.1 100.0 50.0 39.32
charMetrics 'u' = CharMetrics 52.38 66.67 33.33 71.16
charMetrics 'v' = CharMetrics 57.14 66.67 33.33 60.6
charMetrics 'w' = CharMetrics 76.19 66.67 33.33 80.49
charMetrics 'x' = CharMetrics 52.38 66.67 33.33 56.4
charMetrics 'y' = CharMetrics 61.9 100.0 16.67 66.06
charMetrics 'z' = CharMetrics 52.38 66.67 33.33 61.82
charMetrics '{' = CharMetrics 23.81 152.38 42.86 41.63
charMetrics '|' = CharMetrics 0.0 152.38 42.86 23.78
charMetrics '}' = CharMetrics 23.81 152.38 42.86 41.47
charMetrics '~' = CharMetrics 85.71 28.57 42.86 91.27
charMetrics _ = CharMetrics 50.0 100.0 50.0 70.0
