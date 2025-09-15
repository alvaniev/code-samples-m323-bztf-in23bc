-- Exercise - "Geomtrische Formen"
aCircle = ((3.5, 7.0), 12.0)

aRectangle = ((0.5, -1.7), (6.3, 7.9), 14.7)

aTriangle = ((-2.5, -1), (5.7, -0.5), (3.1, 4.4))

-- Exercise - "Tabelle (Relation)"
userTable = [(100, "Wirth", "Niklaus", "nw@inf.ethz.ch"), (101, "Booch", "Grady", "gbo@oonet.com"), (102, "Ritchie", "Dennis", "dr@bell.org")]

myDictionary = [(100, "nw@inf.ethz.ch"), (101, "gbo@oonet.com"), (102, "dr@bell.org")]

-- Exercise - "Summe Quadratzahlen"
squares = sum [x ^ 2 | x <- [1 .. 100]]

-- Exercise - "Summe Quadratzahlen"
coordPairs = [(x, y) | x <- [-10 .. 10], y <- [-10 .. 10], x /= y]

-- Exercise - "Quader"
rectCuboid = [(a, b, c, d) | d <- [100], a <- [1 .. d], b <- [1 .. a], c <- [1 .. b], a ^ 2 + b ^ 2 + c ^ 2 == d ^ 2]

-- Diese Variante wäre falsch, da identische Quader ausgegeben werden (nämlich 12 statt 2). Es würden zusätzlich alle Permutationen (= verschiedene Reihenfolgen)
-- von Länge, Breite und Höhe ausgegeben, die aber alle identischen Kantenverhältnisse haben und somit die gleiche Form repräsentieren.
rectCuboid' = [(a, b, c, d) | d <- [100], a <- [1 .. 100], b <- [1 .. 100], c <- [1 .. 100], a ^ 2 + b ^ 2 + c ^ 2 == d ^ 2]
