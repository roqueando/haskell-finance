module RootLocalizator (bisect, f, newton, f'', fprime) where


type Start = Int
type End = Int
type Equation = (Float -> Float)
type Interval = Float
type Precision = Float

bisect :: Start -> End -> Equation -> Interval -> Interval -> Precision -> Float
bisect start end f' intervalA intervalB precision
   | start >= end = -1.0
   | otherwise = applyFunction
   where
      c = (intervalA + intervalB) / 2.0
      applyFunction
        | f' c == 0.0 || (intervalB - intervalA) / 2.0 < precision = c
        | signum (f' c) == signum (f' intervalA) = bisect (start + 1) end f' c intervalB precision
        | otherwise = bisect (start + 1) end f' intervalA c precision

f :: Equation
f x = x**2 - x - 6.0

newton :: Start -> End -> Equation -> Equation -> Float -> Float -> Float
newton start end f' derivative x0 tol
   | start >= end = -1.0
   | otherwise = applyFunction
   where
      d = derivative x0
      newtonX = x0 - f' x0 / d
      applyFunction
         | abs d < tol = -1.0
         | abs (newtonX - x0) < tol = newtonX
         | otherwise = newton (start + 1) end f' derivative newtonX tol

f'' :: Equation
f'' x = x**2 - 2.0

fprime :: Equation
fprime x = 2.0*x

