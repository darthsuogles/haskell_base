-- Solve 1D Poisson Equation
module SolvePDE
       where
      
phi k i | (k == 0) = 0
        | (i < 0 || i >= nsites) = 0       
        | otherwise = (phi (k-1) (i-1) + phi (k-1) (i+1)) / 2 + h/2 * (rho i)

rho i | i == nsites `div` 2 = 1
      | otherwise = 0

h = 0.1
nsites = 10