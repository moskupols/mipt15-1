{-# LANGUAGE OverloadedStrings #-}

module Lab1 (fTailor, tailor, tailorA, fSolve, iter, newton, dichotomy) where

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Network (withSocketsDo)

import Data.List
import Data.Tuple
import Data.Ord

-- почтовый адрес
email = "alekseev@phystech.edu"
-- общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = (Float, Integer)

fTailor x = (1 + x ** 2) / 2 * atan x - x / 2 -- функция, которую раскладываем
delta = 1e-10
(n, a, b) = (20, 0.1, 0.6) -- интервал

tailor :: Float -> Result
tailor x =
    let nums   = [(-1)^(i-1) * x^(2*i+1) | i <- [1..]]
        denoms = [fromIntegral (4 * i^2 - 1) :: Float | i <- [1..]]
        cs    = zipWith (/) nums denoms
        good  = takeWhile (\x -> abs x >= delta) cs
    in (sum good, toInteger (length good) )

tailorA :: Float -> Result
tailorA x =
    let multiplier = (-x) * x
        nums   = iterate (* multiplier) (x^3)
        denoms = [fromIntegral (4 * i^2 - 1) :: Float | i <- [1..]]
        cs    = zipWith (/) nums denoms
        good  = takeWhile (\x -> abs x >= delta) cs
    in (sum good, toInteger (length good) )

printTailor = mapM_ putStrLn $
    map
        (\ x ->
            let ((firstRes, firstCou), (secondRes, secondCou)) = (tailor x, tailorA x)
            in show x ++ "\t" ++ show firstRes ++ "\t" ++ show firstCou ++ "\t" ++ show secondRes ++ "\t" ++ show secondCou ++ "\t" ++ show (fTailor x))
        [a, a + (b - a) / n .. b]

-- *** Вторая часть

fSolve x = x + sqrt x + x ** (1/3) - 2.5 -- функция, решение которой ищем
(aSolve, bSolve) = (0.4, 1.0)

iter :: (Float -> Float) -> Float -> Float -> Result
iter f a b =
    let iters = 300
        xs = [a, a+(b-a)/(fromIntegral iters :: Float) .. b]
        ys = map (abs . f) xs
    in (snd $ minimumBy (comparing fst) (zip ys xs), iters)

newton :: (Float -> Float) -> Float -> Float -> Result
newton f a b =
    let f' x = 1 + x**(-0.5) + x**(-2/3) -- неясно, как здесь следовало поступить:
                                         -- (f (x+delta) - f x) / delta не даёт достаточной точности.
        newtonA x i
            | abs dx < delta = (x, i)
            | otherwise  = newtonA nx (i+1)
            where y = f x
                  dx = y / f' x
                  nx = x - dx
    in newtonA ((a+b)/2) 0

dichotomy =
    --для функций с аккумулятором удобно ставить его в начало
    let dichotomyA i f a b
            | my > delta    = dichotomyA (i+1) f a mx
            | my < (-delta) = dichotomyA (i+1) f mx b
            | otherwise     = (mx, i)
            where mx = (a + b) / 2.0
                  my = f mx
    in dichotomyA 0 -- чтобы воспользоваться каррированием

printSolve =
    mapM_ putStrLn $ map (\f -> show $ f fSolve aSolve bSolve) [iter, newton, dichotomy]

main = do
    withSocketsDo $ do
    dir <- getCurrentDirectory
    initReq <- parseUrl "http://mipt.eu01.aws.af.cm/lab1"
    handle <- openFile (dir ++ "/lab1.hs") ReadMode
    hSetEncoding handle utf8_bom
    content <- hGetContents handle
    let req = urlEncodedBody [("email", email), ("content", C.pack content)] $ initReq { method = "POST" }
    response <- withManager $ httpLbs req
    hClose handle
    L.putStrLn $ responseBody response
