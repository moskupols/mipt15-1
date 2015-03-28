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

-- почтовый адрес
email = "alekseev@phystech.edu"
-- общий тип для возвращаемых вашими функциями значений, где первая часть кортежа это само значение функции, вторая - кол-во операций
type Result = (Float, Integer)

fTailor x = (1 + x ** 2) / 2.0 * atan x - x / 2.0 -- функция, которую раскладываем
delta = 1e-10
(n, a, b) = (20, 0.1, 0.6) -- интервал

tailor :: Float -> Result
tailor x =
    let cs = [(-1)^(i-1) * x^(2*i+1) / fromIntegral (4 * i^2 - 1) :: Float | i <- [1..]]
        good = takeWhile (\x -> abs x >= delta) cs
    in (sum good, toInteger (length good) )

tailorA :: Float -> Result
tailorA x =
    let minusX2 = (-x) * x
        tailorA' :: Float -> Result -> Result
        tailorA' px r = if abs deltaY < delta
            then (acc, i-1)
            else tailorA' nx (acc + deltaY, i+1)
                where (acc, i) = r
                      nx = px * minusX2
                      deltaY = nx / fromIntegral (4 * i^2 - 1) :: Float
    in tailorA' (-x) (0.0, 1)

printTailor = mapM_ putStrLn $
    map
        (\ x ->
            let ((firstRes, firstCou), (secondRes, secondCou)) = (tailor x, tailorA x)
            in show x ++ "\t" ++ show firstRes ++ "\t" ++ show firstCou ++ "\t" ++ show secondRes ++ "\t" ++ show secondCou ++ "\t" ++ show (fTailor x))
        [a, a + (b - a) / n .. b]

-- *** Вторая часть

fSolve = \x -> x -- функция, решение которой ищем

iter :: (Float -> Float) -> Float -> Float -> Result
iter f a b = (42, 0)

newton :: (Float -> Float) -> Float -> Float -> Result
newton f a b = (42, 0)

dichotomy =
    --для функций с аккумулятором удобно ставить его в начало
    let dichotomyA i f a b = (42, 0)
    in dichotomyA 0 -- чтобы воспользоваться каррированием

printSolve =
    mapM_ putStrLn $ map (\f -> show $ f fSolve a b) [iter, newton, dichotomy]

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
