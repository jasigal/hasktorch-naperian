{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}

module Main where

import           Naperian
import           Torch.Naperian
import           Data.Naperian.Examples
import qualified Torch.Tensor                  as D
import qualified Torch.Autograd                as A
import qualified Torch.DType                   as TDT
import           Torch.Static                  as S

main :: IO ()
main = do
    putStrLn "--- Dim fold ---"
    let dim :: Dim '[2] '[Vector 3] TDT.Float
        dim = Dim (pure ones)
        summed :: Dim '[2] '[] TDT.Float
        summed = foldrDimLayer add ones dim
    print dim
    print summed
    putStrLn "--- Tensor unbind ---"
    let test1 = ones :: S.Tensor TDT.Float '[3,2]
        test1Dyn = toDynamic test1
    print test1Dyn
    print (unbindDynamic test1Dyn)
    print (unbind test1)
    putStrLn "--- Dim unbind ---"
    print (dimUp dim :: Dim '[] '[Vector 2, Vector 3] TDT.Float)
    putStrLn "--- Tensor stack ---"
    let test2 = ones :: S.Tensor TDT.Float '[2]
        test2Dyn = toDynamic test2
    print test2Dyn
    print (stackDynamic [test2Dyn, test2Dyn, test2Dyn])
    print (stack [test2, test2, test2] :: S.Tensor TDT.Float '[3, 2])
    putStrLn "--- Dim stack ---"
    print (dimDown dim :: Dim '[3, 2] '[] TDT.Float)
    putStrLn "--- Dim cat ---"
    print (dimCat dim)
