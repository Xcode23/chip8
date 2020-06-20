module Utils where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

writeVectorToVector :: (V.Unbox a, Integral b) => b -> V.Vector a -> V.Vector a-> V.Vector a
writeVectorToVector start newValues vector = runST $ do
  mutableValues <- V.thaw newValues
  mutableVector <- V.thaw vector
  let mutableSlice = VM.slice (fromIntegral start) (VM.length mutableValues) mutableVector
  VM.copy mutableSlice mutableValues
  V.freeze mutableVector

writeValueToVector :: (V.Unbox a, Integral b) => b -> a -> V.Vector a -> V.Vector a
writeValueToVector index value vector = runST $ do
  mutableVector <- V.thaw vector
  VM.write mutableVector (fromIntegral index) value
  V.freeze mutableVector

readFromVector :: (V.Unbox a, Integral b) => b -> V.Vector a -> a
readFromVector index vector = vector V.! fromIntegral index

readVectorFromVector :: (V.Unbox a, Integral b, Integral c) => b -> c -> V.Vector a -> V.Vector a
readVectorFromVector index size = V.slice (fromIntegral index) $ fromIntegral size

modifyValueInVector :: (V.Unbox a, Integral b) => (a -> a) -> b -> V.Vector a -> V.Vector a
modifyValueInVector func index vector = runST $ do
  mutableVector <- V.thaw vector
  VM.modify mutableVector func $ fromIntegral index
  V.freeze mutableVector