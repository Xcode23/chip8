module Utils where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

writeVectorToVector :: (V.Unbox a, Integral b) => V.Vector a -> b -> V.Vector a -> V.Vector a
writeVectorToVector vector start newValues = runST $ do
  mutableValues <- V.thaw newValues
  mutableVector <- V.thaw vector
  let mutableSlice = VM.slice (fromIntegral start) (VM.length mutableValues) mutableVector
  VM.copy mutableSlice mutableValues
  V.freeze mutableVector

writeValueToVector :: (V.Unbox a, Integral b) => V.Vector a -> b -> a -> V.Vector a
writeValueToVector vector index value = runST $ do
  mutableVector <- V.thaw vector
  VM.write mutableVector (fromIntegral index) value
  V.freeze mutableVector

readFromVector :: (V.Unbox a, Integral b) => V.Vector a -> b -> a
readFromVector vector index = vector V.! fromIntegral index

modifyValueInVector :: (V.Unbox a, Integral b) => V.Vector a -> (a -> a) -> b -> V.Vector a
modifyValueInVector vector func index = runST $ do
  mutableVector <- V.thaw vector
  VM.modify mutableVector func $ fromIntegral index
  V.freeze mutableVector