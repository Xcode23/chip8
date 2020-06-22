module Utils where

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as VM
import Control.Monad.ST

writeVectorToVector :: (V.Unbox a, Integral b) => b -> V.Vector a -> V.Vector a-> V.Vector a
writeVectorToVector start newValues vector = runST $ do
  mutableValues <- V.unsafeThaw newValues
  mutableVector <- V.unsafeThaw vector
  let mutableSlice = VM.slice (fromIntegral start) (VM.length mutableValues) mutableVector
  VM.unsafeCopy mutableSlice mutableValues
  V.unsafeFreeze mutableVector

writeValueToVector :: (V.Unbox a, Integral b) => b -> a -> V.Vector a -> V.Vector a
writeValueToVector index value vector = runST $ do
  mutableVector <- V.unsafeThaw vector
  VM.write mutableVector (fromIntegral index) value
  V.unsafeFreeze mutableVector

readFromVector :: (V.Unbox a, Integral b) => b -> V.Vector a -> a
readFromVector index vector = vector V.! fromIntegral index

readVectorFromVector :: (V.Unbox a, Integral b, Integral c) => b -> c -> V.Vector a -> V.Vector a
readVectorFromVector index size = V.slice (fromIntegral index) $ fromIntegral size

modifyValueInVector :: (V.Unbox a, Integral b) => (a -> a) -> b -> V.Vector a -> V.Vector a
modifyValueInVector func index vector = runST $ do
  mutableVector <- V.unsafeThaw vector
  VM.unsafeModify mutableVector func $ fromIntegral index
  V.unsafeFreeze mutableVector

modifyListInVector :: V.Unbox a => (a -> a -> a) -> [(Int,a)] -> V.Vector a -> V.Vector a
modifyListInVector func list vector = runST $ do
  mutableVector <- V.unsafeThaw vector
  auxModifyListInVector func list mutableVector
  V.unsafeFreeze mutableVector

auxModifyListInVector :: V.Unbox a => (a -> a -> a) -> [(Int,a)] -> VM.MVector s a -> ST s ()
auxModifyListInVector _ [] _ = return ()
auxModifyListInVector func ((index, value) : next) vector = do
  VM.unsafeModify vector (func value) $ fromIntegral index
  auxModifyListInVector func next vector
