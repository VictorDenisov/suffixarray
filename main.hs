import Foreign
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array

import System.IO.Unsafe
import qualified Data.ByteString.Char8 as C
import Data.Array.IArray
import Data.ByteString.Internal
import Data.Bits    (shiftL)


main = do
       s <- C.getLine
       putStrLn $ show $ sortIndexes s

doMore s = unsafePerformIO $ unsafeDoMore s

unsafeDoMore s = do
        p <- mallocArray n
        unsafeSortIndexes s p
        cs <- mallocArray n
        populateClasses s p cs
        peekArray n cs
    where
        n = (C.length s)

populateClasses :: C.ByteString -> Ptr Int -> Ptr Int -> IO()
populateClasses (PS input str l) p c = withForeignPtr input (\s -> go s 1 0)
        where
                go s classes i | i == l = return ()
                go s classes i = do
                          pi <- peekElemOff p i
                          pim1 <- peekElemOff p (i - 1)
                          spi <- peekElemOff s pi
                          spim1 <- peekElemOff s pim1
                          let newClasses = if spi == spim1 then classes else (classes + 1)
                          pokeElemOff c pi (newClasses - 1)
                          go s newClasses (i + 1)

sortIndexes :: C.ByteString -> [Int]
sortIndexes s = unsafeCreateArray (C.length s) (unsafeSortIndexes s)

unsafeSortIndexes :: C.ByteString -> Ptr Int -> IO()
unsafeSortIndexes (PS input s l) ans = allocaArray 256 $ \arr -> do

                _ <- memset (castPtr arr) 0 (256 * fromIntegral (sizeOf (undefined :: Int)))
                withForeignPtr input (\x -> countOccurrences arr (x `plusPtr` s) l)
                producePartialSums arr 1
                
                let go str i | i == l = return ()
                    go str i          = do
                            char     <- fromIntegral `fmap` peekElemOff str i
                            position <- peekElemOff arr (char - 1)
                            pokeElemOff ans position i
                            pokeElemOff arr (char - 1) (position + 1)
                            go str (i + 1)

                withForeignPtr input (\str -> go str 0)
          where
            producePartialSums arr i | i == 256 = return ()
            producePartialSums arr i = do
                                prev <- peekElemOff arr (i - 1)
                                cur  <- peekElemOff arr (i)
                                pokeElemOff arr i (cur + prev)
                                producePartialSums arr (i + 1)

-- count occurences in string Ptr Word8. Store to Ptr Int
countOccurrences :: Ptr Int -> Ptr Word8 -> Int -> IO ()
countOccurrences a b c | a `seq` b `seq` c `seq` False = undefined
countOccurrences counts str len = go 0
    where
        go a | a `seq` False = undefined
        go i | i == len    = return ()
             | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                              x <- peekElemOff counts k
                              pokeElemOff counts k (x + 1)
                              go (i + 1)
                

unsafeCreateArray :: Int -> (Ptr Int -> IO ()) -> [Int]
unsafeCreateArray n f = unsafePerformIO $ createArray n f

createArray :: Int -> (Ptr Int -> IO ()) -> IO([Int])
createArray n f = do
        a <- mallocArray n
        f a
        peekArray n a

