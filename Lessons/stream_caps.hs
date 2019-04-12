import qualified Data.ByteString.Char8 as S
import qualified System.IO.Streams as Streams
import Data.Char (toUpper)
import System.Environment (getArgs)
import System.IO (IOMode(ReadMode), withFile)

main = do
    [file_in, file_out] <- getArgs
    withFile file_in ReadMode $ \handle -> do
        input_stream  <- Streams.handleToInputStream handle
        output_stream <- Streams.makeOutputStream
            $ \maybe_bs -> case maybe_bs of
                Nothing -> return ()
                Just bs -> S.writeFile file_out
                    (S.pack $ map toUpper (S.unpack bs))
        Streams.connect input_stream output_stream