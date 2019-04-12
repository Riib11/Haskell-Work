import qualified Data.ByteString.Char8 as S
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as Streams
import System.IO
import Data.Char (toUpper)

cat :: FilePath -> IO ()
-- withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
cat file = withFile file ReadMode $ \h -> do
    -- input_stream :: IO (Streams.OutputStream S.ByteString)
    is <- Streams.handleToInputStream h
    -- Streams.connect :: InputStream a -> Streams.OutputStream a -> IO ()
    Streams.connect is Streams.stdout

copy_file :: FilePath -> FilePath -> IO ()
copy_file file_in file_out = withFile file_in ReadMode
    $ \handle -> do
        input_stream  <- Streams.handleToInputStream handle
        output_stream <- Streams.makeOutputStream
            $ \maybe_bs -> case maybe_bs of
                Just bs -> S.writeFile file_out bs
                Nothing -> return ()
        Streams.connect input_stream output_stream