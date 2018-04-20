import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

bytestring = BC.pack "I'm a ByteString, not a [Char]"

bytes = B.unpack bytestring
chars = BC.unpack bytestring

main = do
    BC.putStrLn bytestring
    print $ head bytes
    print $ head chars
