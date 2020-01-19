module BytesReader.Counter where
import Common(ByteString,BytesLen)
class Class a where
  getBytesCounter   :: a -> BytesLen
  resetBytesCounter :: a -> a

