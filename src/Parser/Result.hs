{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Result where
import Common(BytesLen)

data (Show a) => Data a =
  Parsed a
  | DataIsTooShort (Maybe BytesLen)-- 一致するデータがあったが、元データが不足している（続きのデータを追加して再実行すればうまくいくと思われる）。値は不足しているバイト数
  | NotMatch       -- 一致するデータが存在しなかった
  | SumCheckError  -- 合計値エラー。データに何らかの欠損があると思われる
  | NotSupported   -- サポートしていない
  | UnknownReason  -- 原因不明の失敗
  deriving (Show)

map :: (Show a, Show b) => (a -> b) -> Data a -> Data b
map f x = case x of
  Parsed x         -> Parsed $ f x
  DataIsTooShort i -> DataIsTooShort i
  NotMatch         -> NotMatch
  SumCheckError    -> SumCheckError
  NotSupported     -> NotSupported
  UnknownReason    -> UnknownReason

(>>==) :: (Show a, Show c) => (Data a, b) -> (a -> b -> (Data c,b)) -> (Data c,b)
(>>==) (Parsed x        ,y) f = f x y
(>>==) (DataIsTooShort i,y) f = (DataIsTooShort i,y)
(>>==) (NotMatch        ,y) f = (NotMatch        ,y)
(>>==) (SumCheckError   ,y) f = (SumCheckError   ,y)
(>>==) (UnknownReason   ,y) f = (UnknownReason   ,y)

(>>===) :: (Show a, Show c) => IO (Data a, b) -> ((a,b) -> IO (Data c,b)) -> IO (Data c,b)
(>>===) io f = do
  res <- io
  case res of
    (Parsed x        ,y) -> f (x,y)
    (DataIsTooShort i,y) -> return (DataIsTooShort i,y)
    (NotMatch        ,y) -> return (NotMatch        ,y)
    (SumCheckError   ,y) -> return (SumCheckError   ,y)
    (UnknownReason   ,y) -> return (UnknownReason   ,y)
    
infixl 3 >>===
