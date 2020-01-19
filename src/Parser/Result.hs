{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Result where
import Common(BytesLen)

data Data a =
  Parsed a
  | DataIsTooShort (Maybe BytesLen)-- 一致するデータがあったが、元データが不足している（続きのデータを追加して再実行すればうまくいくと思われる）。値は不足しているバイト数
  | NotMatch       -- 一致するデータが存在しなかった
  | SumCheckError  -- 合計値エラー。データに何らかの欠損があると思われる 
  | UnknownReason  -- 原因不明の失敗

map :: (a -> b) -> Data a -> Data b
map f x = case x of
  Parsed x         -> Parsed $ f x
  DataIsTooShort i -> DataIsTooShort i
  NotMatch         -> NotMatch
  SumCheckError    -> SumCheckError
  UnknownReason    -> UnknownReason
