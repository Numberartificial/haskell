-- file: ch08/ElfMagic.hs
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC

--我们将检测一个文件是否是 ELF object 文件：这种文件类型几乎被所有现代类 Unix 系统作为可执行文件。

--这个简单的问题可以通过查看文件头部的四个字节解决，看他们是否匹配某个特定的字节序列。表示某种文件类型的字节序列通常被称为 魔法数 。

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  inf <- L.readFile path
  return $ hasElfMagic inf

geneFile :: FilePath -> IO ()
geneFile path= do
  writeFile path. unlines. map ((++ ".00") . show) $ [1..10]


highestClosingFrom :: FilePath -> IO ()
highestClosingFrom path = do
  contents <- L.readFile path
  print $ highestClosing contents

closing = readPrice . (!!0) .LC.split ','

readPrice :: LC.ByteString -> Maybe Int
readPrice str =
  case LC.readInt str of
    Nothing -> Nothing
    Just (yuan, rest) ->
      case LC.readInt $ L.tail rest of
        Nothing -> Nothing
        Just (cents, more) ->
          Just (yuan * 100 + cents)

highestClosing = maximum . (Nothing:) .map closing . LC.lines
