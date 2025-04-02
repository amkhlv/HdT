module PdQ (Bookmark (..), Note (..), PdQ (..), getPdQ, savePdQ) where

import Data.Default
import Text.XML.HXT.Arrow.Pickle
import Text.XML.HXT.Core

data Bookmark = Bookmark
  { title :: String,
    bookmarkPage :: Int
  }
  deriving (Show, Eq)

instance XmlPickler Bookmark where
  xpickle = bookmarkPickler

bookmarkPickler :: PU Bookmark
bookmarkPickler =
  xpElem "bookmark" $
    xpWrap (uncurry Bookmark, \b -> (title b, bookmarkPage b)) $
      xpPair xpText (xpAttr "page" xpInt)

data Note = Note
  { note :: Maybe String,
    noteR :: Int,
    noteG :: Int,
    noteB :: Int,
    notePage :: Int,
    noteX :: Double,
    noteY :: Double
  }
  deriving (Show, Eq)

uncurry7 f (a, b, c, d, e, g, h) = f a b c d e g h

notePickler :: PU Note
notePickler =
  xpElem "note" $
    xpWrap (uncurry7 Note, \n -> (note n, noteR n, noteG n, noteB n, notePage n, noteX n, noteY n)) $
      xp7Tuple (xpOption xpText) (xpAttr "r" xpInt) (xpAttr "g" xpInt) (xpAttr "b" xpInt) (xpAttr "page" xpInt) (xpAttr "x" xpPrim) (xpAttr "y" xpPrim)

data PdQ = PdQ
  { summary :: Maybe [XmlTree],
    bookmarks :: Maybe [Bookmark],
    notes :: Maybe [Note]
  }
  deriving (Show, Eq)

instance Default PdQ where
  def = PdQ {summary = Nothing, bookmarks = Nothing, notes = Nothing}

instance XmlPickler PdQ where
  xpickle = pdqPickler

pdqPickler :: PU PdQ
pdqPickler =
  xpElem "root" $
    xpWrap (\(s, bs, ns) -> PdQ {summary = s, bookmarks = bs, notes = ns}, \q -> (summary q, bookmarks q, notes q)) $
      xpTriple (xpOption (xpElem "summary" xpTrees)) (xpOption (xpElem "bookmarks" (xpList xpickle))) (xpOption (xpElem "notes" (xpList notePickler)))

getPdQ :: String -> IO PdQ
getPdQ filename = do
  [pdq] <-
    runX
      (xunpickleDocument pdqPickler [withValidate no, withRemoveWS yes] filename)
  return pdq

savePdQ :: String -> PdQ -> IO ()
savePdQ filename pdq = do
  runX
    ( constA pdq
        >>> xpickleDocument pdqPickler [withIndent yes] filename
    )
  return ()
