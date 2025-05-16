{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Config
import Control.Monad (join, unless, void, when, (>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.GI.Base
import Data.GI.Base.GError
import qualified Data.GI.Base.GValue as GV
import Data.IORef
import Data.List (find)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import GHC.Int (Int32)
import qualified GI.Cairo.Render as CR
import qualified GI.Cairo.Render.Connector as CRC
import qualified GI.Cairo.Structs as CStructs
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Constants as GdkConst
import qualified GI.Gdk.Flags as GdkFlags
import qualified GI.Gdk.Objects.Clipboard as CB
import qualified GI.Gdk.Objects.Display as GD
import qualified GI.Gdk.Structs.Rectangle as GdkRect
import qualified GI.Gio.Callbacks as GCallbacks
import qualified GI.Gio.Interfaces.File as GFile
import GI.Gio.Objects.Cancellable
import qualified GI.Gio.Objects.Task as GTask
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Constants as GtkConst
import qualified GI.Gtk.Objects.CssProvider as GtkProvider
import qualified GI.Gtk.Objects.EntryBuffer as GtkBuf
import qualified GI.Gtk.Objects.Label as GtkLbl
import qualified GI.Gtk.Objects.Popover as GtkPop
import qualified GI.Gtk.Objects.StyleContext as GtkStyleContext
import qualified GI.Gtk.Objects.TextView as GtkTV
import qualified GI.Gtk.Objects.Tooltip as GtkTooltip
import qualified GI.Gtk.Objects.Window as GtkWin
import qualified GI.Poppler.Enums as PopEnums
import qualified GI.Poppler.Objects.Document as PopDoc
import qualified GI.Poppler.Objects.Page as PopPage
import qualified GI.Poppler.Structs.ActionGotoDest as AGD
import qualified GI.Poppler.Structs.ActionGotoRemote as AGR
import qualified GI.Poppler.Structs.Dest as Dest
import qualified GI.Poppler.Structs.LinkMapping as LinkMap
import qualified GI.Poppler.Structs.Rectangle as Rect
import qualified GI.Poppler.Unions.Action as Act
import qualified GI.Rsvg.Enums as RsvgEnums
import qualified GI.Rsvg.Objects.Handle as RsvgH
import qualified GI.Rsvg.Structs.Rectangle as RsvgRect
import Numeric (showHex)
import qualified Options.Applicative as Opt
import PdQ
import PrepSVG
import System.Directory
import Text.Printf (printf)
import Text.Read (readMaybe)
import Utils

data Options = Options
  { extractPage :: Int,
    pdfFile :: String,
    openPage :: Int
  }

optParser :: Opt.Parser Options
optParser =
  Options
    <$> Opt.option Opt.auto (Opt.long "extract-page" <> Opt.short 'e' <> Opt.help "extract page in SVG" <> Opt.value 0)
    <*> Opt.strArgument (Opt.metavar "PDF_FILE" <> Opt.value "")
    <*> Opt.option Opt.auto (Opt.long "page" <> Opt.short 'p' <> Opt.help "open page" <> Opt.value 1)

getGFile :: Options -> IO GFile.File
getGFile clops = GFile.fileNewForPath $ pdfFile clops

getDoc :: Options -> IO PopDoc.Document
getDoc clops = do
  pdfGFile <- getGFile clops
  PopDoc.documentNewFromGfile pdfGFile Nothing (Nothing :: Maybe Cancellable)

refresh' :: Gtk.DrawingArea -> IORef AppState -> CStructs.Context -> IO ()
refresh' da1 state ctxt = do
  st <- readIORef state
  let sc = scale st
  let doc = document st
  let conf = config st
  pg <- PopDoc.documentGetPage doc (head $ pages st)
  (pw, ph) <- PopPage.pageGetSize pg
  Gtk.drawingAreaSetContentWidth da1 $ round (sc * pw)
  Gtk.setWidgetWidthRequest da1 $ round (sc * pw)
  Gtk.drawingAreaSetContentHeight da1 $ round (sc * ph)
  Gtk.setWidgetHeightRequest da1 $ round (sc * ph)
  PopPage.pageRender pg ctxt
  let overlayFile = dDir st ++ "/p" ++ show (1 + head (pages st)) ++ ".svg"
  existsOverlay <- doesFileExist overlayFile
  when existsOverlay $ do
    mhandle <- RsvgH.handleNewFromFile $ T.pack overlayFile
    rect <- RsvgRect.newZeroRectangle
    RsvgRect.setRectangleHeight rect ph
    RsvgRect.setRectangleWidth rect pw
    let f :: RsvgEnums.Error -> T.Text -> IO ()
        f err msg = putStrLn $ "Error (most likely layer with id >>>" ++ overlayLayerID conf ++ "<<< not found): " ++ show err ++ " " ++ show msg
     in handleGErrorJustDomain f $
          mapM_
            (\handle -> RsvgH.handleRenderLayer handle ctxt (Just $ T.pack $ '#' : overlayLayerID conf) rect)
            mhandle

textExtract :: Gtk.ApplicationWindow -> Config -> IORef AppState -> IO ()
textExtract win conf state = do
  st <- readIORef state
  page <- PopDoc.documentGetPage (document st) (head $ pages st)
  text <- PopPage.pageGetText page
  view <- new Gtk.TextView []
  buffer <- GtkTV.textViewGetBuffer view
  Gtk.textBufferSetText buffer text (fromIntegral $ length (T.unpack text))
  popup <- new Gtk.Window [#transientFor := win, #child := view, #destroyWithParent := True, #modal := True, #title := "Text Extracted"]
  controllerKeyPress <-
    new
      Gtk.EventControllerKey
      [ On #keyPressed $ \x _ _mdfr -> do
          case x of
            Gdk.KEY_Escape -> GtkWin.windowDestroy popup
            _ -> return ()
          return True
      ]
  Gtk.widgetAddController popup controllerKeyPress
  Gtk.windowSetDefaultSize popup (fromIntegral $ (4 * windowWidth conf) `div` 5) (fromIntegral $ (4 * windowHeight conf) `div` 5)
  Gtk.widgetSetVisible view True
  Gtk.widgetSetVisible popup True

copyToClipboard :: String -> IO ()
copyToClipboard txt = do
  mdisplay <- GD.displayGetDefault
  case mdisplay of
    Just display -> do
      clipboard <- GD.displayGetClipboard display
      gtxt <- GV.toGValue $ Just txt
      CB.clipboardSet clipboard gtxt
    Nothing -> putStrLn "=== No display"

data ToUpdate = ToUpdate
  { da :: Gtk.DrawingArea,
    lPage :: Gtk.Label,
    lZoom :: Gtk.Label
  }

newBookmarkDialog :: Gtk.ApplicationWindow -> ToUpdate -> IORef AppState -> IO ()
newBookmarkDialog win tu state = do
  st <- readIORef state
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "New Bookmark"]
  entry <-
    new
      Gtk.Entry
      [ #placeholderText := "bookmark title",
        On #activate $ do
          putStrLn "activate"
          buf <- Gtk.entryGetBuffer ?self
          txt <- T.unpack <$> GtkBuf.entryBufferGetText buf
          let oldPdQ = pdq st
          let newPdQ =
                oldPdQ
                  { bookmarks =
                      Just $
                        Bookmark
                          { title = txt,
                            bookmarkPage = fromIntegral $ head (pages st)
                          }
                          : fromMaybe [] (bookmarks $ pdq st)
                  }
          savePdQ (pdqFile st) newPdQ
          let newSt =
                st
                  { pdq = newPdQ
                  }
          writeIORef state newSt
          GtkWin.windowDestroy dialog

          return ()
      ]
  GtkWin.windowSetChild dialog (Just entry)
  controllerKeyPress <-
    new
      Gtk.EventControllerKey
      [ On #keyPressed $ \x _ _mdfr -> do
          st <- readIORef state
          case x of
            Gdk.KEY_Escape -> GtkWin.windowDestroy dialog
            _ -> return ()
          return True
      ]
  Gtk.widgetAddController dialog controllerKeyPress
  Gtk.widgetSetVisible entry True
  Gtk.widgetGrabFocus entry
  Gtk.widgetSetVisible dialog True

gotoBookmarkDialog :: Gtk.ApplicationWindow -> ToUpdate -> IORef AppState -> IO ()
gotoBookmarkDialog win tu state = do
  st <- readIORef state
  let doc = document st
  let conf = config st
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "Bookmarks"]
  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  let mbms = bookmarks $ pdq st
  case mbms of
    Just bms ->
      sequence_
        [ do
            lbl <- new Gtk.Label [#label := T.pack $ i : ':' : ' ' : title bm]
            vbox.append lbl
          | (i, bm) <- zip ['a' .. 'z'] bms
        ]
    Nothing -> return ()
  controllerKeyPress <-
    new
      Gtk.EventControllerKey
      [ On #keyPressed $ \x _ _mdfr -> do
          st <- readIORef state
          print x
          case x of
            Gdk.KEY_Escape -> GtkWin.windowDestroy dialog
            y -> do
              sequence_
                [ when (y == i) $ do
                    let oldPdQ = pdq st
                    let newPdQ = oldPdQ {bookmarks = filter (/= bm) <$> bookmarks (pdq st)}
                    writeIORef state $ st {pdq = newPdQ}
                    savePdQ (pdqFile st) newPdQ
                    GtkWin.windowDestroy dialog
                  | (i, bm) <- zip [65 ..] (fromMaybe [] $ bookmarks $ pdq st)
                ]
              sequence_
                [ when (y == i) $ do
                    writeIORef state $ st {pages = fromIntegral (bookmarkPage bm) : pages st}
                    readIORef state >>= updateUI tu
                    GtkWin.windowDestroy dialog
                  | (i, bm) <- zip [97 ..] (fromMaybe [] $ bookmarks $ pdq st)
                ]
          return True
      ]
  GtkWin.windowSetChild dialog (Just vbox)
  Gtk.widgetAddController dialog controllerKeyPress
  Gtk.widgetSetVisible vbox True
  Gtk.widgetSetVisible dialog True

gotoPageDialog :: Gtk.ApplicationWindow -> ToUpdate -> IORef AppState -> IO ()
gotoPageDialog win tu state = do
  st <- readIORef state
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "Go to page:"]
  entry <-
    new
      Gtk.Entry
      [ #placeholderText := "page number",
        On #activate $ do
          putStrLn "activate"
          buf <- Gtk.entryGetBuffer ?self
          txt <- T.unpack <$> GtkBuf.entryBufferGetText buf
          case readMaybe txt of
            Just num -> when (num > 0 && num <= fromIntegral (totalPages st)) $ do
              let newSt = st {pages = (num - 1) : pages st}
              writeIORef state newSt
              GtkWin.windowDestroy dialog
              readIORef state >>= updateUI tu
            Nothing -> return ()

          return ()
      ]
  controllerKeyPress <-
    new
      Gtk.EventControllerKey
      [ On #keyPressed $ \x _ _mdfr -> do
          case x of
            Gdk.KEY_Escape -> GtkWin.windowDestroy dialog
            _ -> return ()
          return True
      ]
  Gtk.widgetAddController dialog controllerKeyPress
  GtkWin.windowSetChild dialog (Just entry)
  Gtk.widgetSetVisible entry True
  Gtk.widgetSetVisible dialog True

  return ()

dashboard :: Gtk.ApplicationWindow -> ToUpdate -> IORef AppState -> IO ()
dashboard win tu state =
  do
    st <- readIORef state
    vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
    panel <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "Notes:", #child := vbox]
    sequence_
      [ let notesOnPage = filter (\nt -> notePage nt == fromIntegral p - 1) $ fromMaybe [] (notes $ pdq st)
         in unless (null notesOnPage) $ do
              hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
              sequence_
                [ do
                    lbl0 <- new Gtk.Label []
                    Gtk.labelSetMarkup
                      lbl0
                      (T.pack $ printf "<span foreground=\"#%06XFF\">█</span>" (256 * 256 * noteR nt + 256 * noteG nt + noteB nt))
                    btn <-
                      new
                        Gtk.Button
                        [ #child := lbl0,
                          On #clicked $ do
                            let newSt = st {pages = fromIntegral (notePage nt) : pages st}
                            writeIORef state newSt
                            readIORef state >>= updateUI tu
                            print nt
                        ]
                    Gtk.widgetSetTooltipText btn (T.pack <$> note nt)
                    hbox.append btn
                  | nt <- notesOnPage
                ]
              vbox.append hbox
        | p <- [1 .. totalPages st]
      ]
    okBtn <-
      new
        Gtk.Button
        [ #label := "OK",
          On #clicked $ do
            GtkWin.windowDestroy panel
        ]
    vbox.append okBtn
    Gtk.widgetSetVisible panel True

noteDialog :: Gtk.ApplicationWindow -> Double -> Double -> Maybe Note -> ToUpdate -> IORef AppState -> IO ()
noteDialog win x y oldnote tu state = do
  st <- readIORef state
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "Note:"]
  -- Gtk.windowSetDefaultSize dialog 400 300
  entry <- new Gtk.TextView []
  Gtk.widgetSetSizeRequest entry 300 200
  buffer <- GtkTV.textViewGetBuffer entry
  let onote = if isJust (movingNote st) then movingNote st else oldnote
  mapM_ (\text -> Gtk.textBufferSetText buffer (T.pack text) (fromIntegral $ length text)) (onote >>= note)
  vbox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 6]
  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  hboxColors <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal]
  -- rgba0 <- new Gdk.RGBA [#red := 1.0, #green := 1.0]
  -- colorDialog <- new Gtk.ColorDialog [#modal := True]
  -- colorButton <- new Gtk.ColorDialogButton [#rgba := rgba0, #dialog := colorDialog]
  noteColor <- newIORef $ case onote of
    Just nt -> Clr {r = fromIntegral $ noteR nt, g = fromIntegral $ noteG nt, b = fromIntegral $ noteB nt}
    Nothing -> defaultMarkerColor $ config st
  let colors = markerColors (config st)
  unless (null colors) $ do
    lbl0 <- new Gtk.Label []
    clrBtn0 <-
      new
        Gtk.ToggleButton
        [ #child := lbl0,
          On #toggled $ do
            writeIORef noteColor $ head colors
        ]
    Gtk.labelSetMarkup
      lbl0
      (T.pack $ printf "<span foreground=\"#%06XFF\">█</span>" (256 * 256 * (r $ head colors) + 256 * (g $ head colors) + (b $ head colors)))
    print (T.pack $ printf "<span foreground=\"#%06XFF\">█</span>" (256 * 256 * (r $ head colors) + 256 * (g $ head colors) + (b $ head colors)))
    hboxColors.append clrBtn0
    sequence_
      [ do
          lbl1 <- new Gtk.Label []
          clrBtn1 <-
            new
              Gtk.ToggleButton
              [ #child := lbl1,
                On #toggled $ do
                  writeIORef noteColor c
              ]
          Gtk.labelSetMarkup
            lbl1
            (T.pack $ printf "<span foreground=\"#%06XFF\">█</span>" (256 * 256 * r c + 256 * g c + b c))
          Gtk.toggleButtonSetGroup clrBtn1 (Just clrBtn0)
          hboxColors.append clrBtn1
        | c <- tail colors
      ]

  okButton <-
    new
      Gtk.Button
      [ #label := "OK",
        On #clicked $ do
          startIter <- Gtk.textBufferGetStartIter buffer
          endIter <- Gtk.textBufferGetEndIter buffer
          text <- Gtk.textBufferGetText buffer startIter endIter False
          c <- readIORef noteColor

          let newNote =
                maybe
                  ( Note
                      { notePage = fromIntegral $ head (pages st),
                        noteX = x,
                        noteY = y,
                        noteR = fromIntegral $ r c,
                        noteG = fromIntegral $ g c,
                        noteB = fromIntegral $ b c,
                        note = Just (T.unpack text)
                      }
                  )
                  ( \nt ->
                      nt
                        { note = Just (T.unpack text),
                          noteR = fromIntegral $ r c,
                          noteG = fromIntegral $ g c,
                          noteB = fromIntegral $ b c
                        }
                  )
                  oldnote
          let newPdQ =
                (pdq st)
                  { notes =
                      Just
                        ( newNote
                            : maybe
                              []
                              ( filter
                                  ( \nt -> case onote of
                                      Just nt0 -> nt /= nt0
                                      Nothing -> True
                                  )
                              )
                              (notes $ pdq st)
                        )
                  }
          GtkWin.windowDestroy dialog
          writeIORef state $ st {pdq = newPdQ, movingNote = Nothing}
          readIORef state >>= updateUI tu
          savePdQ (pdqFile st) newPdQ
      ]
  hbox.append okButton
  moveButton <-
    new
      Gtk.Button
      [ #label := "move",
        On #clicked $ do
          startIter <- Gtk.textBufferGetStartIter buffer
          endIter <- Gtk.textBufferGetEndIter buffer
          text <- Gtk.textBufferGetText buffer startIter endIter False
          c <- readIORef noteColor

          let newNote =
                maybe
                  ( Note
                      { notePage = fromIntegral $ head (pages st),
                        noteX = x,
                        noteY = y,
                        noteR = fromIntegral $ r c,
                        noteG = fromIntegral $ g c,
                        noteB = fromIntegral $ b c,
                        note = Just (T.unpack text)
                      }
                  )
                  (\nt -> nt {note = Just (T.unpack text)})
                  oldnote
          let newPdQ =
                (pdq st)
                  { notes =
                      filter
                        ( \nt -> case oldnote of
                            Just nt0 -> nt /= nt0
                            Nothing -> True
                        )
                        <$> notes (pdq st)
                  }
          GtkWin.windowDestroy dialog
          writeIORef state $ st {pdq = newPdQ, movingNote = Just newNote}
          readIORef state >>= updateUI tu
          savePdQ (pdqFile st) newPdQ
      ]
  when (isJust oldnote) $ hbox.append moveButton
  deleteButton <-
    new
      Gtk.Button
      [ #label := "delete",
        On #clicked $ do
          startIter <- Gtk.textBufferGetStartIter buffer
          endIter <- Gtk.textBufferGetEndIter buffer
          text <- Gtk.textBufferGetText buffer startIter endIter False
          c <- readIORef noteColor

          let newPdQ =
                (pdq st)
                  { notes =
                      filter
                        ( \nt -> case oldnote of
                            Just nt0 -> nt /= nt0
                            Nothing -> True
                        )
                        <$> notes (pdq st)
                  }
          GtkWin.windowDestroy dialog
          writeIORef state $ st {pdq = newPdQ, movingNote = Nothing}
          readIORef state >>= updateUI tu
          savePdQ (pdqFile st) newPdQ
      ]
  when (isJust oldnote) $ hbox.append deleteButton
  cancelButton <-
    new
      Gtk.Button
      [ #label := "cancel",
        On #clicked $ do
          GtkWin.windowDestroy dialog
      ]
  hbox.append cancelButton
  vbox.append entry
  vbox.append hboxColors
  vbox.append hbox
  GtkWin.windowSetChild dialog $ Just vbox
  Gtk.widgetSetVisible entry True
  Gtk.widgetSetVisible dialog True
  return ()

mkColor :: Int -> Double
mkColor x = fromIntegral x / 255.0

refresh :: Gtk.DrawingArea -> IORef AppState -> CStructs.Context -> IO ()
refresh da1 state =
  CRC.renderWithContext $ do
    st <- liftIO $ readIORef state
    let sc = scale st
    CR.scale sc sc
    CRC.toRender (refresh' da1 state)
    CR.scale (1/sc) (1/sc)
    let doc = document st
    let conf = config st
    pg <- PopDoc.documentGetPage doc (head $ pages st)
    (pw, ph) <- PopPage.pageGetSize pg
    let nts = maybe [] (filter (\x -> notePage x == (fromIntegral . head . pages) st)) $ notes (pdq st)
    sequence_
      [ do
          CR.rectangle (sc * pw * noteX nt) (sc * ph * noteY nt) (sc * markerSize conf) (sc * markerSize conf)
          CR.setSourceRGB (mkColor $ noteR nt) (mkColor $ noteG nt) (mkColor $ noteB nt)
          CR.fill
          CR.rectangle (sc * pw * noteX nt) (sc * ph * noteY nt) (sc * markerSize conf) (sc * markerSize conf)
          CR.setLineWidth $ sc * markerSize conf / 10.0
          CR.setSourceRGB 0.0 0.0 0.0
          CR.stroke
        | nt <- nts
      ]
    case (showMatches st, (Vec.!? (fromIntegral . head $ pages st)) =<< matches st) of
      (True, Just rects) ->
        sequence_
          [ do
              coors <- liftIO $ sequence [Rect.getRectangleX1 rect, Rect.getRectangleX2 rect, Rect.getRectangleY1 rect, Rect.getRectangleY2 rect]
              -- liftIO $ putStr "match at: " >> print coors
              let cs = (sc *) <$> coors
              CR.rectangle (cs !! 0) (sc * ph - (cs !! 3)) (cs !! 1 - cs !! 0) (cs !! 3 - cs !! 2)
              CR.setLineWidth $ 5
              CR.setSourceRGB 0.75 0.25 0.0
              CR.stroke
            | rect <- rects
          ]
      _ -> return ()

data AppState = AppState
  { pdqFile :: String,
    dDir :: String,
    document :: PopDoc.Document,
    pdq :: PdQ,
    pages :: [Int32],
    totalPages :: Int32,
    scale :: Double,
    config :: Config,
    matches :: Maybe (Vec.Vector [Rect.Rectangle]),
    showMatches :: Bool,
    searchStartedOnPage :: Maybe Int32,
    poppedUp :: Maybe GtkPop.Popover,
    movingNote :: Maybe Note,
    prevXY :: Maybe (Double, Double)
  }

scrollV :: Gtk.ScrolledWindow -> Double -> IO ()
scrollV swin dv = do
  vadj <- Gtk.scrolledWindowGetVadjustment swin
  v <- Gtk.getAdjustmentValue vadj
  Gtk.setAdjustmentValue vadj (v + dv)
  Gtk.scrolledWindowSetVadjustment swin (Just vadj)

scrollH :: Gtk.ScrolledWindow -> Double -> IO ()
scrollH swin dh = do
  hadj <- Gtk.scrolledWindowGetHadjustment swin
  h <- Gtk.getAdjustmentValue hadj
  Gtk.setAdjustmentValue hadj (h + dh)
  Gtk.scrolledWindowSetHadjustment swin (Just hadj)

updateUI :: ToUpdate -> AppState -> IO ()
updateUI tu st = do
  -- GtkLbl.labelSetText (lPage tu) $ T.pack (show $ head (pages st) + 1)
  Gtk.labelSetMarkup (lPage tu) (T.pack $ "<span size=\"x-large\" weight=\"bold\">" ++ show (head (pages st) + 1) ++ "</span>")
  GtkLbl.labelSetText (lZoom tu) $ T.pack (show (round (100 * scale st)) ++ "%")
  Gtk.widgetQueueDraw (da tu)

reload :: Options -> IORef AppState -> IO ()
reload clops state = do
  st <- readIORef state
  conf <- getConfig $ dDir st
  doc <- getDoc clops
  npages <- PopDoc.documentGetNPages doc
  pdq' <- getPdQ (pdqFile st)
  let newPages = filter (< npages) $ pages st
  writeIORef state $ st {document = doc, totalPages = npages, pdq = pdq', config = conf, pages = newPages, matches = Nothing, showMatches = False}

search :: Gtk.ApplicationWindow -> ToUpdate -> Options -> IORef AppState -> IO ()
search win tu clops state = do
  st <- readIORef state
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "Go to page:"]
  entry <-
    new
      Gtk.Entry
      [ #placeholderText := "search",
        On #activate $ do
          buf <- Gtk.entryGetBuffer ?self
          txt <- T.unpack <$> GtkBuf.entryBufferGetText buf
          if null txt
            then do
              writeIORef state $ st {matches = Nothing, showMatches = False, searchStartedOnPage = Nothing}
              GtkWin.windowDestroy dialog
              readIORef state >>= updateUI tu
            else do
              ms <-
                sequence $
                  Vec.generate
                    (fromIntegral $ totalPages st)
                    (PopDoc.documentGetPage (document st) . fromIntegral >=> flip PopPage.pageFindText (T.pack txt))
              writeIORef state $ st {matches = Just ms, showMatches = True, searchStartedOnPage = Just (head $ pages st)}
              GtkWin.windowDestroy dialog
              readIORef state >>= updateUI tu
      ]
  GtkWin.windowSetChild dialog (Just entry)
  Gtk.widgetSetVisible entry True
  Gtk.widgetSetVisible dialog True

findNext :: Gtk.ApplicationWindow -> Bool -> ToUpdate -> Options -> IORef AppState -> IO ()
findNext win searchBackwards tu clops state = do
  st <- readIORef state
  case matches st of
    Just ms -> do
      let mNewPage = if searchBackwards then find (\n -> not (null (ms Vec.! fromIntegral n))) $ reverse [0 .. head (pages st) - 1] else find (\n -> not (null (ms Vec.! fromIntegral n))) [1 + head (pages st) .. totalPages st - 1]
       in mapM_
            ( \newPage -> do
                writeIORef state $ st {pages = newPage : tail (pages st), showMatches = True}
                readIORef state >>= updateUI tu
            )
            mNewPage
    Nothing -> return ()

returnToWhereSearchStarted :: ToUpdate -> IORef AppState -> IO ()
returnToWhereSearchStarted tu state = do
  st <- readIORef state
  case searchStartedOnPage st of
    Just p -> do
      let oldpages = pages st
      writeIORef state $ st {pages = if null oldpages then [p] else p : tail oldpages, showMatches = False}
      readIORef state >>= updateUI tu
    Nothing -> return ()

mkpdqfname :: String -> String
mkpdqfname pdfname = reverse $ "qdp" ++ (tail . tail . tail . reverse $ pdfname)

mkddirname :: String -> String
mkddirname pdfname = reverse $ "tdh" ++ (tail . tail . tail . reverse $ pdfname)

mouseOverNote :: Double -> Double -> Double -> Double -> Double -> Double -> Int32 -> [Note] -> Maybe Note
mouseOverNote x y pw ph sc markerSz p allNotes =
  let notesOnThisPage = filter (\t -> notePage t == fromIntegral p) allNotes
      overNotes =
        filter
          ( \nt ->
              x > sc * pw * noteX nt
                && x < sc * (pw * noteX nt + markerSz)
                && y > sc * ph * noteY nt
                && y < sc * (ph * noteY nt + markerSz)
          )
          notesOnThisPage
   in listToMaybe overNotes

activate :: Options -> Gtk.Application -> IO ()
activate clops app = do
  mdisplay <- GD.displayGetDefault
  provider <- new GtkProvider.CssProvider []
  cssFile <- getCSSFile (mkddirname $ pdfFile clops)
  Gtk.cssProviderLoadFromFile provider cssFile
  mapM_ (\disp -> GtkStyleContext.styleContextAddProviderForDisplay disp provider $ fromIntegral GtkConst.STYLE_PROVIDER_PRIORITY_USER - 1) mdisplay
  let pdqF = mkpdqfname $ pdfFile clops
  let pdqD = mkddirname $ pdfFile clops
  conf <- getConfig pdqD
  doesFileExist pdqF >>= flip unless (savePdQ pdqF def)
  pdq' <- getPdQ pdqF
  print pdq'
  doc0 <- getDoc clops
  npages <- PopDoc.documentGetNPages doc0
  state <-
    newIORef $
      AppState
        { pdqFile = pdqF,
          dDir = pdqD,
          pages = [fromIntegral clops.openPage - 1],
          totalPages = npages,
          scale = initialScale conf,
          document = doc0,
          pdq = pdq',
          config = conf,
          matches = Nothing,
          showMatches = False,
          searchStartedOnPage = Nothing,
          poppedUp = Nothing,
          movingNote = Nothing,
          prevXY = Nothing
        }

  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 1]

  da <-
    new
      Gtk.DrawingArea
      [#name := "da"]
  Gtk.widgetAddCssClass da "main-area"
  view <- new Gtk.Viewport [#child := da, #hexpand := True]
  swin <- new Gtk.ScrolledWindow [#child := view, #hexpand := True]

  separator <- new Gtk.Separator [#orientation := Gtk.OrientationHorizontal]
  pageLabelLabel <- new Gtk.Label []
  Gtk.labelSetMarkup pageLabelLabel "<small>page</small>"
  pageLabel <- new Gtk.Label []
  Gtk.labelSetMarkup pageLabel (T.pack $ "<span size=\"x-large\" weight=\"bold\">" ++ show (openPage clops) ++ "</span>")
  totalPagesLabelLabel <- new Gtk.Label []
  Gtk.labelSetMarkup totalPagesLabelLabel "<small>of</small>"
  totalPagesLabel <- new Gtk.Label [#label := (T.pack . show $ npages)]
  zoomLabel <- new Gtk.Label [#label := T.pack (show (round $ 100 * initialScale conf) ++ "%")]

  let toUpdate = ToUpdate {da = da, lPage = pageLabel, lZoom = zoomLabel}

  buttonNextPage <-
    new
      Gtk.Button
      [ #label := "⇨",
        On
          #clicked
          ( do
              st <- readIORef state
              when
                (head (pages st) < totalPages st - 1)
                ( (writeIORef state $ st {pages = head (pages st) + 1 : tail (pages st)})
                    >> readIORef state
                    >>= updateUI toUpdate
                )
          )
      ]
  buttonPrevPage <-
    new
      Gtk.Button
      [ #label := "⇦",
        On
          #clicked
          ( do
              st <- readIORef state
              when
                (head (pages st) > 0)
                ( (writeIORef state $ st {pages = head (pages st) - 1 : tail (pages st)})
                    >> readIORef state
                    >>= updateUI toUpdate
                )
          )
      ]
  buttonGoBack <-
    new
      Gtk.Button
      [ #label := "🔙",
        On
          #clicked
          ( do
              st <- readIORef state
              when
                (length (pages st) > 1)
                ( (writeIORef state $ st {pages = tail (pages st)})
                    >> readIORef state
                    >>= updateUI toUpdate
                )
          )
      ]
  buttonZoomIn <-
    new
      Gtk.Button
      [ #label := "⊕",
        On
          #clicked
          ( do
              st <- readIORef state
              let newScale = scale st * scaleStep conf
              writeIORef state $ st {scale = newScale}
              readIORef state >>= updateUI toUpdate
          )
      ]
  buttonZoomOut <-
    new
      Gtk.Button
      [ #label := "⊖",
        On
          #clicked
          ( do
              st <- readIORef state
              let newScale = scale st / scaleStep conf
              writeIORef state $ st {scale = newScale}
              readIORef state >>= updateUI toUpdate
          )
      ]
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := T.pack $ pdfFile clops,
        #child := hbox
      ]
  buttonStartSearch <-
    new
      Gtk.Button
      [ #label := "🔍",
        On
          #clicked
          ( do
              st <- readIORef state
              search window toUpdate clops state
          )
      ]
  buttonNextMatch <-
    new
      Gtk.Button
      [ #label := "⏭",
        On
          #clicked
          (findNext window False toUpdate clops state)
      ]
  buttonPrevMatch <-
    new
      Gtk.Button
      [ #label := "⏮",
        On
          #clicked
          (findNext window True toUpdate clops state)
      ]
  buttonReturnToWhereSearchStarted <-
    new
      Gtk.Button
      [ #label := "✖",
        On
          #clicked
          (returnToWhereSearchStarted toUpdate state)
      ]
  buttonTextExtract <-
    new
      Gtk.Button
      [ #label := "txt",
        On #clicked (textExtract window conf state)
      ]
  buttonReload <-
    new
      Gtk.Button
      [ #label := "↺",
        On #clicked (reload clops state >> readIORef state >>= updateUI toUpdate)
      ]
  buttonDashboard <-
    new
      Gtk.Button
      [ #label := "📋",
        On
          #clicked
          (dashboard window toUpdate state)
      ]
  buttonOpenPdQ <-
    new
      Gtk.Button
      [ #label := "pdq",
        On #clicked $ do
          -- cancellable <- GCancellable.cancellableGetCurrent
          task <- GTask.taskNew (Just ?self) (Nothing :: Maybe Cancellable) Nothing
          GTask.taskRunInThread task (\tsk obj dat mcanc -> readIORef state >>= (openEditor . pdqFile))
      ]
  buttonOpenInkscape <-
    new
      Gtk.Button
      [ #label := "✍",
        On #clicked $ do
          putStrLn "hi"
          -- cancellable <- GCancellable.cancellableGetCurrent
          task <- GTask.taskNew (Just ?self) (Nothing :: Maybe Cancellable) Nothing
          GTask.taskRunInThread
            task
            ( \tsk obj dat mcanc -> do
                st <- readIORef state
                svg <- prepSVG (1 + fromIntegral (head $ pages st)) (overlayLayerID conf) (Just $ dDir st)
                openInkscape svg
            )
      ]
  toolbar <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 1]
  toolbar.append buttonReload
  toolbar.append buttonPrevPage
  toolbar.append buttonNextPage
  toolbar.append buttonGoBack
  toolbar.append separator
  toolbar.append pageLabelLabel
  toolbar.append pageLabel
  toolbar.append totalPagesLabelLabel
  toolbar.append totalPagesLabel
  toolbar.append separator
  toolbar.append buttonZoomIn
  toolbar.append buttonZoomOut
  toolbar.append zoomLabel
  toolbar.append buttonStartSearch
  toolbar.append buttonPrevMatch
  toolbar.append buttonNextMatch
  toolbar.append buttonReturnToWhereSearchStarted
  toolbar.append buttonTextExtract
  toolbar.append buttonDashboard
  toolbar.append buttonOpenPdQ
  toolbar.append buttonOpenInkscape

  controllerMouseMove <-
    new
      Gtk.EventControllerMotion
      [ On #motion $ \x y ->
          do
            ev <- Gtk.eventControllerGetCurrentEvent ?self
            modifier <- mapM Gdk.eventGetModifierState ev
            let rightButtonPressed = maybe False (elem GdkFlags.ModifierTypeButton1Mask) modifier
            st <- readIORef state
            if rightButtonPressed -- scrolling
              then case prevXY st of
                Just (xPrev, yPrev) -> do
                  x0 <- Gtk.scrolledWindowGetHadjustment swin >>= Gtk.adjustmentGetValue
                  y0 <- Gtk.scrolledWindowGetVadjustment swin >>= Gtk.adjustmentGetValue
                  scrollH swin $ (xPrev - (x - x0))
                  scrollV swin $ (yPrev - (y - y0))
                  writeIORef state (st {prevXY = Just (x - x0, y - y0)})
                Nothing -> do
                  x0 <- Gtk.scrolledWindowGetHadjustment swin >>= Gtk.adjustmentGetValue
                  y0 <- Gtk.scrolledWindowGetVadjustment swin >>= Gtk.adjustmentGetValue
                  writeIORef state (st {prevXY = Just (x - x0, y - y0)})
              else do
                let sc = scale st
                let conf = config st
                let doc = document st
                pg <- PopDoc.documentGetPage doc (head $ pages st)
                (pw, ph) <- PopPage.pageGetSize pg
                let nts = maybe [] (filter (\t -> notePage t == (fromIntegral . head . pages) st)) $ notes (pdq st)
                let onNote = notes (pdq st) >>= mouseOverNote x y pw ph sc (markerSize conf) (head $ pages st)
                case onNote of
                  Just nt -> when (isNothing $ poppedUp st) $ do
                    lbl <- new Gtk.Label [#label := T.pack (fromMaybe "-" (note nt))]
                    pop <- new Gtk.Popover [#child := lbl, #autohide := False]
                    rct <- new GdkRect.Rectangle [#x := round x, #y := round y]
                    Gtk.widgetSetParent pop da
                    GtkPop.popoverSetPointingTo pop (Just rct)
                    writeIORef state $ st {poppedUp = Just pop}
                    GtkPop.popoverPresent pop
                    GtkPop.popoverPopup pop
                  Nothing -> do
                    mapM_ GtkPop.popoverPopdown (poppedUp st)
                    writeIORef state (st {poppedUp = Nothing})
      ]
  controllerMouseClick <-
    new
      Gtk.GestureClick
      [ On #pressed $ \_nclicks x y -> do
          st <- readIORef state
          p <- PopDoc.documentGetPage (document st) (head $ pages st)
          (_width, height) <- PopPage.pageGetSize p
          let s = scale st
          links <- PopPage.pageGetLinkMapping p
          sequence_
            [ do
                rect <- LinkMap.getLinkMappingArea link
                coors <- sequence [Rect.getRectangleX1 rect, Rect.getRectangleX2 rect, Rect.getRectangleY1 rect, Rect.getRectangleY2 rect]
                -- putStrLn $ "Link at: [" ++ show (s * coors !! 0) ++ "," ++ show (s * coors !! 1) ++ "]x[" ++ show (s * (height - coors !! 2)) ++ "," ++ show (s * (height - coors !! 3)) ++ "]"
                when (x > s * coors !! 0 && x < s * coors !! 1 && y > s * (height - coors !! 3) && y < s * (height - coors !! 2)) $ do
                  putStrLn "hit"
                  action <- LinkMap.getLinkMappingAction link
                  actionRemote <- mapM Act.getActionGotoRemote action
                  actionGoTo <- mapM Act.getActionGotoDest action
                  actionGoToType <- mapM AGD.getActionGotoDestType actionGoTo
                  case actionGoToType of
                    Just PopEnums.ActionTypeGotoDest -> do
                      case actionGoTo of
                        Just goTo -> do
                          dest <- AGD.getActionGotoDestDest goTo
                          destType <- mapM Dest.getDestType dest
                          name <- join <$> mapM Dest.getDestNamedDest dest
                          pnum <- mapM Dest.getDestPageNum dest

                          case (name, destType) of
                            (Nothing, _) -> do
                              putStrLn $ "Link destination DestType is: " ++ show destType
                              case pnum of
                                Just n1 -> do
                                  putStrLn $ "going to page: " ++ show n1
                                  when (n1 > 0) (writeIORef state $ st {pages = n1 - 1 : pages st})
                                  readIORef state >>= updateUI toUpdate
                                Nothing -> putStrLn $ "Unknown destination page number"
                            (Just name1, Just PopEnums.DestTypeNamed) -> do
                              d1 <- PopDoc.documentFindDest (document st) name1
                              n1 <- Dest.getDestPageNum d1
                              putStrLn $ "going to page: " ++ show n1
                              when (n1 > 0) (writeIORef state $ st {pages = n1 - 1 : pages st})
                              readIORef state >>= updateUI toUpdate
                            -- Dest.destFree d1
                            (_, dtype) -> do
                              putStrLn $ "dtype is: " ++ show dtype
                        -- mapM_ Dest.destFree dest
                        Nothing -> return ()
                    Just PopEnums.ActionTypeUri -> do
                      putStrLn "Link destination is a URI"
                      case actionRemote of
                        Just remote -> do
                          remoteType <- AGR.getActionGotoRemoteType remote
                          remoteTitle <- AGR.getActionGotoRemoteTitle remote
                          remoteFileName <- AGR.getActionGotoRemoteFileName remote
                          putStrLn $ "RemoteType: " ++ show remoteType
                          putStrLn $ "RemoteTitle: " ++ show remoteTitle
                          putStrLn $ "RemoteFileName: " ++ show remoteFileName
                          mdisplay <- GD.displayGetDefault
                          case mdisplay of
                            Just display -> do
                              clipboard <- GD.displayGetClipboard display
                              gvalFile <- GV.toGValue remoteFileName
                              CB.clipboardSet clipboard gvalFile
                            Nothing -> putStrLn "=== No display"
                        Nothing -> do
                          putStrLn "No URI action remote"
                    Just other -> do
                      putStrLn $ "=== Other ActionType: " ++ show other
                    Nothing ->
                      putStrLn "=== Nothing as ActionGoToType"
              | -- mapM_ Act.actionFree action
                link <- links
            ]
          x0 <- Gtk.scrolledWindowGetHadjustment swin >>= Gtk.adjustmentGetValue
          y0 <- Gtk.scrolledWindowGetVadjustment swin >>= Gtk.adjustmentGetValue
          modifyIORef state (\s -> s {prevXY = Just (x - x0, y - y0)})
      ]
  Gtk.widgetAddController da controllerMouseClick
  Gtk.widgetAddController da controllerMouseMove
  controllerMouseRightClick <-
    new
      Gtk.GestureClick
      [ On #pressed $ \_nclicks x y -> do
          ev <- Gtk.eventControllerGetCurrentEvent ?self
          modifier <- mapM Gdk.eventGetModifierState ev
          let shiftPressed = maybe False (elem GdkFlags.ModifierTypeShiftMask) modifier
          when shiftPressed $ putStrLn "=== SHIFT ==="
          st <- readIORef state
          let sc = scale st
          let conf = config st
          p <- PopDoc.documentGetPage (document st) (head $ pages st)
          (pw, ph) <- PopPage.pageGetSize p
          let onNote = notes (pdq st) >>= mouseOverNote x y pw ph sc (markerSize conf) (head $ pages st)
          noteDialog window (x / pw / sc) (y / ph / sc) onNote toUpdate state
          putStrLn $ "right click" ++ show x ++ " " ++ show y
          print onNote
      ]
  Gtk.gestureSingleSetButton controllerMouseRightClick $ fromIntegral GdkConst.BUTTON_SECONDARY

  Gtk.widgetAddController da controllerMouseRightClick

  hbox.append toolbar
  hbox.append swin
  controllerKeyPress <-
    new
      Gtk.EventControllerKey
      [ On #keyPressed $ \x _ mdfr -> do
          st <- readIORef state
          let uui = readIORef state >>= updateUI toUpdate
           in case (mdfr, x) of
                (_, Gdk.KEY_a) -> newBookmarkDialog window toUpdate state
                (_, Gdk.KEY_c) -> makeAbsolute (pdfFile clops) >>= copyToClipboard
                (_, Gdk.KEY_d) -> makeAbsolute (dDir st) >>= copyToClipboard
                (_, Gdk.KEY_j) -> scrollV swin (dY conf) >> uui
                (_, Gdk.KEY_J) -> scrollV swin (5 * dY conf) >> uui
                (_, Gdk.KEY_k) -> scrollV swin (-dY conf) >> uui
                (_, Gdk.KEY_K) -> scrollV swin (-5 * dY conf) >> uui
                ([Gdk.ModifierTypeControlMask], Gdk.KEY_l) -> reload clops state >> uui
                (_, Gdk.KEY_l) -> scrollH swin (dX conf) >> uui
                (_, Gdk.KEY_L) -> scrollH swin (5 * dX conf) >> uui
                (_, Gdk.KEY_h) -> scrollH swin (-dX conf) >> uui
                (_, Gdk.KEY_H) -> scrollH swin (-5 * dX conf) >> uui
                (_, Gdk.KEY_slash) -> search window toUpdate clops state
                ([Gdk.ModifierTypeControlMask], Gdk.KEY_g) -> returnToWhereSearchStarted toUpdate state
                (_, Gdk.KEY_g) -> gotoPageDialog window toUpdate state
                (_, Gdk.KEY_period) -> writeIORef state (st {scale = scaleStep conf * scale st}) >> uui
                (_, Gdk.KEY_comma) -> writeIORef state (st {scale = scale st / scaleStep conf}) >> uui
                ([Gdk.ModifierTypeControlMask], Gdk.KEY_n) -> findNext window False toUpdate clops state
                (_, Gdk.KEY_n) ->
                  when
                    (head (pages st) + 1 < totalPages st)
                    (writeIORef state (st {pages = head (pages st) + 1 : tail (pages st)}) >> uui)
                ([Gdk.ModifierTypeControlMask], Gdk.KEY_p) -> findNext window True toUpdate clops state
                (_, Gdk.KEY_p) ->
                  when
                    (head (pages st) > 0)
                    (writeIORef state (st {pages = head (pages st) - 1 : tail (pages st)}) >> uui)
                ([Gdk.ModifierTypeControlMask], Gdk.KEY_b) -> gotoBookmarkDialog window toUpdate state
                ([], Gdk.KEY_t) -> textExtract window conf state
                (_, Gdk.KEY_b) -> when (length (pages st) > 1) (writeIORef state (st {pages = tail (pages st)}) >> uui)
                ([], Gdk.KEY_F1) -> writeIORef state (st {showMatches = not $ showMatches st}) >> uui
                _ -> return ()
          return True
      ]

  Gtk.drawingAreaSetDrawFunc
    da
    ( Just $ \area context _ _ -> do
        refresh area state context
        return ()
    )
  Gtk.widgetAddController window controllerKeyPress
  Gtk.windowSetDefaultSize window (fromIntegral $ windowWidth conf) (fromIntegral $ windowHeight conf)
  window.show

main :: IO ()
main = do
  clops <- Opt.execParser $ Opt.info (Opt.helper <*> optParser) Opt.fullDesc
  if extractPage clops > 0
    then do
      conf <- getConfig "."
      _ <- prepSVG (extractPage clops) (overlayLayerID conf) Nothing
      return ()
    else do
      app <-
        new
          Gtk.Application
          [ #applicationId := "hdt",
            On #activate (activate clops ?self)
          ]
      void $ app.run Nothing
