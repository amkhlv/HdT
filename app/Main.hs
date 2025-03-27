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
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import GHC.Int (Int32)
import qualified GI.Cairo.Render as CR
import qualified GI.Cairo.Render.Connector as CRC
import qualified GI.Cairo.Structs as CStructs
import qualified GI.Gdk as Gdk
import qualified GI.Gdk.Objects.Clipboard as CB
import qualified GI.Gdk.Objects.Display as GD
import qualified GI.Gio.Interfaces.File as GFile
import GI.Gio.Objects.Cancellable
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.EntryBuffer as GtkBuf
import qualified GI.Gtk.Objects.Label as GtkLbl
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
import qualified Options.Applicative as Opt
import PdQ
import PrepSVG
import System.Directory
import Text.Read (readMaybe)

data Options = Options
  { extractPage :: Int,
    pdfFile :: String
  }

optParser :: Opt.Parser Options
optParser =
  Options
    <$> Opt.option Opt.auto (Opt.long "extract-page" <> Opt.short 'e' <> Opt.help "extract page in SVG" <> Opt.value 0)
    <*> Opt.strArgument (Opt.metavar "PDF_FILE" <> Opt.value "")

getGFile :: Options -> IO GFile.File
getGFile clops = GFile.fileNewForPath $ pdfFile clops

getDoc :: Options -> IO PopDoc.Document
getDoc clops = do
  pdfGFile <- getGFile clops
  PopDoc.documentNewFromGfile pdfGFile Nothing (Nothing :: Maybe Cancellable)

refresh' :: Gtk.DrawingArea -> IORef AppState -> CStructs.Context -> IO (Int32, Int32)
refresh' da state ctxt = do
  st <- readIORef state
  let sc = scale st
  let doc = document st
  let conf = config st
  pg <- PopDoc.documentGetPage doc (head $ pages st)
  (pw, ph) <- PopPage.pageGetSize pg
  Gtk.drawingAreaSetContentWidth da $ round (sc * pw)
  Gtk.setWidgetWidthRequest da $ round (sc * pw)
  Gtk.drawingAreaSetContentHeight da $ round (sc * ph)
  Gtk.setWidgetHeightRequest da $ round (sc * ph)
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
  return (round pw, round ph)

newBookmarkDialog :: Gtk.ApplicationWindow -> Gtk.DrawingArea -> Gtk.Label -> Gtk.Label -> IORef AppState -> IO ()
newBookmarkDialog win da l1 l2 state = do
  st <- readIORef state
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "New Bookmark"]
  entry <-
    new
      Gtk.Entry
      [ #placeholderText := "page number",
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

gotoBookmarkDialog :: Gtk.ApplicationWindow -> Gtk.DrawingArea -> Gtk.Label -> Gtk.Label -> IORef AppState -> IO ()
gotoBookmarkDialog win da l1 l2 state = do
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
                    readIORef state >>= updateUI da l1 l2
                    GtkWin.windowDestroy dialog
                  | (i, bm) <- zip [97 ..] (fromMaybe [] $ bookmarks $ pdq st)
                ]
          return True
      ]
  GtkWin.windowSetChild dialog (Just vbox)
  Gtk.widgetAddController dialog controllerKeyPress
  Gtk.widgetSetVisible vbox True
  Gtk.widgetSetVisible dialog True
  putStrLn "TODO"

gotoPageDialog :: Gtk.ApplicationWindow -> Gtk.DrawingArea -> Gtk.Label -> Gtk.Label -> IORef AppState -> IO ()
gotoPageDialog win da l1 l2 state = do
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
            Just num -> do
              let newSt = st {pages = (num - 1) : pages st}
              writeIORef state newSt
              GtkWin.windowDestroy dialog
              readIORef state >>= updateUI da l1 l2
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

mkColor :: Int -> Double
mkColor x = fromIntegral x / 255.0

refresh :: Gtk.DrawingArea -> IORef AppState -> CStructs.Context -> IO (Int32, Int32)
refresh da state =
  CRC.renderWithContext $ do
    st <- liftIO $ readIORef state
    let sc = scale st
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
              liftIO $ putStr "match at: " >> print coors
              let cs = (sc *) <$> coors
              CR.rectangle (cs !! 0) (sc * ph - (cs !! 3)) (cs !! 1 - cs !! 0) (cs !! 3 - cs !! 2)
              CR.setLineWidth $ 5
              CR.setSourceRGB 0.75 0.25 0.0
              CR.stroke
            | rect <- rects
          ]
      _ -> return ()
    CR.scale sc sc
    CRC.toRender (refresh' da state)

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
    showMatches :: Bool
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

updateUI :: Gtk.DrawingArea -> Gtk.Label -> Gtk.Label -> AppState -> IO ()
updateUI da pageLabel zoomLabel st = do
  GtkLbl.labelSetText pageLabel $ T.pack (show $ head (pages st) + 1)
  GtkLbl.labelSetText zoomLabel $ T.pack (show (round (100 * scale st)) ++ "%")
  Gtk.widgetQueueDraw da

reload :: Options -> IORef AppState -> IO ()
reload clops state = do
  st <- readIORef state
  conf <- getConfig
  doc <- getDoc clops
  npages <- PopDoc.documentGetNPages doc
  pdq' <- getPdQ (pdqFile st)
  let newPages = filter (< npages) $ pages st
  writeIORef state $ st {document = doc, totalPages = npages, pdq = pdq', config = conf, pages = newPages, matches = Nothing, showMatches = False}

search :: Gtk.ApplicationWindow -> Gtk.DrawingArea -> Gtk.Label -> Gtk.Label -> Options -> IORef AppState -> IO ()
search win da pageLabel zoomLabel clops state = do
  st <- readIORef state
  dialog <- new Gtk.Window [#transientFor := win, #destroyWithParent := True, #modal := True, #title := "Go to page:"]
  entry <-
    new
      Gtk.Entry
      [ #placeholderText := "search",
        On #activate $ do
          putStrLn "activate"
          buf <- Gtk.entryGetBuffer ?self
          txt <- T.unpack <$> GtkBuf.entryBufferGetText buf
          if null txt
            then do
              writeIORef state $ st {matches = Nothing, showMatches = False}
              GtkWin.windowDestroy dialog
              readIORef state >>= updateUI da pageLabel zoomLabel
            else do
              ms <-
                sequence $
                  Vec.generate
                    (fromIntegral $ totalPages st)
                    (PopDoc.documentGetPage (document st) . fromIntegral >=> flip PopPage.pageFindText (T.pack txt))
              writeIORef state $ st {matches = Just ms, showMatches = True}
              GtkWin.windowDestroy dialog
              readIORef state >>= updateUI da pageLabel zoomLabel
      ]
  GtkWin.windowSetChild dialog (Just entry)
  Gtk.widgetSetVisible entry True
  Gtk.widgetSetVisible dialog True

findNext :: Gtk.ApplicationWindow -> Bool -> Gtk.DrawingArea -> Gtk.Label -> Gtk.Label -> Options -> IORef AppState -> IO ()
findNext win searchBackwards da pageLabel zoomLabel clops state = do
  st <- readIORef state
  case matches st of
    Just ms -> do
      let mNewPage = if searchBackwards then find (\n -> not (null (ms Vec.! fromIntegral n))) $ reverse [0 .. head (pages st) - 1] else find (\n -> not (null (ms Vec.! fromIntegral n))) [1 + head (pages st) .. totalPages st - 1]
       in mapM_
            ( \newPage -> do
                writeIORef state $ st {pages = newPage : tail (pages st), showMatches = True}
                readIORef state >>= updateUI da pageLabel zoomLabel
            )
            mNewPage
    Nothing -> return ()

activate :: Options -> Gtk.Application -> IO ()
activate clops app = do
  conf <- getConfig
  let pdqF = reverse $ "qdp" ++ (tail . tail . tail . reverse $ pdfFile clops)
  doesFileExist pdqF >>= flip unless (savePdQ pdqF def)
  pdq' <- getPdQ pdqF
  print pdq'
  doc <- getDoc clops
  npages <- PopDoc.documentGetNPages doc
  state <-
    newIORef $
      AppState
        { pdqFile = pdqF,
          dDir = reverse $ "d" ++ (tail . tail . tail . reverse $ pdfFile clops),
          pages = [0],
          totalPages = npages,
          scale = initialScale conf,
          document = doc,
          pdq = pdq',
          config = conf,
          matches = Nothing,
          showMatches = False
        }

  hbox <- new Gtk.Box [#orientation := Gtk.OrientationHorizontal, #spacing := 1]

  da <-
    new
      Gtk.DrawingArea
      [#name := "da"]
  view <- new Gtk.Viewport [#child := da, #hexpand := True]
  swin <- new Gtk.ScrolledWindow [#child := view, #hexpand := True]

  pageLabel <- new Gtk.Label [#label := "--"]
  zoomLabel <- new Gtk.Label [#label := "--"]

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
                ((writeIORef state $ st {pages = head (pages st) + 1 : tail (pages st)}) >> updateUI da pageLabel zoomLabel st)
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
                ((writeIORef state $ st {pages = head (pages st) - 1 : tail (pages st)}) >> updateUI da pageLabel zoomLabel st)
          )
      ]
  buttonGoBack <-
    new
      Gtk.Button
      [ #label := "↩",
        On
          #clicked
          ( do
              st <- readIORef state
              when
                (length (pages st) > 1)
                ((writeIORef state $ st {pages = tail (pages st)}) >> updateUI da pageLabel zoomLabel st)
          )
      ]
  toolbar <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 1]
  toolbar.append buttonNextPage
  toolbar.append buttonPrevPage
  toolbar.append buttonGoBack
  toolbar.append pageLabel
  toolbar.append zoomLabel

  controllerMouse <-
    new
      Gtk.GestureClick
      [ On #pressed $ \_nclicks x y -> do
          p <- readIORef state >>= PopDoc.documentGetPage doc . head . pages
          st <- readIORef state
          (_width, height) <- PopPage.pageGetSize p
          let s = scale st
          links <- PopPage.pageGetLinkMapping p
          sequence_
            [ do
                rect <- LinkMap.getLinkMappingArea link
                coors <- sequence [Rect.getRectangleX1 rect, Rect.getRectangleX2 rect, Rect.getRectangleY1 rect, Rect.getRectangleY2 rect]
                putStrLn $ "Link at: [" ++ show (s * coors !! 0) ++ "," ++ show (s * coors !! 1) ++ "]x[" ++ show (s * (height - coors !! 2)) ++ "," ++ show (s * (height - coors !! 3)) ++ "]"
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
                          case (name, destType) of
                            (Nothing, _) -> do
                              putStrLn $ "Link destination DestType is: " ++ show destType
                            (Just name1, Just PopEnums.DestTypeNamed) -> do
                              d1 <- PopDoc.documentFindDest doc name1
                              n1 <- Dest.getDestPageNum d1
                              putStrLn $ "going to page: " ++ show n1
                              when (n1 > 0) (writeIORef state $ st {pages = n1 - 1 : pages st})
                              readIORef state >>= updateUI da pageLabel zoomLabel
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
      ]
  Gtk.widgetAddController da controllerMouse

  hbox.append toolbar
  hbox.append swin
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app,
        #title := T.pack $ pdfFile clops,
        #child := hbox
      ]
  controllerKeyPress <-
    new
      Gtk.EventControllerKey
      [ On #keyPressed $ \x _ mdfr -> do
          st <- readIORef state
          case (mdfr, x) of
            (_, Gdk.KEY_a) -> newBookmarkDialog window da pageLabel zoomLabel state
            (_, Gdk.KEY_j) -> scrollV swin $ dY conf
            (_, Gdk.KEY_J) -> scrollV swin $ 5 * dY conf
            (_, Gdk.KEY_k) -> scrollV swin $ -dY conf
            (_, Gdk.KEY_K) -> scrollV swin $ -5 * dY conf
            ([Gdk.ModifierTypeControlMask], Gdk.KEY_l) -> reload clops state
            (_, Gdk.KEY_l) -> scrollH swin $ dX conf
            (_, Gdk.KEY_L) -> scrollH swin $ 5 * dX conf
            (_, Gdk.KEY_h) -> scrollH swin $ -dX conf
            (_, Gdk.KEY_H) -> scrollH swin $ -5 * dX conf
            (_, Gdk.KEY_slash) -> search window da pageLabel zoomLabel clops state
            (_, Gdk.KEY_g) -> gotoPageDialog window da pageLabel zoomLabel state
            (_, Gdk.KEY_period) -> writeIORef state $ st {scale = scaleStep conf * scale st}
            (_, Gdk.KEY_comma) -> writeIORef state $ st {scale = scale st / scaleStep conf}
            ([Gdk.ModifierTypeControlMask], Gdk.KEY_n) -> findNext window False da pageLabel zoomLabel clops state
            (_, Gdk.KEY_n) -> when (head (pages st) + 1 < totalPages st) (writeIORef state $ st {pages = head (pages st) + 1 : tail (pages st)})
            ([Gdk.ModifierTypeControlMask], Gdk.KEY_p) -> findNext window True da pageLabel zoomLabel clops state
            (_, Gdk.KEY_p) -> when (head (pages st) > 0) (writeIORef state $ st {pages = head (pages st) - 1 : tail (pages st)})
            ([Gdk.ModifierTypeControlMask], Gdk.KEY_b) -> gotoBookmarkDialog window da pageLabel zoomLabel state
            (_, Gdk.KEY_b) -> when (length (pages st) > 1) (writeIORef state $ st {pages = tail (pages st)})
            ([], Gdk.KEY_F1) -> writeIORef state $ st {showMatches = not $ showMatches st}
            _ -> return ()

          newst <- readIORef state
          updateUI da pageLabel zoomLabel newst
          return True
      ]

  Gtk.drawingAreaSetDrawFunc
    da
    ( Just $ \area context _ _ -> do
        _ <- refresh area state context
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
      conf <- getConfig
      prepSVG (extractPage clops) (overlayLayerID conf)
    else do
      app <-
        new
          Gtk.Application
          [ #applicationId := "hdt",
            On #activate (activate clops ?self)
          ]

      void $ app.run Nothing
