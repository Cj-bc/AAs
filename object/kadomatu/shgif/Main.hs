import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Widgets (shgif)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif, getShgif, updateShgif)


data Name = Noname deriving (Eq, Ord)

data Kadomatu = Kadomatu {base :: Shgif
                         , center :: Shgif
                         , right  :: Shgif
                         , left   :: Shgif
                         }

kadomatuView (Kadomatu b c r l)
        = [ translateBy (Location ( 0,16)) $ shgif b
          , translateBy (Location (11, 0)) $ shgif c
          , translateBy (Location (20, 6)) $ shgif r
          , translateBy (Location ( 2, 6)) $ shgif l
          ]

ui :: Kadomatu -> [Widget Name]
ui = kadomatuView

eHandler :: Kadomatu -> BrickEvent Name TickEvent -> EventM Name (Next Kadomatu)
eHandler k (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt k
eHandler (Kadomatu b c r l) (AppEvent Tick) = continue =<< liftIO (Kadomatu <$> (updateShgif b)
                                                                            <*> (updateShgif c)
                                                                            <*> (updateShgif r)
                                                                            <*> (updateShgif l))
eHandler k _ = continue k

app :: App Kadomatu TickEvent Name
app = App { appDraw = ui
          , appHandleEvent = eHandler
          , appStartEvent  = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }

main = do
    kadomatu_base'   <- getShgif "shgif/kadomatu-shgif-base.yaml"
    kadomatu_side'   <- getShgif "shgif/kadomatu-shgif-side.yaml"
    kadomatu_center' <- getShgif "shgif/kadomatu-shgif-center.yaml"

    let isLeft (Left _) = True
        isLeft _ = False

    when (isLeft kadomatu_base') $ error "base isnt loaded"
    when (isLeft kadomatu_side') $ error "side isnt loaded"
    when (isLeft kadomatu_center') $ error "center isnt loaded"

    let (Right kadomatu_base) = kadomatu_base'
        (Right kadomatu_side) = kadomatu_side'
        (Right kadomatu_center) = kadomatu_center'

    _ <- mainWithTick Nothing 1000 app (Kadomatu kadomatu_base kadomatu_center kadomatu_side kadomatu_side)
    return ()
