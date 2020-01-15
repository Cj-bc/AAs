import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Widgets (shgif)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif, getShgif, updateShgif)

type AS = (Kadomatu, Shgif)

data Name = Noname deriving (Eq, Ord)

data Kadomatu = Kadomatu {base :: Shgif
                         , center :: Shgif
                         , right  :: Shgif
                         , left   :: Shgif
                         , flowerRight  :: Shgif
                         , flowerLeft   :: Shgif
                         }

kadomatuView (Kadomatu b c r l fr fl)
        = [ translateBy (Location ( 0,16)) $ shgif b
          , translateBy (Location (17, 0)) $ shgif c
          , translateBy (Location (26, 5)) $ shgif r
          , translateBy (Location ( 8, 5)) $ shgif l
          , translateBy (Location (36,10)) $ shgif fr
          , translateBy (Location ( 6,12)) $ shgif fl
          ]

ui :: AS -> [Widget Name]
ui (k, d) = kadomatuView k
            ++  [translateBy (Location (14, 45)) (shgif d)]

eHandler :: AS -> BrickEvent Name TickEvent -> EventM Name (Next AS)
eHandler k (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt k
eHandler (Kadomatu b c r l fr fl, d) (AppEvent Tick) = continue =<< liftIO (Kadomatu <$> (updateShgif b)
                                                                                     <*> (updateShgif c)
                                                                                     <*> (updateShgif r)
                                                                                     <*> (updateShgif l)
                                                                                     <*> (updateShgif fr)
                                                                                     <*> (updateShgif fl)
                                                                             >>= appendD)
      where
        appendD x = do
            d' <- updateShgif d
            return (x, d')
eHandler k _ = continue k

app :: App AS TickEvent Name
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
    kadomatu_flower_r' <- getShgif "shgif/kadomatu-shgif-flowerRight.yaml"
    kadomatu_flower_l' <- getShgif "shgif/kadomatu-shgif-flowerLeft.yaml"
    desc'            <- getShgif "shgif/kadomatu-shgif-description.yaml"

    let isLeft (Left _) = True
        isLeft _ = False

    when (isLeft kadomatu_base') $ error "base isnt loaded"
    when (isLeft kadomatu_side') $ error "side isnt loaded"
    when (isLeft kadomatu_center') $ error "center isnt loaded"
    when (isLeft kadomatu_flower_r') $ error "flower right isnt loaded"
    when (isLeft kadomatu_flower_l') $ error "flower left isnt loaded"
    when (isLeft desc') $ error "description isnt loaded"

    let (Right kadomatu_base) = kadomatu_base'
        (Right kadomatu_side) = kadomatu_side'
        (Right kadomatu_center) = kadomatu_center'
        (Right kadomatu_flower_r) = kadomatu_flower_r'
        (Right kadomatu_flower_l) = kadomatu_flower_l'
        (Right desc) = desc'

    _ <- mainWithTick Nothing 1000 app (Kadomatu kadomatu_base kadomatu_center kadomatu_side kadomatu_side
                                                 kadomatu_flower_r kadomatu_flower_l
                                       , desc
                                       )
    return ()
