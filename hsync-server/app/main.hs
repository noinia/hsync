import Prelude                 (IO)
import Yesod.Default.Config    (fromArgs)
import Yesod.Default.Main      (defaultMain)
import HSync.Server.Settings   (parseExtra)
import HSync.Server.Application(makeApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) makeApplication
