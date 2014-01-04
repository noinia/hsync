import Prelude                 (IO, return, ($))
import Yesod.Default.Config    (fromArgs,appExtra)
import Yesod.Default.Main      (defaultMain)
import HSync.Server.Settings   (parseExtra)
import HSync.Server.Application(makeApplication)
import HSync.Server.AcidSync   (withAcidSync)

main :: IO ()
main = do
         config <- fromArgs parseExtra
         withAcidSync (appExtra config) $ \acidSync ->
           defaultMain (return config) (makeApplication acidSync)
