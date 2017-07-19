import Lichen.Config.Languages
import Lichen.Config.Diagnostics
import Lichen.Diagnostics.Main

main :: IO ()
main = realMain $ defaultConfig { language = langC }
