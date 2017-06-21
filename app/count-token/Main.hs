import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.CountToken
import Lichen.CountToken.Main

main :: IO ()
main = run $ defaultConfig { language = langC }
