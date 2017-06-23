import Lichen.Config.Languages
import Lichen.Config.Count
import Lichen.Count.Main

main :: IO ()
main = run $ defaultConfig { language = langC }
