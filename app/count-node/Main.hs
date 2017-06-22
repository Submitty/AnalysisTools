import Lichen.Config.Languages
import Lichen.Config.CountNode
import Lichen.CountNode.Main

main :: IO ()
main = run $ defaultConfig { language = langC }
