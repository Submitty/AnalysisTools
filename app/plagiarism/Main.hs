import Lichen.Config.Languages
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Main

main :: IO ()
main = realMain $ defaultConfig { language = langC }
