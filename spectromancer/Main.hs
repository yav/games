import           Snap.Util.FileServe(serveDirectory)
import           Snap.Http.Server (quickHttpServe)

main :: IO ()
main = quickHttpServe (serveDirectory ".")


