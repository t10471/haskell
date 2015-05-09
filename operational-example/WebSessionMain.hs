
import WebSessionState
import Network.CGI

main = runCGI . runWeb $ example
