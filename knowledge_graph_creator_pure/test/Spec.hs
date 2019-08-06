import ClassificationWebClient
import CorefWebClient
import NlpWebClient

main :: IO ()
main
  -- JSON from Coref server (written in Python)
 = do
  s <- corefClient "My sister has a dog. She loves him"
  putStrLn $ s
  s2 <- nlpClient "John Smith went to Mexico."
  putStrLn $ s2
