{-# LANGUAGE OverloadedStrings #-}

-- Example from https://github.com/robstewart57/rdf4h/blob/master/examples/ESWC.hs

module Main where

import Data.RDF
import qualified Data.Text as T

eswcCommitteeURI, heldByProp :: T.Text
eswcCommitteeURI = "http://data.semanticweb.org/conference/eswc/2015/program-committee-member"
heldByProp       = "swc:heldBy"

-- | returns a list of full names of people who served as
--   members on the ESWC 2015 conference programme committee.
eswcCommitteeMembers :: RDF TList -> [T.Text]
eswcCommitteeMembers graph =
  let triples = query graph (Just (unode eswcCommitteeURI)) (Just (unode heldByProp)) Nothing
      memberURIs = map objectOf triples
  in map
     (\memberURI ->
              let (LNode (PlainL firstName)) =
                    objectOf $ head $ query graph (Just memberURI) (Just (unode "foaf:firstName")) Nothing
                  (LNode (PlainL lastName))  =
                    objectOf $ head $ query graph (Just memberURI) (Just (unode "foaf:lastName")) Nothing
              in (T.append firstName (T.append (T.pack  " ") lastName)))
     memberURIs
        
main :: IO ()
main = do
  result <- parseURL
    (XmlParser Nothing Nothing)
    "http://data.semanticweb.org/dumps/conferences/eswc-2015-complete.rdf"
  case result of
    Left (ParseFailure err) -> error ("Unable to parse RDF content from that URL: " ++ err)
    Right rdfGraph -> do
      let eswcMemberNames = eswcCommitteeMembers rdfGraph
      mapM_ (putStrLn . T.unpack) eswcMemberNames
