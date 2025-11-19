{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (nub)
import Control.Monad (guard)

-- ==========================================
-- 1. RDF Data Types
-- ==========================================

-- A Node can be an IRI (URI), a Literal string, or a Blank Node
data Node
  = IRI String
  | Lit String
  | BNode Int
  deriving (Eq, Ord)

instance Show Node where
  show (IRI s)   = "<" ++ s ++ ">"
  show (Lit s)   = "\"" ++ s ++ "\""
  show (BNode i) = "_:b" ++ show i

-- An RDF Triple: (Subject, Predicate, Object)
type Triple = (Node, Node, Node)

-- The Store is just a list of triples (in a real DB, this would be an indexed Set)
type Graph = [Triple]

-- ==========================================
-- 2. SPARQL Query Types
-- ==========================================

-- A Query Item can be a concrete Node or a Variable ("?x")
data QueryNode
  = QTerm Node
  | QVar String
  deriving (Eq, Show)

-- A Triple Pattern: e.g., { ?s <likes> ?o }
data TriplePattern = 
  TP QueryNode QueryNode QueryNode
  deriving (Show)

-- A simplified representation of a SELECT query
data SelectQuery = Select
  { vars  :: [String]         -- Variables to select (e.g., ["s", "o"])
  , whereClause :: [TriplePattern] -- The WHERE block
  }

-- A Binding maps variable names to concrete RDF Nodes
type Binding = Map String Node

-- ==========================================
-- 3. The Query Engine
-- ==========================================

-- | Checks if a concrete Triple matches a Triple Pattern given a starting context.
-- Returns a new Binding extended with matches if successful, or Nothing if failed.
matchTriple :: Binding -> TriplePattern -> Triple -> Maybe Binding
matchTriple ctx (TP qs qp qo) (s, p, o) = do
  ctx1 <- matchNode ctx qs s
  ctx2 <- matchNode ctx1 qp p
  matchNode ctx2 qo o

-- | Helper to match a single QueryNode against a concrete Node
matchNode :: Binding -> QueryNode -> Node -> Maybe Binding
matchNode ctx (QTerm t) n
  | t == n    = Just ctx  -- Concrete terms must match exactly
  | otherwise = Nothing
matchNode ctx (QVar v) n =
  case Map.lookup v ctx of
    Nothing -> Just (Map.insert v n ctx) -- Bind new variable
    Just val -> if val == n then Just ctx else Nothing -- Check existing binding constraint

-- | Executes a list of patterns against the graph using the List Monad for join logic.
evaluatePatterns :: Graph -> [TriplePattern] -> [Binding]
evaluatePatterns _ [] = [Map.empty] -- Base case: empty pattern results in one empty binding (identity)
evaluatePatterns graph (pat:pats) = do
  -- 1. Take previous results (or start)
  ctx <- evaluatePatterns graph pats
  
  -- 2. Find all triples in the graph that match the current pattern 
  --    consistent with the current context (ctx)
  triple <- graph
  
  -- 3. Attempt to bind the triple to the pattern
  case matchTriple ctx pat triple of
    Just newCtx -> return newCtx
    Nothing     -> []

-- | The main entry point for running a query.
-- It evaluates the WHERE clause and then projects only the requested SELECT variables.
runQuery :: Graph -> SelectQuery -> [[Node]]
runQuery graph query =
  let 
    -- 1. Find all valid bindings for the WHERE clause
    -- We reverse the patterns because the list monad naturally processes right-to-left 
    -- in the recursion above, but we want left-to-right evaluation flow.
    allBindings = evaluatePatterns graph (reverse $ whereClause query)
    
    -- 2. Project specific variables (SELECT ?s ?o)
    project binding = map (\v -> fromMaybe (Lit "NULL") (Map.lookup v binding)) (vars query)
  in
    nub $ map project allBindings

-- ==========================================
-- 4. Example Data and Usage
-- ==========================================

-- Helpers to make data entry cleaner
iri :: String -> Node
iri = IRI

lit :: String -> Node
lit = Lit

-- The "Social Network" Graph
myGraph :: Graph
myGraph =
  [ (iri "Alice", iri "likes", iri "Bob")
  , (iri "Alice", iri "likes", iri "Pizza")
  , (iri "Bob",   iri "likes", iri "Alice")
  , (iri "Bob",   iri "likes", iri "Pasta")
  , (iri "Charlie", iri "likes", iri "Bob")
  , (iri "Alice", iri "age", lit "25")
  , (iri "Bob",   iri "age", lit "28")
  ]

main :: IO ()
main = do
  putStrLn "--- RDF Store Loaded ---"
  mapM_ print myGraph
  
  putStrLn "\n--- Query 1: Select ?s ?o where { ?s likes ?o } ---"
  let q1 = Select 
        { vars = ["s", "o"]
        , whereClause = 
            [ TP (QVar "s") (QTerm (iri "likes")) (QVar "o") 
            ]
        }
  printTable ["?s", "?o"] (runQuery myGraph q1)

  putStrLn "\n--- Query 2: Select ?who where { ?who likes <Bob> } ---"
  let q2 = Select 
        { vars = ["who"]
        , whereClause = 
            [ TP (QVar "who") (QTerm (iri "likes")) (QTerm (iri "Bob")) 
            ]
        }
  printTable ["?who"] (runQuery myGraph q2)

  putStrLn "\n--- Query 3 (Join): Who likes someone who likes them back? ---"
  -- SPARQL: SELECT ?a ?b WHERE { ?a likes ?b . ?b likes ?a }
  let q3 = Select 
        { vars = ["a", "b"]
        , whereClause = 
            [ TP (QVar "a") (QTerm (iri "likes")) (QVar "b")
            , TP (QVar "b") (QTerm (iri "likes")) (QVar "a")
            ]
        }
  printTable ["?a", "?b"] (runQuery myGraph q3)

-- Utility to print results nicely
printTable :: [String] -> [[Node]] -> IO ()
printTable headers rows = do
  putStrLn $ unwords headers
  putStrLn $ replicate (length (unwords headers) + 5) '-'
  mapM_ (putStrLn . unwords . map show) rows

