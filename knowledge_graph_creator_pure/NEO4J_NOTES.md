# login password

account:  neo4j
password: myself

# remove local database to start over

    cd ~/bin/neo4j
    rm -rf data/databases/graph.db

# run web interface

    cd ~/bin/neo4j
    ./bin/neo4j console

use http://localhost:7474/ and click Neo4J graphic in upper left to get to tutorials and console

# Loading Data

Sample data files:  https://neo4j.com/developer/example-data/

not so useful

# Loading RDF data

Neo4J plugin: https://github.com/jbarrasa/neosemantics


# good article on converting RDF data for Neo4J

https://jbarrasa.com/2016/06/07/importing-rdf-data-into-neo4j/

Rule1: Subjects of triples are mapped to nodes  in Neo4j. A node in Neo4j representing an RDF resource will be labeled :Resource and have a property uri with the resource’s URI.

(S,P,O) => (:Resource {uri:S})...

Rule2a: Predicates of triples are mapped to node properties in Neo4j if the object of the triple is a literal

(S,P,O) && isLiteral(O) => (:Resource {uri:S, P:O})

Rule 2b: Predicates of triples are mapped to relationships in Neo4j if the object of the triple is a resource

(S,P,O) && !isLiteral(O) => (:Resource {uri:S})-[:P]->(:Resource {uri:O})

Example, convert this (and a few more):

<https://newsshop.com/june/z902.html>
  <http://knowledgebooks.com/schema/news/category/>
  <http://knowledgebooks.com/schema/topic/weather> .

<https://newsshop.com/june/z902.html>
  <http://knowledgebooks.com/schema/containsCountryDbPediaLink>
  <http://dbpedia.org/resource/Canada> .

to (NOTE! not the same data, I wanted a shared node):

CREATE (z902:News {name:"z902", uri:"https://newsshop.com/june/z902.html"})
CREATE (weather:News {name:"weather", uri:"http://knowledgebooks.com/schema/topic/weather"})
CREATE (canada:News {name:"Canada", uri:"http://dbpedia.org/resource/Canada"})
CREATE (z902)-[:Category]->(weather)
CREATE (z902)-[:ContainsCountryDbPediaLink]->(canada)
CREATE (a1023:News {name:"a1023", uri:"https://newsshop.com/may/a1023.html"})
CREATE (a1023)-[:ContainsCountryDbPediaLink]->(canada)

