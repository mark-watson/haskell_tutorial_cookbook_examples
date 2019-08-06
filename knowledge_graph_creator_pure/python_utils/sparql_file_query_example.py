import rdflib.graph as g
from pprint import pprint

graph = g.Graph()
graph.parse('output.n3', format='n3')

results = graph.query("""
SELECT ?s ?p ?o
WHERE {
?s ?p ?o.
}
ORDER BY (?p)
""")

for result in results:
  pprint(result)
