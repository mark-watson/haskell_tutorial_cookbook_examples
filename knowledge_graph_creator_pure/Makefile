all: gendata rdf cypher

gendata:
	stack build --fast --exec Dev-exe

rdf:
	echo "Removing duplicate RDF statements"
	awk '!visited[$$0]++' out.n3 > output.n3
	rm -f out.n3

cypher:
	echo "Removing duplicate Cypher statements"
	awk '!visited[$$0]++' out.cypher > output.cypher
	rm -f out.cypher

clean:
	rm -f *~ */*~

run:
	../python-intelligent-systems/spacy_nlp_web_service/bin/spacynlpserver  > /tmp/AISspacynlp.txt &
	../python-intelligent-systems/coref_anaphora_resolution_web_service/bin/corefserver  > /tmp/AISspacycoref.txt &

stop:
	pkill -f corefserver
	pkill -f spacynlpserver

tidy:
	cd src/fileutils; stylish-haskell -i *.hs; hindent *.hs
	cd src/nlp; stylish-haskell -i *.hs; hindent *.hs
	cd src/sw; stylish-haskell -i *.hs; hindent *.hs
	cd src/webclients; stylish-haskell -i *.hs; hindent *.hs
	cd src/toplevel; stylish-haskell -i *.hs; hindent *.hs
	cd test; stylish-haskell -i *.hs; hindent *.hs
	cd app; stylish-haskell -i *.hs; hindent *.hs
	cd dev; stylish-haskell -i *.hs; hindent *.hs

