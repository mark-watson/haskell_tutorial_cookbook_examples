build:
	cd Blackjack; stack build
	cd ClientServer; stack build
	cd CommandLineApp; stack build
	echo "skipping building Database-postgres"
	cd Database-sqlite; stack build
	cd debugging; stack build
	cd ImPure; stack build
	cd NlpTool; stack build
	cd OpenAiApiClient; stack build
	cd knowledge_graph_creator_pure; stack build
	cd Pure; stack build
	cd SparqlClient; stack build
	cd StateMonad; stack build
	cd TextProcessing; stack build
	cd Timers; stack build
	cd WebScraping; cabal build
	echo "There may be problems compiling the getenerated LexData source files in LexiconData.hs:"
	cd FastTag; stack build

clean:
	rm -r -f */.stack-work */dist-newstyle */*.dyn*

update_stack_resolver_macOs:
	sed -i ''  's/^resolver: .*/resolver: lts-22.26/' */stack.yaml

update_stack_resolver_linux:
	sed -i  's/^resolver: .*/resolver: lts-22.26/' */stack.yaml

