build:
	cd Blackjack; stack build
	cd ClientServer; stack build
	cd CommandLineApp; stack build
	echo "skipping building Database-postgres"
	cd Database-sqlite; stack build
	cd debugging; stack build
	cd FastTag; stack build
	cd ImPure; stack build
	cd NlpTool; stack build
	cd Pure; stack build
	cd SparqlClient; stack build
	cd StateMonad; stack build
	cd TextProcessing; stack build
	cd Timers; stack build
	cd WebScraping; stack build

clean:
	rm -r -f */.stack-work

