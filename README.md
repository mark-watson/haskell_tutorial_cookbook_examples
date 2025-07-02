# Examples for "Haskell Tutorial and Cookbook" by Mark Watson

![Haskell Logo](haskell.svg)

This repo contains the examples for my Haskell book that I try to update with new material about once a year:

[Haskell Tutorial and Cookbook](https://leanpub.com/haskell-cookbook)

If you would like to support my work please consider purchasing my books on [Leanpub](https://leanpub.com/u/markwatson) and star my git repositories that you find useful on [GitHub](https://github.com/mark-watson?tab=repositories&q=&type=public). You can also interact with me on social media on [Mastodon](https://mastodon.social/@mark_watson) and [Twitter](https://twitter.com/mark_l_watson).

Note: occasionally I update code in this example GitHub repository before updating the book text.

## I am starting to use this repo for all personal Haskell projects.

In the future, if you see a file named **NOT_YET_IN_BOOK.md** in any subdirectory, then please don't expect (yet) that this example is in the book. Currently everything in this repo is in the second edition of my book.

## Updates

Updated 7/28/2023 for stack resolver: lts-lts-21.4

Note: to build sqlite example I had to:

    Set 'allow-newer: true' in /home/markw/.stack/config.yaml to ignore all version constraints

## Tool Installation

See Appendix A in my book, or simply install **stack** and **hlint**.

Note: 6/18/2020: install directions in Appendix A to optionally install **cabal-install** (with latest GHC 8.8.3) produces an error running:

        stack install cabal-install

Installing **cabal-install** is optional. Installing **stack** using the directions in Appendix A which refers you to th official documentation at [http://docs.haskellstack.org/en/stable/README.html](http://docs.haskellstack.org/en/stable/README.html is sufficient for running the exampes in my book.

## Notes for running examples using Replit.com repls (September 8, 2024)

By default stack is not installed. 
Accept the default's for Haskell after creating a new Replit repl by cloning this GitHub project.

Then update cabal package index:

    cd Pure
    cabal update

It will ask you which cabal sysem to install. I chose the command line version.

    cabal build
    cabal repl
    ghci> :l Guards.hs 
    ghci> main
