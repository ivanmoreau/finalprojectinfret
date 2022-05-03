   Similitud Coseno
=======================

This is the program for Similitud Conseno written in Haskell.

- parser: Parses a file storing the final serialized data.
- latex: Generate LaTeX tables.
- docs: Show docs
- weights: Show weights
- similitudes: Show similarity between documents. If inorder, then sort by score (slower).
- words: Show words


````bash
ivanmolinarebolledo@Ivans-macOS proyectorecinfo % cabal run Similitudcoseno -- -\?
Up to date
The similitudcoseno program

similitudcoseno [COMMAND] ... [OPTIONS]

Common flags:
  -? --help         Display help message
  -V --version      Print version information

similitudcoseno parser [OPTIONS]

  -i --input=ITEM 
  -o --output=ITEM

similitudcoseno latex [OPTIONS]

  -i --input=ITEM 

similitudcoseno docs [OPTIONS]

  -i --input=ITEM 

similitudcoseno weights [OPTIONS]

  -i --input=ITEM 

similitudcoseno similitudes [OPTIONS]

     --input=ITEM 
     --inorder    

similitudcoseno words [OPTIONS]

  -i --input=ITEM 
ivanmolinarebolledo@Ivans-macOS proyectorecinfo %
```
