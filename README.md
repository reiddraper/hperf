```
cabal sandbox init
cabal install --only-dependencies -j

# run the server in one terminal
cabal run -- -s

# run the client in another
cabal run -- -c
```
