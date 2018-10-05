# Haskell Blockchain
:wa

A simple blockchain-like thing implemented in Haskell.

## Running
Install `cabal-install` and if you want install `nix`, and launch `nix-shell`.

```
$ touch block.chain
$ cabal new-run blockchain block.chain
$ cabal new-run blockchain block.chain
$ cat block.chain
01;05.10.18-16:30:16;01;01;Foo;ca62d1a7b28eebd850cf0f5c4483cfef105ea51dc2d8c25e947b3c6ec718b7d7
01;05.10.18-16:30:17;01;01;Foo;8e25592f5f097b65a6b313ec0f2039121122d89067959dc1132e34d2d5ad0a5e
```
