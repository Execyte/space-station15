<p align="center"> <img alt="Space Station 15" width="880" height="300" src="https://raw.githubusercontent.com/Execyte/asset-dump/refs/heads/main/svg/SS15longBGP.svg" /></p>

Space Station 15 is a remake of SS13 in haskell which adds it's own gimmicks to the game. SS15 tries to keep the original tgstation style including it's tile based movement.

# Links
[Discord](https://discord.com/invite/qW8bHkncrb) | [Install Haskell](https://www.haskell.org/downloads/) [and Stack](https://docs.haskellstack.org/en/stable/)

# Building

1. Clone this repo
2. Run `stack build --haddock-docs` at the root
3. Run `stack hoogle -- generate --local` to build the hoogle docs

You will get 2 executables: space-station15-client and space-station15-server. Run them with `stack exec`.

Run `stack hoogle -- server --local --port=8080` on a separate terminal if u are planning to make changes, then connect to https://localhost:8080.
