# Haskell REST api Server for Factom blockchain

Re-implements [OpenApi](https://docs.openapi.de-facto.pro/api-sdks) specification from DeFacto for connecting to Factom blockchain in a Haskell programming language. This server implementation provides automated OpenApi/Swagger generation for Elm and Purescript based on internal metaprogramming techniques.

## Running

You need to have GHC and Stack tool to run this server.

1. Build executable with
```bash
$ stack build
```

2. Run executable with following command
```bash
stack exec -- factom-open-api-server -p 5001 -c config/config.local.yaml -d
```

3. Change configuration parameters by editing config folder and related values
