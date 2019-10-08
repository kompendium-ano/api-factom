# Haskell REST api client for Factom blockchain

Uses [OpenApi](https://docs.openapi.de-facto.pro/api-sdks) description from DeFacto for connecting to Factom blockchain in a more easier way.


## Using

1. Receive your token to accecss api

2. Use provided function wrappers to get response

2.1 Import `Factom.Rest.Client.Api` module to your programm

2.2 Override endpoint `let endpoint = "https://my-awesome-url-to-open-api-serve"`

2.3 Use methods
```haskell
main :: IO ()
main = do
  -- prepare defaults
  let myEndpoint = "http://localhost:8081/v1"
      myToken    = Just "1q3sre2we25ph"

  -- receive typed Entries list
  entries <- getChainEntries endpoint token

  -- manipulate data by accessing it's fields
  -- in any way required for business logic
  let entriesHashes = map entryEntryHash entries

  -- show the dadta
  putStrLn $ show $ entriesHashes

```

3. All Response data is typed! It means that json responses are automatically
