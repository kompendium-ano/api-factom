# Haskell REST api client for Factom blockchain

[![Build Status](https://travis-ci.com/kompendium-llc/api-factom.svg?branch=master)](https://travis-ci.com/kompendium-llc/api-factom)
[![License](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/kompendium-llc/api-factom/blob/master/LICENSE)

Uses [OpenApi](https://docs.openapi.de-facto.pro/api-sdks) description from DeFacto for connecting to Factom blockchain in a more easier way.

Additionally Provides Swagger client generation for Elm and Purescript.

## Haskell client Usage

Install library with [Stack](https://haskellstack.org) for building Haskell client. For clients in Elm, Purescript you need to build Haskell OpenApi server which will generate appropriate Elm and Purescript bindings.

Add `api-factom` pakage to cabal file and import functionality inside your module `import Factom.Rest.Client.Api`

### 1. Require library
```haskell
import Factom.Rest.Api
```

### 2. Initialize client
```haskell
let endpoint = "http://1.2.3.4:8081"
    apiKey   = "<put user api key"
    config   = (apiKey, endpoint)
```

### 3. Use client to work with Factom Open API
1. Get API info
```haskell
-- Get API version
apiInfo <- getApiInfo config
```

2. Get user info
```haskell
-- Get user info
let user = getUserInfo config
```

3. Create a chain
```haskell
-- Creates chain on the Factom blockchain
chain <- createChain config ["My new chain", "Second ExtID"]  -- external ids
	                        "Content of the first entry"      -- optional content
```

4. Get chains
```haskell
-- Get all user’s chains
cchains <- getChains config 0 0 "" ""

-- Get user's chains from 41th to 60th
chains <- getChains config 40 20 "" ""

-- Get user's chains with status "queue"
-- start=0, limit=0 — use defaults pagination params
-- status="queue" — filter chains by status "queue" (also "processing" | "completed")
chains <- getChains config 0 0 "queue" ""

-- Get user's chains in reverse sorting (from oldest to newest)
-- start=0, limit=0 — use defaults pagination params
-- status=NULL — not filter by status
-- sort="asc" — sort results by createdAt ASC ("desc" is default sorting)
chains <- getChains config 0 0 "" "asc"

-- Combine all filters and params
-- start=40, limit=20, status="queue", sort="asc"
chains <-getChains config 40 20 "queue" "asc"
```

5. Get chain
```haskell
-- Get Factom chain by Chain ID
chain = getChain config "fb5ad150761da70e090cb2582445681e4c13107ca863f9037eaa2947cf7d225c" -- chain id
```

6. Get chain entries
```haskell
-- Get entries of Factom chain
entries <- getChainEntries config "fb5ad150761da70e090cb2582445681e4c13107ca863f9037eaa2947cf7d225c"

-- Filters and params may be applied to results
-- Example: start=40, limit=20, status="queue", sort="asc"
let chainId = "fb5ad150761da70e090cb2582445681e4c13107ca863f9037eaa2947cf7d225c"
entries <- getChainEntries config chainId 40 20 "queue" "asc"
```

7. Get first/last entry of the chain
```haskell
let chainId = "fb5ad150761da70e090cb2582445681e4c13107ca863f9037eaa2947cf7d225c";

-- Get first entry of Factom chain
firstEntry <- getChainFirstEntry config chainId

-- Get last entry of Factom chain
lastEntry <- getChainLastEntry config chainId
```

8. Search user's chains by external ids
```haskell
-- Search for chains with tag "Forum Thread"
chains <- searchChains config ["Forum Thread"] 0 0 "" ""

-- Search for entries with 2 tags simultaneously
chains2 <- searchChains config ["Forum Thread", ""v1.0.0""] 0 0 "" ""

-- Filters and params may be applied to results
-- Example: start=40, limit=20, status="completed", sort="asc"
chains = searchChains config ["Forum Thread", ""v1.0.0""] 40 20 "completed" "asc"
```

9. Search chain entries by external ids
```haskell
-- Search entries into Factom chain by external id(s)
let chainId = "fb5ad150761da70e090cb2582445681e4c13107ca863f9037eaa2947cf7d225c";

-- Search for entries with tag "Forum Post"
entries <- searchChainEntries config chainId ["Forum Post"] 0 0 "" ""

-- Search for entries with 2 tags simultaneously
entries2 <- searchChainEntries config chainId ["Forum Post", "v1.0.0"] 0 0 "" ""

-- Filters and params may be applied to results
-- Example: start=40, limit=20, status="processing", sort="asc"
entries <- searchChainEntries config chainId [] 40 20 "processing" "asc"
```

10. Create an entry
```haskell
-- Create entry in the Factom chain
let chainId = "fb5ad150761da70e090cb2582445681e4c13107ca863f9037eaa2947cf7d225c";
entry <- createEntry config chainId ["Second ExtID", ""My new entry""] "Content of the new entry"
```

11. Get entry
```haskell
-- Get Factom entry by EntryHash
let entryHash = "dc2160b99b5f46f156e54bdebc81aef3243884b68b2c0c05e4741910738273f2";
entry <- getEntry config entryHash
```

12. Generic factomd interface
```haskell
-- Example of factomd API call without params: 'heights'
heights <- getHeights config
```

## Elm client usage

Build library and run test executable to generate Elm client.

## PureScript client usage

Build library and run test executable to generate Elm client.
