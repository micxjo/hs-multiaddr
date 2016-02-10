# hs-multiaddr

[![Hackage](http://img.shields.io/hackage/v/multiaddr.svg)](https://hackage.haskell.org/package/multiaddr)
[![Build Status](https://travis-ci.org/micxjo/hs-multiaddr.svg?branch=master)](https://travis-ci.org/micxjo/hs-multiaddr)

A [multiaddr](https://github.com/jbenet/multiaddr) implementation in Haskell. Multiaddr is a self-describing network address format supporting a variety of protocols, with both string and binary representations.

## Installation

We're on [Hackage](https://hackage.haskell.org/package/multiaddr), so just add ``multiaddr`` to your cabal build-depends.

## Usage

```haskell
import Network.Multiaddr

let Just somewhere = readMultiaddr "/ip4/8.8.8.8/tcp/80"
protocolNames somewhere -- ["ip4", "tcp"]

-- Encapsulation
-- Multiaddr is a monoid, `encapsulate` is just an alias for (<>)
let Just proxy = readMultiaddr "/ip6/::1/tcp/443"
let proxied = proxy `encapsulate` somewhere

toText proxied -- "/ip6/::1/tcp/443/ip4/8.8.8.8/tcp/80"
hasIPv6 proxied -- True

-- Grab an individual part of the address
parts proxied !! 2  -- "/ip4/8.8.8.8"

-- Encode into a ByteString
let bytes = encode proxied

-- Decode and get back the original!
let Just decoded = decode bytes
decoded == proxied -- True
```
