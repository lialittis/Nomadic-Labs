# What is blockchain ?

At an abstract level, a blockchain is

- An immutable database;
- Operating in a decentralized network.

## Key concepts

It relies on:

- Public Key Cryptography / Digital signature / Cryptographic Hash Functions
- A probabilistic solution to the 'Byzantine generals problem' for consensus among all nodes
- A p2p / gossip network for low level communication

It is often called crypto-ledgers:

- Electronic book
- Recording transactions
- Users identity and book immutability cryptographically ensured

## How it works ?

Validate and append transactions to the ledger.

**Generic algorithm**:

1. Send/receive/broadcast new transactions to all “participants” of the network (nodes);
2. Aggregate transaction into blocks
3. The next block is broadcasted by one (or several) nodes
4. Nodes express their acceptance of a block by including its hash in the next block they create

### Difficulties

TODO


### Case study: Bitcoin





### Distributed applications: smart contracts

- Blockchain as a decentralized platform, popularized by Ethereum.
- User code in the blockchain (vending machine analogy): user stores code in block ; other users can call this code.
- Contract has state, can perform blockchain operations: interacts with outside services (oracle); can perform access control.
- Many applications: finacial contracts, new currencies, voting, games, crowd-funding.

## Tezos in a nutshell

• Protocol upgrade/Voting process

• Liquid Proof of Stake

• Michelson smart contract language

• Formally verified cryptographic primitives


### Specificities

• A baker must have a minimum of 8.000tz (a roll) to get slots

• Slot attribution is proportional to the number of rolls

• If a participant does not wish to bake, it is possible to delegate its stake



































