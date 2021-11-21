### Introduction

**Blockchains**

See: [never ending book parable](https://github.com/jonathondilworth/blockchaincourse/blob/master/My%20Notes/0-Lecture-One.md#money-30) for intuition into 'what is a blockchain'.

TLDR: write-only memory, decentralised public ledger which acts as a platform to build on top of. The use of an example of such a platform: logging financial transactions, educational achievements, properties chain of ownership.

**Cryptocurrencies**

A specific use of Blockchain systems (distributed ledgers and blockchain consensus algorithms or protocols) where by the implementation tracks financial transactions.

Personal value (or finances) are secured through cryptographic techniques (public-private key cryptography, digital signatures, hashing functions, etc).

**Beyond Generic Blockchains**

1. Monetary Policies: which are self-regulating (on-chain governance)
2. Transaction Fees and Reward Mechanisms / Game Theory & Byzantine Fault Tolerant Protocols
3. Smart Contracts: instances of accounts or addresses which house arbitrary logic placing constraints on specific interactions between multiple signatories or other contracts.

**Ideal Blockchain**

A linearly ordered linked list where the first entry is known as the Genesis block and is usually initiated using some agreed upon data.

Each entry within the list is a block. Each block contains numerous attributes (e.g. transactions which are linearly ordered via a Merkle Tree), one of which can be thought of as a pointer to the previous block (in reality it is simply the hash of the previous block, or block header<sup><a href="#s1">1</a></sup>). Thus, eliminating the possibility of any change to any block being difficult to identify (so long as the majority of participants maintaining the blockchain are honest).

Acting dis-honestly when maintaining the distributed ledger or Blockchain would be acting irrationally (or not within ones own self-interest). This mechanism is implemented through the use of a game theoretic model which is usually based on the issuance of 'coins' or some kind of value.

**More on Hashing Functions**

The selected hash function to use for preserving immutability on the ledger must be:

* Effectively Commutable: fast to compute, ideally not computationally expensive, since consensus relies on every node executing this function in a timely manner upon the addition of a new block.
* Collision Free (and bijective): otherwise, whilst it would be highly unlikely (depending on the potential collision rate / the entropy), it would be technically possible to forge a transaction.
* Hiding: a hashing function should not be reversible (no mapping exists that given an output you can compute the original input).
* Puzzle Friendly: within PoW systems, it is easy (possible and effectively computable) to reason about puzzle computations. Example: SHA-256.

*Note: you cannot have a completely collision free hash function, since memory is discrete. Further, the hash function (or composite set thereof) output has to be truncated such that it is somewhat space efficient. As a result, the output domain of any hash function has to be discrete (orders of magnitude more discrete than the total memory of the system / node); and since you cannot have a function which maps an infinite set to a finite set, collisions are theoretically possible. Thus, the caveat that must be added to the definition of collision free hash functions  is that it is practically infeasible and extremely difficult to find two inputs which result in the same output.*

*Question: would it be accurate to say that the definition of 'hiding' (for hash functions) can be phrased such that: given a hash function H, an inverse function H' does not exist. Thus, given $h(x) = y$,  $h^{-1}(y) = x$ does not exist?*

*Question: Given the statement regarding hash functions: "the input are arbitrary byte strings, so there are potentially infinitely many of those" - given that memory is discrete, how is it possible for there to be infinitely many input byte strings? Are we discussing hash functions in terms of their implementation on a discrete system, or their mathematical properties within 'the real world' / continuous space?*

*Question: assuming a non-bijective hashing function is used in an attempt to preserve immutability, how bad of a hashing function would be required in order to forge an entire block during this lifetime? Assume we're discussing a first generation blockchain such as BitCoin and the AVG transactions per block is 1721.74 (as of NOV 20 2021). The data structure used to store transactions is a Merkle Tree, transactions and blocks are implemented using the same implementation as that on BTC. Surly it must still be difficult? Reasoning for the question: I'm thinking in terms of: threat model analysis and the kind of design decisions that are made when implementing a blockchain, their present vulnerabilities and potential future vulnerabilities, in addition to any trade-offs that could be made. Note: this is more of a hypothetical question, because, thankfully we do have highly collision free bijective hashing functions which are effectively computable, right?*

**The Importance of Transactional Ordering**

If transactions represent a change in state of UTxO values or account balances, transactions are required to be ordered such that it can be determined whether or not cumulative UTxOs (given a signatory) or account balance can cover the cumulative transactional value.

*Question (this was discussed within the IOG Discord, but I don't think consensus was necessarily reached on the outcome of the question, so I'll raise it here): if one was to push two transactions to chain with the intent to have them validated within the same block, is there anything stopping the outputs from one transaction acting as the inputs to the other? Emphasis being on: both transactions are not yet validated, but have been pushed to chain (or an SPO mempool) in an ordered fashion, such that the first transaction is pushed first and the second has been pushed second; can the second transaction use outputs from the first transaction whilst both being validated within the same block? My apologies if there is an obvious answer or is this is a silly question. I would actually be interested in knowing if this is possible in both accounting based models and UTxO based models.*

**Digital Signatures**

*Note: commonly understood within the domain of cryptography. I don't think I need to comment too much on this... However, I do have some questions regarding ECC VS RSA. Pretty sure I can just google those, but I'll outline them below for the sake of anyone who may be thinking the same thing.*

* RSA key-pairs are typically generated using an increasingly large number of bits (I believe 4096 bits are typically used for 'secure' applications as of 2021). How does RSA compare to ECC and what are the trade-offs in terms of computational complexity during generation, encryption, decryption, signature generation and signature verification?

*Note: nobody has to answer that, I'll google it.*

Within the slides (preventing forgery with digital signatures) it states that "each block is signed by the block creator), shouldn't that be: each transaction is signed by the transaction creator? Then, each transaction is verified by the block creator? Isn't that essentially what they're (miners) getting paid to do?

**Forking**

Summary of the 'Double Spend' attack: performing multiple transactions by spending the same UTxO twice before the first transaction is verified by a miner. In some instances, you can actually get competing blocks (as mentioned in the never ending book parable), but the 'true chain' will be the one that ends up having the largest number of transactions after consensus is continued to be executed. From what I understand this is typically why we're sometimes asked to wait for more than one validation per block before the true 'balance' of your account (which is just a list of UTxOs in the 'cloud') is updated. Furthermore, this gives rise to the phenomenon of orphan blocks.

**Consensus Protocols**

When you take a course in distributed computing, one of the first things you learn is "the eight fallacies of distributed computing", I can't quite remember them all, but it's like: network latency is zero, reliability assumptions, bandwidth, transport, etc. Thus, since you cannot guarantee that the network is reliable, it may be the case that the network your chains nodes are running on gets split into two or more networks, it's impossible to avoid a fork. A rather extreme example of this provided in the lecture (which did make me laugh) is that "some Atlantic cable could break" - which is possible. I just kind of hope the guys running the Atlantic cables did some research before laying them! I guess a more likely issue which could occur (and has occurred) would be problems relating to the fundamental routing protocols which underpin any network that the majority of a given chains nodes run on. I'm not networking expert, but we've seen BGP issues with organisations such Facebook fairly recently, if they're planning on launching some kind of meta-verse (which I still don't understand), there could be all kinds of potential attacks if they have a similar problem to their recent issue (assuming they're launching some kind of blockchain platform and they'll be operating a substantial number of nodes). DNS issues which cut out parts of the network? Those types of issues could result in forks and even 51% attacks, etc. Again, I am not a networking expert; and these notes are primarily for me, but I'm pretty sure what I'm saying holds some water, at least.

* Common prefix: an agreed upon chain up to a certain point. At this point the chain my diverge (and fork), but the common prefix is the set of ordered blocks prior to this point. Thus, when nodes are reconnected, they can measure the depth from the common prefix and evaluate which has a largest length; thus, the longer chain is the chain of truth.

*In short: a consensus protocol is a lottery based on some kind of asset which belongs to the majority of honest parties. The asset typically weights any honest party, such that a party with a larger asset holding has an increased probability of winning the lottery (and receiving a reward). However, it is important to note that depending on protocol implementation the probability of winning doesn't necessarily scale linearly with the amount of asset held. Furthermore, lot's of variants of these protocols exist and can be changed dynamically in some cases.*

**Examples:**

* BitCoin (BTC): Proof-of-Work (computing power, hash-rate, hashing power).
* Cardano (ADA): Dynamic-Proof-of-Stake.
* Algorand (ALGO): Pure-Proof-of-Stake.

ADA essentially just replaces computing power with network stake (although there are some more complicated details, anyway, let's talk some more about consensus protocols).

**Some Additional Details: Consensus Properties:**

Given a consensus protocol with participants who provide 'suggestions' (inputs: v), the protocol must converge to a common output (u). Thus, it must implement the following properties:

* Termination
* Agreement
* Strong Validity

Termination, meaning there exists an output given an input from an honesty party.

$ \forall i \in H\ \exists\ u_{i} $

Agreement, meaning all honest parties reach the same output, or you could say, the majority of the parties (honest, or dishonest) reach a shared output.

$ \forall i,j \in H\ (u_{i} = u_{j}) $ 

Strong Validity, meaning an honest party reaches an same output equal to any honest parties input (including the possibility of its own). That is to say strong validity maps any honest output to an element which belongs to the set of inputs.

$ \forall i \in H\ \exists\ j \in H (u_{i} = v_{j}) $

*(for all honest parties, there exists inputs which equal the output of another honest party)*

Thus, to achieve consensus (as far as I understand the theory), **termination, agreement and strong validity must hold**.

There is another property called validity, which, for the sake of time (I've been writing these notes for hours now), we won't bother diving into.

We could go on to talk about majorities and dishonest parties, but let's just move on for now.

**Transaction Structure**

*I've probably read enough about UTxO by now to watch this part, I've skimmed through and I'm pretty sure I'm happy with all this stuff.*

**Valid Blocks**

* The 'lottery winner' is the only person eligible to create (and therefore sign) the block.
* The signature of the block creator can be verified.
* The transactions in the block are valid (and can be verified).
* All Txs in the block are consistent with the ordering constraint as mentioned above.

**Fees and Incentives**

Difference between the cumulative input and output of a transaction would typically be considered to be the transaction fees paid to the block validator.

Mechanisms differ from chain to chain.

Monetary policies are also different from chain to chain.

**IOHK's Role & Philosophy**

* IOHK <=> Cryptocurrencies.
* Work on ETC in Scala and... Agda? It says ADA (Haskell)?
* Best Practices, Peer Review, Code Review, etc.
* Formal Methods and Mathematics.
* Develops a toolbox for cryptocurrencies, different protocols, incentive schemes, etc.
* Wants to get things done correctly. Don't move fast and break things, slow and steady wins the race (or so I'm told, I can't move at any speed other than slow and steady, so I feel fairly 'at home' within the community here).

**Why Haskell?**

* High level of abstraction.
* Expressive.
* DSLs are pretty straight forward-ish to write.
* Formal Verification (would that just be implementing two functions and checking that for any input, both functions map to the same output? So, you can formally verify that two functions which may be implemented, say with a list comprehension and then one with recursion do actually do the same the thing?)
* Statically Type, nice type system (type inference, parametric polymorphism, stuff some pretty nice stuff I guess?)
* Lazy evaluation
* GHC fairly optimised (I believe? Although I believe Plutus has been worked on to be even more.. refined?) [[1]](#1)
* You don't really get run-time errors due to the nature of the compiler.
* Apparently it's "a lot of fun" - it starts to get fun after about four months (from my experience).

**Footnotes**

<a href="#s1" id="s1">1</a>. As far as I understand some of this is subjective and are essentially design decisions, since hashing the entire previous block doesn't seem to make sense to me if it has already been hashed through the nature of the implemented data structure containing the blocks transactions, in BTCs case, a Merkle Tree. If the block header references the block contents, and the block contents references the Merkle Tree and the Merkle Tree references the transactions, is it not just 'hashes all the way down' - thus, simply taking a hash of the previous block header would appear to be fine, right? Or am I just completely off the mark here?

**Citations**

[1](#1). Peyton Jones, M., Gkoumas, V., Kireev, R., MacKenzie, K., Nester, C., Wadler, P. <br />
Unraveling recursion: compiling an IR with recursion to system F. <br />
Hutton, G. (ed.) MPC 2019. LNCS, vol. 11825, pp. 414–443. Springer, Cham (2019). <br /> https://
doi.org/10.1007/978-3-030-33636-3 15