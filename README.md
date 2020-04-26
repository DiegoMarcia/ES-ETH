# ES-ETH
ES-ETH (from the Italian _**E**secuzione **S**imbolica di contratti **ETH**ereum_, id est _Symbolic Execution of Ethereum contracts_) is my MSc thesis project. Its main aim is to perform code analysis on smart contracts involved in an arbitrary Ethereum transaction, in order to determine the sets of read and written _Storage_ locations as well as account balances.

## General approach
The original idea was to perform a static analysis of the contract's bytecode, but as the tool's name suggests, it rapidly became clear that some sort of code interpreting was needed to achieve the main goal. As an obvious example of the obstacles against purely static analysis, one could think about the bytecode being based on an operand stack rather than expressions, wich requires reconstructing stack transformations around each Storage operation _at least_.  
Therefore, **ES-ETH** carries a lazy execution of the bytecode, borrowing from typical symbolic execution techniques.

## Followed refs
The tool has been implemented following review _d2232ff_ of the Yellow Paper, which describes the protocol version _EIP 608_ (codename _Tangerine Whistle_).  
Ocaml version used during development is 4.04.0, but based on the language changelogs 4.05.0 should be fine too.

## Dependencies
- **ES-ETH** interacts with [Etherscan][1]'s web API, which exposes methods equivalent to Ethereum's JSON-RPC interface.
- Your system should provide the tool with access to _wget_ in order to retrieve transaction data from Etherscan.
- Transaction data is parsed using _yojson_. You should be able to install the library through opam. Yojson version used during development is [1.3.3][2] and no tests have been carried with following versions, but I assume backwards compatibility has been preserved. Anyway, since the received json object is fairly basic, I might remove this dependency in the future in favour of home-made parsing. No one likes dependencies.
- Since the EVM uses 32-byte integers, this tool uses arbitrary precision library _[Num][3]_. I apologise for this choice, since developers had already been advised against its use (being Zarith the new standard) at time of development. Num has been stripped from OCaml core distribution since version 4.06.0 but is still available throgh opam. `opam switch` could be used in case you should want to use OCaml 4.04.0 while having another version installed as default in your system. I commit myself to bringing **ES-ETH** to align with current OCaml distribution. Some day.
- Modules _Str_ and _Unix_ should still be available within core distribution.
- I don't remember neither when nor why I tossed [OCamlMakefile][6], so it might come back in the future.

## Compiling
To compile **ES-ETH**, it suffices to

    cd src
    make

A `clean` target is provided too.

## Executing
After compilation, you can simply

    cd ../bin
    ./ES-ETH <TRANSACTION_HASH>

to execute **ES-ETH**. Of course, being `<TRANSACTION_HASH>` a valid hash for a transaction. It can also be provided by standard input.  
Prior to analysis, the tool will `wget` the transaction data from Etherscan and save it in a `<TRANSACTION_HASH>.tmp` file. As of today, following executions with the same hash will _not_ rely on this file, and a new request will be made each time.

If you are not interested in performing the analysis on all contracts involved in a transaction, you can specify a single contract with

    ./ES-ETH --contract <CONTRACT_HASH>

being `<CONTRACT_HASH>` a valid hash for an Externally Owned Account.  
Please note that the analysis will be performed on all the possible paths in the contract. Again, a new request will be made each time the tool gets invoked.

You can make up a dummy transaction with custom call data or currency transfer, put it in a file and call

    ./ES-ETH -i <DUMMY_TRANSACTION_HASH>

where `<DUMMY_TRANSACTION_HASH>` is a file whose format is valid, ie the same fields as the ones in Etherscan's response are present.

    ./ES-ETH --contract -i <DUMMY_CONTRACT_HASH>

will do the same with a dummy contract. The same constraint applies.

### A note on Etherscan
**ES-ETH** is powered by Etherscan.io APIs.  
Instead of making users provide their apikey for Etherscan, I simply hardcoded my key.  
I trust you will abide by Etherscan's [API Key Rate Limit][4], and with Etherscan's [Terms of Service][5] in general.

## Outputs
Results will be written on standard output, following the general format

    ( account , target , operation )

Where `operation` can be either `R` for reading or `W` for writing; `target` is the Storage location `operation` has been performed on, including the special location `BALANCE`; `account` is the hash of the Storage or Balance owner.  
Literal `<TOP>` indicates it has not been possible determining the target location or the account.  
Whole contract analysis will possibly show literal `0x00` as the hash for the account receiving currency during execution.  
Please remember that the results are to be considered an overapproximation of the real read-write sets.

### A couple examples
Let us consider executing **ES-ETH** with transaction hash `0xe8ed587b8a8fa29ac46424374b84f550b71eb8bd581071be7573a0765889a4d2`. This would yield the output

    (0x1ac5dc9ee2bdbec9410a01e2c6aee0c5d0f5f445, <TOP>, r)
    (0x1ac5dc9ee2bdbec9410a01e2c6aee0c5d0f5f445, <TOP>, w)

Which means it has not been possible to determine which Storage addresses belonging to `0x1ac5dc9ee2bdbec9410a01e2c6aee0c5d0f5f445` have been read / written in the scope of the transaction.

The output for executing on transaction `0xc4feabe0ef273efd68da8f3b00e65c4f3081a8f3d51fde9b2de138c2045019b1` should be similar to

    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, 0x0, r)
    ...
    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, 0x8, r)
    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, <TOP>, r)
    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, 0x0, w)
    ...
    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, 0x8, w)
    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, <TOP>, w)
    (0x7d56485e026d5d3881f778e99969d2b1f90c50af, <BALANCE>, w)
    (<TOP>, <BALANCE>, w)

Location `0x6` is shared by three Solidity varibles packed together in a single 32-byte slot. The tool can't currently distinguish between them.
The read and write operations on <TOP> are due to the use of dynamic arrays by the contract. Such data structures currently lead to some problems for the tool.  
The last line is due to unretrieved addresses for contracts object of `CALL` operations.

## Shortcomings
- As stated before, results retrieved from Etherscan gets saved in a temporary file, but not re-used for following executions. This is annoying.
- No _Gas_ consumption is taken into account. Therefore, no _Out-of-Gas_ exceptions are detected;
  - Not decrementing Gas means need cycle detection is needed: **ES-ETH** follows a quite naive approach, interrupting the current execution path as soon as a loop is detected. This leads to the missing of various accesses to Storage.
- Several instructions are not implemented, their execution is simulated by a symbol being put on stack:
  - SHA3 (the most needed by far, used by dictionaries and dynamic-size arrays)
  - EXTCODESIZE
  - EXTCODECOPY
  - BLOCKHASH
  - COINBASE
  - TIMESTAMP
  - NUMBER
  - DIFFICULTY
  - GASLIMIT
  - SSTORE & SLOAD
  - GAS
  - The LOGs
  - CREATE
  - CALLCODE
  - DELEGATECALL
  - SELFDESTRUCT
- Storage evolution is not simulated.
- Storage content at the moment of transaction execution is not retrieved: this was not expected to be a problem, but turns out storage location dereferencing is quite a thing.
- As stated in the example, access to limited portions of a Storage location are treated as accesses to the location as a whole.

[1]: https://etherscan.io/apis
[2]: https://opam.ocaml.org/packages/yojson/yojson.1.3.3/
[3]: https://caml.inria.fr/pub/docs/manual-ocaml/libnum.html
[4]: https://info.etherscan.com/api-return-errors/
[5]: https://etherscan.io/terms
[6]: http://mmottl.github.io/ocaml-makefile/
