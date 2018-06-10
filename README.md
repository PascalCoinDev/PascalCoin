# Pascal Coin: P2P Cryptocurrency without need of historical operations.  
  
Copyright (c) 2016-2018 PascalCoin developers based on original Albert Molina source code
  
THIS IS EXPERIMENTAL SOFTWARE. Use it for educational purposes only.  
  
This software is a Node of the Pascal Coin P2P Cryptocurrency.  
It can be used to Mine and Explore blocks and operations.  
  
Distributed under the MIT software license, see the accompanying file  
LICENSE  or visit http://www.opensource.org/licenses/mit-license.php.  

This product includes software developed by the OpenSSL Project and Denis  
Grinyuk (https://github.com/Arvur/OpenSSL-Delphi), and some  
cryptographic functions inspirated in code written by Ladar Levison and   
Marco Ferrante.  
Original source code is written in Pascal Language and is available at   
https://github.com/PascalCoin/PascalCoin  
  
  
## HOW TO COMPILE:  
  
See instructions at GitHub Wiki: https://github.com/PascalCoin/PascalCoin/wiki
  
  
Enjoy Pascal Coin!
  
## Donations  
  
If you like it, consider a donation using BitCoin:
16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

Also, consider a donation at PascalCoin development account: "0-10"

## History:  

### Build 3.0.1 - 2018-05-07
- Deprecated use of OpenSSL v1.0 versions. Only allowed OpenSSL v1.1 versions
- JSON-RPC Added param "openssl" on "nodestatus" call. Will return OpenSSL library version as described in OpenSSL_version_num ( https://www.openssl.org/docs/man1.1.0/crypto/OPENSSL_VERSION_NUMBER.html )

### Build 3.0.0 - 2018-05-02
- Implementation of Hard fork on block 210000
  - PIP - 0010: 50% inflation reduction
  - PIP - 0011: 20% Development reward
  - PIP - 0017: Anonymity via transaction mixing (multioperation)
  - New target calc on protocol V3 in order to reduce the sinusoidal effect
    - Harmonization of the sinusoidal effect modifying the rise / fall by 50% calculating over the last 10 blocks only when increase/decrease is high
- New Safebox Snapshoting
  - This allow quickly rollback/commit directly to Safebox instead of create a separate Safebox on memory (read from disk... use more ram...)
  - Is usefull when detecting posible orphan blocks in order to check which chain is the highest chain without duplicating a safebox to compare
- New Node network operations
  - Get pending operations (code $0030)
    - Implementation of the PIP-0013 (not exactly but with similar features)	
  - Get account (code $0031)
    - This call will allow a simple third party app communicate directly to a node to get account info (balance, n_operation, name, public key... )
  - Reserverd codes from $1000 to $1FFF
    - A node will not break connection if those codes are used, but will response with ERRORCODE_NOT_IMPLEMENTED ($00FF)
- MultiOperation: PIP-0017
  - Multioperation allows a transactional like operations, they can include transactions and change info operations in a signle multioperation
    - Allow to send coins from N accounts to M receivers in a transaction mixing, without knowledge of how many coins where sent from "Alice" to "Bob" if properly mixed
	- Ophash can be previously known by all signers before signing. They must sign only if multioperation includes it's transactions as expected 
	- OpHash of a multioperation will allow to include n_operation and account of each signer account, but md160hash chunk will be the same for all
- JSON-RPC changes:
  - Added param "startblock" to "getaccountoperations" in order to start searching backwards on a specific block. Note: Balance will not be returned on each operation due cannot be calculated. Default value "0" means start searching on current block as usual
  - Operation Object changes:
    New fields:
    - "senders" : ARRAY of objects - When is a transaction, this array contains each sender
      - "account" : Sending Account 
      - "n_operation"
      - "amount" : PASCURRENCY - In negative value, due it's outgoing from "account"
      - "payload" : HEXASTRING
    - "receivers" : ARRAY of objects - When is a transaction, this array contains each receiver
      - "account" : Receiving Account 
      - "amount" : PASCURRENCY - In positive value, due it's incoming from a sender to "account"
      - "payload" : HEXASTRING
    - "changers" : ARRAY of objects - When accounts changed state
      - "account" : changing Account 
      - "n_operation"
      - "new_enc_pubkey" : If public key is changed or when is listed for a private sale
      - "new_name" : If name is changed
      - "new_type" : If type is changed
      - "seller_account" : If is listed for sale (public or private) will show seller account
      - "account_price"	: PASCURRENCY - If is listed for sale (public or private) will show account price
      - "locked_until_block" : If is listed for private sale will show block locked
      - "fee" : PASCURRENCY - In negative value, due it's outgoing from "account"
    Modified fields / DEPRECATED FIELDS
    Caused by multioperation introduction, search in "senders"/"receivers"/"changers" instead
    - "balance" will not be included when is not possible to calc previous balance of account searching at the past
    - "signer_account" will not be included in Multioperations
    - "account" : will not be included in Multioperations, use fields in "senders"/"receivers"/"changers" instead    
    - "n_operation" will not be included in Multioperations, use fields in "senders"/"receivers"/"changers" instead
    - "payload" will not be included in Multioperations, use fields in "senders"/"receivers"/"changers" instead
    - "sender_account" is not correct to be used. Use "account" param on "senders" array instead
    - "dest_account" is not correct to be used. Use "account" param on "receivers" array instead
    - "amount" is not correct to be used. Use each "amount" param on "senders/receivers" instad. Note: sender "amount" is a negative number, positive for receiver
  - New object "MultiOperation Object" : Will return info about a MultiOperation
    - "rawoperations" : HEXASTRING with this single MultiOperation in RAW format
    - "senders" : Will return an Array with Objects
      - "account" : Sending Account 
      - "n_operation"
      - "amount" : In negative value, due it's outgoing from "account"
      - "payload"
    - "receivers"
      - "account" : Receiving Account 
      - "amount" : In positive value, due it's incoming from a sender to "account"
      - "payload"
    - "changers" : Will return an Array with Objects
      - "account" : changing Account 
      - "n_operation"
      - "new_enc_pubkey" : If public key is changed
      - "new_name" : If name is changed
      - "new_type" : If type is changed
    - "amount" : PASCURRENCY Amount received by receivers
    - "fee" : PASCURRENCY Equal to "total send" - "total received"
	- "signed_count" : Integer with info about how many accounts are signed. Does not check if signature is valid for a multioperation not included in blockchain 
	- "not_signed_count" : Integer with info about how many accounts are pending to be signed
    - "signed_can_execute"	: Boolean. True if everybody signed. Does not check if MultiOperation is well formed or can be added to Network because is an offline call
  - New method "signmessage": Signs a digest message using a public key
    - Params:
      - "digest" : HEXASTRING with the message to sign
	  - "b58_pubkey" or "enc_pubkey" : HEXASTRING with the public key that will use to sign "digest" data
    - Result: False on error
      - "digest" : HEXASTRING with the message to sign
	  - "enc_pubkey" : HESATRING with the public key that used to sign "digest" data
	  - "signature" : HEXASTRING with signature
  - New method "verifysign": Verify if a digest message is signed by a public key
    - Params:
      - "digest" : HEXASTRING with the message to check
	  - "b58_pubkey" or "enc_pubkey" : HEXASTRING with the public key that used to sign "digest" data
	  - "signature" : HEXASTRING returned by "signmessage" call
    - Result: False on error
      - "digest" : HEXASTRING with the message to check
	  - "enc_pubkey" : HESATRING with the public key that used to sign "digest" data
	  - "signature" : HEXASTRING with signature
  - New method "multioperationaddoperation": Adds operations to a multioperation (or creates a new multioperation and adds new operations)
    This method does not need current Safebox state, so can be used offline or on COLD wallets when all info is provided
    - Params:
      - "rawoperations" : HEXASTRING (optional) with previous multioperation. If is valid and contains a single  multiopertion will add operations to this one, otherwise will generate a NEW MULTIOPERATION
	  - "auto_n_operation" : Boolean - Will fill n_operation (if not provided). Only valid if wallet is ONLINE (no cold wallets)
      - "senders" : ARRAY of objects that will be Senders of the multioperation
        - "account" : Integer
        - "n_operation" : Integer (optional) - if not provided, will use current safebox n_operation+1 value (on online wallets)
        - "amount" : PASCURRENCY in positive format
        - "payload" : HEXASTRING
      - "receivers" : ARRAY of objects that will be Receivers of the multioperation
        - "account" : Integer
        - "amount" : PASCURRENCY in positive format
        - "payload" : HEXASTRING
      - "changesinfo" : ARRAY of objects that will be accounts executing a changing info
        - "account" : Integer
        - "n_operation" : Integer (optional) - if not provided, will use current safebox n_operation+1 value (on online wallets)
        - "new_b58_pubkey"/"new_enc_pubkey": (optional) If provided will update Public key of "account"
        - "new_name" : STRING (optional) If provided will change account name
        - "new_type" : Integer (optional) If provided will change account type
    - Result:
      If success will return a "MultiOperation Object"
  - New method "multioperationsignoffline"
    This method will sign a Multioperation found in a "rawoperations"
	Must provide info about accounts and keys (current Safebox state, provided by an ONLINE wallet)
    - Params:
      -	"rawoperations" : HEXASTRING with 1 multioperation in Raw format
      - "accounts_and_keys"	: ARRAY of objects with info about accounts and public keys to sign
        - "account" : Integer 
        - "b58_pubkey" or "enc_pubkey" : HEXASTRING with the public key of the account
    - Result:
      If success will return a "MultiOperation Object"
  - New method "multioperationsignonline"
    This method will sign a Multioperation found in a "rawoperations" based on current safebox state public keys
	Must provide info about accounts and keys (current Safebox state, provided by an ONLINE wallet)
    - Params:
      -	"rawoperations" : HEXASTRING with 1 multioperation in Raw format
    - Result:
      If success will return a "MultiOperation Object"
  - New method "operationsdelete"
    This method will delete an operation included in a Raw operations object
    - Params:
      -	"rawoperations" : HEXASTRING with Raw Operations Object
    - Result:
      If success will return a "Raw Operations Object"
      - "rawoperations" : HEXASTRING with operations in Raw format
      - "operations" : Integer
      - "amount" : PASCURRENCY
      - "fee" : PASCURRENCY  
  - Updated method "getpendings" : Added params "start" (0..N) default=0 and "max" default=100 (0 = ALL)
    - Also fixed bug #86 : https://github.com/PascalCoin/PascalCoin/issues/86
  - New method "getpendingscount" : Returns pending operations count
- Daemon:
  - Allow to force max block read from Blockchain when started using "-b MAX_BLOCK_NUMBER" param. Example "nohup ./pascalcoin_daemon -r -b 12345 &"
- Protections against invalid nodes (scammers):
  - Protection on GetBlocks and GetBlockOperations
- Merged new GUI with current stable core
- New folders organization
- Bugs solved

### Build 2.1.9 - 2018-04-16
- Searchs last valid block found on corrupted BlockChainStream.blocks file and allows to continue from last valid one. On prior versions, app halted and needed manually file deletion

### Build 2.1.8 - 2018-04-16
- Solved bug that can cause to corrupt BlockChainStream.blocks file when detecting an orphan block and creating new BlockHeaders row (every 1000 blocks). Very rare bug, but fatal error

### Build 2.1.7 - 2018-04-10
- Remove use of TPCOperation.FSignatureChecked introduced on 2.1.6 because is not 100% secure
- Minor bugs

### Build 2.1.6 - 2018-02-14
- Important improvements
  - Improved speed when processing operations on start
  - Improved speed when processing pending operations after a new block found
  - Deleted duplicate "SanitizeOperations" call
  - Verify signed operations only once (TPCOperation.FSignatureChecked Boolean)
  - Improvements in search methods of TOperationsHashTree
    - Increase speed in search methods thanks to internal ordered lists
    - Increase speed copying thanks to using FHashTree sender buffer instad of generating new one
  - Internal bugs
- Those improvements solved BUG that caused operations not included to blockchain due slow processing with MemPool 
- NOTE: It's HIGHLY RECOMMENDED to upgrade to this version

### Build 2.1.5 - 2018-02-09
- GUI changes:
  - Allow massive accounts "change info" operation
  - Added "account type" and "sale price" on accounts grid
  - Show "account type" stats on search account form  
  - Changed Icon to current PascalCoin icon
- Pending operations buffer cached to file to allow daemon/app restart without losing pending operations
- Less memory usage thanks to a Public keys centralised buffer
- JSON-RPC changes
  - Added param "n_operation" to "Operation Object" JSON-RPC call
  - New method "findnoperation": Search an operation made to an account based on n_operation field
    - Params:
	  - "account" : Account
	  - "n_operation" : n_operation field (n_operation is an incremental value to protect double spend)
	- Result:
	  - If success, returns an Operation Object	  
  - New method "findnoperations": Search an operation made to an account based on n_operation 
    - Params:
	  - "account" : Account
	  - "n_operation_min" : Min n_operation to search
	  - "n_operation_max" : Max n_operation to search
	  - "start_block" : (optional) Block number to start search. 0=Search all, including pending operations
	- Result:
	  - If success, returns an array of Operation Object
  - New method "decodeophash": Decodes block/account/n_operation info of a 32 bytes ophash
    - Params:
      - "ophash" : HEXASTRING with an ophash (ophash is 32 bytes, so must be 64 hexa valid chars)
    - Result:
      - "block" : Integer. Block number. 0=unknown or pending
      - "account" : Integer. Account number
      - "n_operation" : Integer. n_operation used by the account. n_operation is an incremental value, cannot be used twice on same account.
      - "md160hash" : HEXASTRING with MD160 hash
- Solved bug that caused to delete blockchain when checking memory 
- Solved bug in Network adjusted time on receiving connections caused by full entry buffer
- Minor optimizations

### Build 2.1.3.0 - 2017-11-15
- Fixed BUG when buying account assigning an invalid public key
- Added maxim value to node servers buffer, deleting old node servers not used, this improves speed
- Re-add orphaned operations back into the pending pool
- RPC locking to prevent N_Operation race-condition on concurrent invocations
- Minor bugs

### Build 2.1.2.0 - 2017-07-27
- No more blockchain in installer = TRUE DELETABLE BLOCKCHAIN (Safebox will be automatically downloaded by client from network)
- Fixed storage bug when downloading a new safebox
- Safebox will be downloaded in small chunks (aprox 2mb per chunk)
- Read safebox file improvements (quick start)

### Build 2.1.1.0 - 2017-07-16
- Fixed installer bug: In last Windows installer a malformed safebox file was included. This build is only to provide a good safebox file with the installer
- No important changes in exe/binary file 

### Build 2.1.0.0 - 2017-07-14
- Fixed bug of slow mass transactions from a single account due to incorrect sending order
- Fixed memory leak on GetSafeBox request call
- Added new GUI improvements on Operations form

### Build 2.0.0.0 - 2017-06-23
- MANDATORY UPGRADE - HARD FORK ACTIVATION WILL OCCUR ON BLOCK 115000
- Introducing Protocol v2.
  - https://github.com/PascalCoin/PascalCoin/blob/master/PascalCoinWhitePaperV2.pdf
  - Core Changes:
    - New safebox hash calculation algorithm to allow safebox checkpointing
    - Improved difficulty target calculation to obtain a more stable average blocktime of 5 minutes, given abrupt hashpower fluctuations
    - Bug-fix for China region users
    - Anti-spam measures
      - New Consensus Rule: A block can only contain 1 zero fee operation per sender account. If a sender account issues 2 or more zero-fee operations, that block is invalid.
    - SafeBox now stored in chunks to facilitate checkpoint distribution
	- Reintroduce orphan blocks operations on main blockchain
  - Added Checkpointing:
    - Checkpoint created on every 100'th block
    - Added network operations to transfer Checkpoint chunks (compressed)
  - Addded In-Protocol PASA Exchanging:
    - New Operation: List Account, used to list an account for public or private sale 
    - New Operation: Delist Account, used to delist an account from sale 
    - New Operation: Buy Account, used to purchase an account lised for sale
    - Updated Operation: Transaction, can be used in place of Buy Account operation in private account sales
  - RPC API Changes:
    - All changes are backwards-compatible for current 3rd-party infrastructure providers (exchanges, wallets).
    - However, use of param "ophash" has been changed in V2, see "Operation Object changes" section to know how to use "old_ophash" param value    
    - JSON changes:
      - Account Object changes:
        - Added param "state"  (values can be "normal" or "listed". When listed then account is for sale)
        - If "state" is "listed" then will fill next values:
          - "locked_until_block", "price", "seller_account" 
          - "private_sale" : Boolean value 
          - "new_enc_pubkey" : (Only on "private_sale" = true) - HEXASTRING with private sale encoded public key
        - Added param "name": String
          - Name available chars: abcdefghijklmnopqrstuvwxyz0123456789!@#$%^&*()-+{}[]\_:"|<>,.?/~
          - Name first char: (cannot start with number): abcdefghijklmnopqrstuvwxyz!@#$%^&*()-+{}[]\_:"|<>,.?/~
          - Name lenght: Empty or 3..64 chars length
        - Added param "type": Integer (Valid values from 0..65535)
      - Operation Object changes:
        - Changed param "ophash": Current returned value of "ophash" is not the same than previous builds (changed calculation method)
        - New param "old_ophash": Will return previous "ophash" value only for old blocks (prior to v2 protocol activation)
        - New param "subtype" : Based on "optype" param, this param allows to discrimine point of view of operation (sender/receiver/buyer/seller ...)
        - New param "signer_account" : Will return the account that signed (and payed fee) for this operation
        - Param "enc_pubkey": Will return both change key and the private sale public key value
        - About method "findoperation" and "ophash"
          - Due to changed "ophash" param value calculation, method "findoperation" will find old "ophash" values too
          - This is to avoid 100% compatibility, but remember that returned "Operation object" will contain "ophash" value calculated in new format only (you can read "old_ophash" value in older blocks)
      - New methods:
        - "listaccountforsale" : Lists an account for sale (public or private)
          - Params:
            - "account_target" : Account to be listed
            - "account_signer" : Account that signs and pays the fee (must have same public key that listed account, or be the same)
            - "price": price account can be purchased for
            - "seller_account" : Account that will receive "price" amount on sell
            - "new_b58_pubkey"/"new_enc_pubkey": If used, then will be a private sale
            - "locked_until_block" : Block number until this account will be locked (a locked account cannot execute operations while locked)
            - "fee","payload","payload_method","pwd"
        - "signlistaccountforsale" : Signs a List an account for sale (public or private) for cold wallets
          - Params: Same "listaccountforsale" params, and also:
            - "rawoperations" : HEXASTRING with previous signed operations (optional)
            - "signer_b58_pubkey"/"signer_enc_pubkey" : The current public key of "account_signer"
            - "last_n_operation" : The current n_operation of signer account        
        - "delistaccountforsale" : Delist an account for sale
          - Params:
            - "account_target" : Account to be delisted
            - "account_signer" : Account that signs and pays the fee (must have same public key that delisted account, or be the same)
            - "fee","payload","payload_method","pwd"
        - "signdelistaccountforsale" : Signs a List an account for sale (public or private) for cold wallets
		  - Params: Same "delistaccountforsale" params, and also:
            - "rawoperations" : HEXASTRING with previous signed operations (optional)
            - "signer_b58_pubkey"/"signer_enc_pubkey" : The current public key of "account_signer"
            - "last_n_operation" : The current n_operation of signer account                      
        - "buyaccount" : Buy an account previously listed for sale (public or private)
          - Params: 
            - "buyer_account","account_to_purchase",
            - "price","seller_account",
            - "new_b58_pubkey"/"new_enc_pubkey","amount",
            - "fee","payload","payload_method","pwd"
        - "signbuyaccount" : Signs a buy operation for cold wallets
          - Params: Same "buyaccount" params, and also:
            - "rawoperations" : HEXASTRING with previous signed operations (optional)
            - "signer_b58_pubkey"/"signer_enc_pubkey" : The current public key of "buyer_account"
            - "last_n_operation" : The current n_operation of buyer account	
        - "changeaccountinfo" : Changes an account Public key, or name, or type value (at least 1 on 3)
            - Params:
            - "account_target" : Account to be delisted
            - "account_signer" : Account that signs and pays the fee (must have same public key that target account, or be the same)
            - "new_b58_pubkey"/"new_enc_pubkey": If used, then will change the target account public key
            - "new_name": If used, then will change the target account name
            - "new_type": If used, then will change the target account type
            - "fee","payload","payload_method","pwd"
        - "signchangeaccountinfo" : Signs a change account info for cold cold wallets
          - Params: Same "changeaccountinfo" params, and also:
            - "rawoperations" : HEXASTRING with previous signed operations (optional)
            - "signer_b58_pubkey"/"signer_enc_pubkey" : The current public key of "account_signer"
            - "last_n_operation" : The current n_operation of signer account
        - "findaccounts" : Find accounts by name/type and returns them as an array of Account objects
          - Returned array will be sorted by account number
          - Params:
            - "name" : (String) If has value, will return the account that match name
            - "type" : (Integer) If has value, will return accounts with same type
            - "start" : (Integer) Start account (by default, 0)
            - "max" : (Integer) Max of accounts returned in array (by default, 100)
      - Updated methods:
        - "changekey" : Added param "account_signer" that allow to pay fee using another account with same public key than target "account" param. This allows to change keys on empty accounts (balance = 0) and pay fee
        - "signchangekey" : Added param "account_signer", same use that in "changekey" method
  - GUI changes:  (not available on daemon)
    - New columns "name" and "type" on accounts
    - Information about Accounts/Operation using F1
    - Operations form now accepts:  (Will be active after V2 protocol activation)
      - List account for sale (public or private)
      - Delist account
      - Buy account
      - Change account name/type
    - Operations form allows to search for accounts using F2 over an account edit box
    - Changes in text showing operations information     
- Other changes:
  - Bug for china users fixed  
  - Bugs fixed

### Build 1.5.6.0 - 2017-05-03
- Allow multiselect accounts (GUI wallet)
- Priority for operations with fee:
  - Allow miner server (pools) to select what to mine using pascalcoin_daemon.ini file (daemon only)
    - RPC_SERVERMINER_MAX_OPERATIONS_PER_BLOCK: Max of operations that can be included in a block (Default 5000, min value 1000)
    - RPC_SERVERMINER_MAX_ZERO_FEE_OPERATIONS: Max of operations with 0 fee that can be included in a block (Default 2000, min value 400)
  - Operations with fee have always preference over 0 fee operations (will be mined first)
  - If operations with fee fills all the buffer, then no zero fee operation will be included
- Fixed bug on receiving big blocks and slow net connection to prevent never synchronize
- Fixed bug on miner server that produces "invalid operations hash" error on valid solutions with 0 operations
- Fixed bug on file storage
- Fixed minor bugs

### Build 1.5.5.0 - 2017-04-11
- Corrected fee result on RPC calls as a negative number on "Change key" operations
- Corrected PASCURRENCY value to be limited as a 4 decimal digits on RPC calls
- JSON-RPC method "getaccountoperations" changed: if param "start" is -1, will include pending operations, otherwise not
- Fixed bug: On "getaccountoperations" if an account had a lot of operations (receive tx included) then sometimes app crashed when executing depth search.
  - Note: High depth search is slow because it search always starting from current state, going backwards, in order to return past balance. This can be a slow method on some account with high transactions volume.

### Build 1.5.4.0 - 2017-03-14

- Added Network Timestamp Adjustment (NAT) to calc valid timestamps
  - Minimum 4 active connections to calc median used for NAT, otherwise use local timestamp
  - Based on IP's (to prevent a malicious IP timejacking, each IP is only used once)
  - Removed IP's and recalculated after disconnecting (to prevent malicious node connecting/disconnecting for timejacking)
- New blocks will not be accepted if using future timestamp greater than NAT timestamp + 15 seconds (also, mantaining current protocol rule >= lastBlock.timestamp)
- Network protocol fixed to 5-5 (Nodes with version prior to 1.5 will not be allowed)
- Added protection for non included operations on a block, to prevent continuous sending. Only will resend operations once.
- Bug #27 fixed: Invalid timestamp on FPC (https://github.com/PascalCoin/PascalCoin/issues/27)
- JSON-RPC
  - Method "getconnections" added "timediff" to know timestamp diff of node
  - Method "payloaddecrypt" added "unenc_hexpayload" result value with HEXASTRING of unencrypted payload
- Fixed some "Random Memory access violation errors" bugs found caused by multithreading and disconnected nodes

### Build 1.5.3.0 - 2017-03-06

- Fixed issue #23: RPC findoperation fails to find operation by opHash
- Miners best practices: Sending new job with new timestamp every 30 seconds
- Buffering last 10 sent jobs to miners
- Small delay prior to destroy a connection to prevent exception handling
- Minor logs changes


### Build 1.5.2.0 - 2017-03-03

- Added a jobs buffer for miners. This will allow to submit old job solutions (limited buffer). (Fix the "tx" issue)
- Miner jobs will not be sent every time a transaction is received, thet will be buffered and sent every few seconds (Fix the "tx" issue)
- Better network performance, allowing more operations and nodes thanks to buffering before relaying
- Daemon: Allow select on ini file how many connections can handle
- Fixed a locking when deleting connections

### Build 1.5.1.0 - 2017-02-20

- Memory leak fixed on RPC-JSON commands
- Memory leak fixed on node connections
- Improved network speed processing new blocks/operations
- Some minor bugs

### Build 1.5.0.0 - 2017-02-15

- Net protocol upgrade to 4-5
- Introducing "more work" with high priority than "more high". Work is calculated based on target. Higher target (more work) is more important than higher length
- Solved locking/crash bug on high connections (caused by bad thread locking)
- Improved network connection
- Added JSON-RPC port 4003 protection Whitelist (only allowed IP's can use JSON-RPC)


### Build 1.4.3.0 - 2017-02-02

- Adding "maturation" param to "JSON Operation object", return null when operation is not included on a blockchain yet, 0 means that is included in highest block and so on...
- Fixing miner timestamp value to prevent invalid time

### Build 1.4.2.0 - 2017-01-23

- Max JSON-RPC miner connections is now 1000 (before, 10)
- JSON-RPC miner enabled at pascalcoin_daemon (linux daemon)
- Screen messages for daemon
- pascalcoin_daemon.ini file for daemon

### Build 1.4.1.0 - 2017-01-18

- Improved JSON communications with Miner client (Port 4009 by default)
- Deleted adding numeration to JSON miner clients (only sends miner name)
- Minor changes

### Build 1.4.0.0 - 2016-12-30

- JSON-RPC changes:
  - Added method "signsendto" to allow a off-line wallet sign transaction operations without being syncrhonized
  - Added method "signchangekey" to allow a off-line wallet sign change key operations without being syncrhonized
  - Added method "executeoperations" to allow a on-line wallet execute operations signed off-line
  - Added method "operationsinfo" that will decode operations signed off-line
  - Added param "max" at JSON-RPC method "getblocks"
  - Changed param name "deep" for "depht" in "getaccountoperations". Deep will be available for compatibility.
  - Changed result for "changekeys". Now returns a "JSON operation object" array with each change key operation result
  - Changes on "JSON Operation object": May return a "valid" param and "errors" param
- Corrected a memory leak when processing JSON-RPC calls
- Better performance in connection protocol
- Updated protocol available to 1
- Updated net protocol to 3-4
- Important bug corrected

### Build 1.3.0.0 - 2016-11-24

- JSON-RPC modifications:
  - New param "b58_pubkey", can be used instead of "enc_pubkey": b58_pubkey is a Base58 encoded public key with checksum, is the value that Wallet exports/imports public key
  - New JSON object type Public Key: "name","can_use","enc_pubkey","b58_pubkey","ec_nid","x" and "y" for each public key
  - Added params "start" and "max" to "getaccountoperations". By default deep=100, max=100 and start=0, so will return last 100 operations made to an account
  - Added params "start" and "max" to "getblockoperations". By default max=100 and start=0, so will return last 100 operations made to a block
  - Added params "start" and "max" to "getwalletaccounts". By default max=100 and start=0, so will return first 100 accounts of the wallet
  - Added params "start" and "max" to "getwalletpubkeys". By default max=100 and start=0, so will return first 100 public keys of the wallet
    - Will return a Public key JSON object
  - Method "decodepubkey" allows params "enc_pubkey" or "b58_pubkey" (if used together, returns error if not match)
    - Will return a Public key JSON object
  - Method "changekey" allows params "new_enc_pubkey" or "new_b58_pubkey" (if used together, returns error if not match)
  - Method "getwalletaccounts" allows params "enc_pubkey" or "b58_pubkey" (if used together, returns error if not match)
  - Method "getwalletaccountscount" allows params "enc_pubkey" or "b58_pubkey" (if used together, returns error if not match)
  - Method "getwalletcoins" allows params "enc_pubkey" or "b58_pubkey" (if used together, returns error if not match)  
  - Method "payloadencrypt"  allows params "enc_pubkey" or "b58_pubkey" when payload_method="pubkey" (if used together, returns error if not match)  
  - Method "addnewkey" changed. Does not return an HEXASTRING with enc_pubkey, now returns a public key JSON object
  - New methods:
    - Method "changekeys". Similar to "changekey" but for multiple accounts at param "accounts" as a coma separated string (ex: "accounts"="1248,1753,85056"). Returns a JSON object with result information
    - Method "lock" to lock wallet. Returns true if locked, returns false if wallet has no password (an empty string, must put a new password prior to lock)
    - New method "getwalletpubkey". Search for "enc_pubkey" or "b58_pubkey" and returns a Public key JSON object
- Corrected a issue saving last 5 .bank files
- Improved protections for .bank corruption files
- Added "open data folder" button on options form
- Other minor changes

### Build 1.2.0.0 - 2016-11-16

- Account checksum values modified to be more easy and more distributed: Checksum = ((N * 101) MOD 89)+10
- Allow find operations by "ophash"
- Show Operation "ophash" in operation payload decoder
- Added param "enc_pubkey" to "getwalletaccounts" JSON-RPC method to return only accounts from this public key
- Added params "pow" and "sbh" to "nodestatus" JSON-RPC method
- Added method "getwalletaccountscount" returning accounts count of the entire wallet or for a single "enc_pubkey"
- Added method "getwalletcoins" returning coins of the entire wallet or for a signle "enc_pubkey"
- Modified seed nodes distribution to send only checked IP nodes
- Corrected invalid operation block index when showing account operations


### Build 1.1.0.0 - 2016-11-03

- JSON-RPC Server included
- Minor changes


### Build 1.0.9.0 - 2016-10-21

- Corrected a BUG (BUG-101) that causes blocking connections when received more than 100 connections, causing "alone in the world" after a cert period time.
- It's necessary to update because new version will refuse old versions to make network stable.


### Build 1.0.8.0 - 2016-10-20

- Cross compatible
- Can compile with Delphi or Lazarus (Free Pascal)
- New storage system. No more access database
- Network hashrate calculation


### Build 1.0.7.0 - 2016-10-10

- Introducing basic JSON-RPC to allow GPU miners development (Third party).
- See file "HOWTO_DEVELOP_GPU_MINER_FOR_PASCALCOIN.txt"
- No more CPU mining due exists GPU mining


### Build 1.0.6.0 - 2016-10-04

- Memory leaks corrections
- Introducing net protocol 2-3
- Source code modified, next build will be compiled with Lazarus and FPC


### Build 1.0.5.0 - 2016-09-21

- Massive operations, selecting multiple accounts
- Filter accounts by balance
- Correct operations explorer order of operations for each block (descending order)
- Minor changes


### Build 1.0.4.0 - 2016-09-16

- IMPORTANT: Introducing net protocol changes: Must update!
- More and more and more stable
- Prevents "Alone in the world" if everybody is updated ;-)
- Invalid local time detector with corrections
- IP nodes configurator


### Build 1.0.3.0 - 2016-09-08

- Important changes to database 
- Peer cache
- Issues with Connections
- More stable
- Miner key selector
- Invalid local time detector


### Build 1.0.2.0 - 2016-08-31

- Improved hashing speed
- Allow mining without opening external ports
- Choose how many CPU's want to mine
- Show real-time pending operations waiting to be included in the block
- More stable
- Some miner modifications

### Build 1.0.1.0 - 2016-08-12

- Included an option to Import/Export Wallet keys file
- Some miner modifications


### Build 1.0.0.0 - 2016-08-11

- First stable version.
- Created with Genesis block hardcoded
- Published at same time than Genesis block. NO PREMINE