# Pascal Coin: P2P Cryptocurrency without need of historical operations.  
  
Copyright (c) 2016 Albert Molina  
  
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
  
# Using daemon version

Note that daemon (pascalcoin_daemon) does not have a GUI to configure.
To configure daemon, must use pascalcoin_daemon.ini file

## pascalcoin_daemon.ini

Please look example of available/documented configurations params.
You can see example at GitHub file 
https://github.com/PascalCoin/PascalCoin/blob/master/pascalcoin_daemon.ini

## Install (linux only, tested on Ubuntu 16.04 64b)

Basic instalation is easy:

- Download/unzip in a fresh folder
- Configure pascalcoin_daemon.ini with properly params values
- Optional (new installation):
  - Download blockchain zip file from sourceforge https://sourceforge.net/projects/pascalcoin/files/
  - Unzip blockchain file and copy files (BlockChainStream.blocks and .bank) to $HOME/PascalCoin/Data
  - Data folder contains blockchain and safebox
  - Result will be: $HOME/PascalCoin/Data/ (blockchain + safebox)
- In terminal mode, go to unziped PascalCoin folder (fresh folder)
- Execute: nohup ./pascalcoin_daemon -r &
- Wait some minutes until connected/updated blockchain

## More info

- Oficial channels are:
  - Source code GitHub: https://github.com/PascalCoin/PascalCoin
  - Wiki information Github
  - Binaries/Installers SourceForge: https://sourceforge.net/projects/pascalcoin/files/




