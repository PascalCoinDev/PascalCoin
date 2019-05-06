# PIP-0031 New Wallet Proposal
This proposal is to fund the development of a comprehensive wallet solution to address a big shortcomming in PascalCoin today. The motivations for this PIP are listed below, as well as an analysis of proposals by vendors.
The objective of PIP-0031 is to develop a wallet solution that solves many problems at the same time.  

The main motivations to resolve in a wallet are:

1. **"Kim Kardashian" UX**: Albert's wallet is great for technical users, but most people complain about that it's too difficult to use, and this hampers adoption. We need a wallet suitable for noob users and which has a "Kim Kardashian" appeal.

2. **Web-Wallet**: we need a wallet which people can use from a web-browser without needing to install a software or download an app. This means they don't need to run a node or install an app to withdraw their PASC from the exchange. This is a very important factor for a wallet solution, since many speculators (90% of market) want a quick way to withdraw their funds.

3. **Mobile-Wallet**: the wallet should be distributed as mobile app and provide a native UX.

4. **Desktop-Wallet**: the wallet should be distributed as desktop app and provide a native UX.

5. **P2P**: the ideal solution should not rely on servers, and should be able to run by connecting directly to P2P network.

6. **Layer-2 Support**: the wallet-code should also serve as an extensible framework for enabling Layer-2 apps and Layer-2 support.

7. **Internationalizable**: the wallet should be translatable into many languages. 

8. **Bang-For-Buck** The wallet should be cost-effective, in that it provides most bang-for-buck.

9. **Maintainability** The developers are proven community members who will maintain wallet going forward, and are in this because they believe in project.


## PascalCoin Foundation Analysis (By Herman):

### PIP-0031B

**Links**

https://github.com/PascalCoin/PascalCoin/blob/master/PIP/PIP-0031B.md

https://github.com/PascalCoinPool/pascal-gui-wallet

**TOTAL:** $20k

**SCORE**

1. Kim Kardashian UX: 8/10
2. Web-Wallet: 10/10
3. Mobile-Wallet: 6/10
4. Desktop-Wallet: 6/10
5. P2P: 5/10
6. Layer2: 10/10
7. Internationalizable: 10/10
8. Bang-For-Buck: 7/10
9. Maintainability: 10/10

**Overall Score:**  8/10

**Notes:** Good option, satisfies mostly everying. P2P is contigent on someone else providing a mobile-compliant daemon in the future (which is likely). For additional $5k, the developer has offered to provide a demo Layer-2 Chat app, which would be great after wallet is delivered! This would be like a decentralised Telegram PoC app. However, this is **NOT** part of this deliverable. Developer is trusted, quality dev and likely to maintain out of good will.

### PIP-0031C

**Links**

https://github.com/bbedward/PascalCoin/blob/pip-0031c/PIP/PIP-0031C.md
https://github.com/bbedward/PascalCoin/blob/pip-overview/PIP/PIP-0031C_overview.md

**TOTAL** $25k

**SCORE**

1. Kim Kardashian UX: 10/10
2. Web-Wallet: 0/10
3. Mobile-Wallet: 10/10
4. Desktop-Wallet: 5/10
5. P2P: 0/10
6. Layer2: 0/10
7. Internationalizable: 10/10
8. Bang-For-Buck: 4/10
9. Maintainability: 4/10

**Overall Score:**  5/10

**Notes:** Well-written pitch but when you drill-down, it's lacking in several areas that were fundamental to PIP-0031 initiative. For example, there is no web-wallet, no layer-2, no P2P and desktop support is contingent on technolog which may be released by Flutter in the future. In the opinion of the reviewer, it's mainly just "bling" mobile app. The desktop app is contingent on technology release from 3rd party vendor that hasn't been released yet. As a benefit, PascalCoin would get Python libraries for interacting with daemon, which would benefit eco-system a little bit. Developers are very professional but not really community members, and realistically unlikely to stick around after payment. 

### PIP-0031D:

**Links**

https://github.com/davidbolet/PascWallet/blob/master/PIP-0031D.md
https://github.com/davidbolet/PascWallet/blob/master/README.md

**TOTAL** $15k

1. Kim Kardashian UX: 8/10
2. Web-Wallet: 0/10
3. Mobile-Wallet: 10/10
4. Desktop-Wallet: 10/10
5. P2P: 10/10
6. Layer2: 7/10
7. Internationalizable: 8/10
8. Bang-For-Buck: 10/10
9. Maintainability: 9/10

**Overall Score:**  8/10

**Notes:** Mobile and Desktop wallets provided with decent decent look-and-feel. P2P will be internal without dependency on daemon. Also has Layer-2 support, but perhaps less flexible than (A), but still usable. Developer is trusted, long-term community member who will maintain software out of good will, however code-base will be in flutter/java which makes maintance more time-consuming. Overall ROI is the best out of all 3 due to price! Recommended!
