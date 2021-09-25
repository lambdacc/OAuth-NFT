# OAuth-NFT project

### Introduction
OAuth 2.0 type authentication using NFT

This project showcases the potential use of NFT, for implementing OAUth 2.0 type authentication.

This project includes the smart contract with the relevant endpoints, the configuration from building a PAB executable and bash script that runs a demo with the PAB simulator. 

#### OAuth 2.0
The OAuth 2.0 authorization framework enables a third-party application to obtain limited access to an HTTP service, either on behalf of a resource owner by orchestrating an approval interaction between the resource owner and the HTTP service, or by allowing the third-party application to obtain access on its own behalf.

For example, you would have used your social media accounts like Google, Facebook or Twitter to sign up and login to other websites. Behind scenes OAuth 2.0 enables the social login. Further details about OAuth 2.0 be found [here](https://oauth.net/2/)


### Idea of OAuth-NFT

We implement a scheme similar to the OAuth protocol where minted NFTs act the authentication tokens.
By demonstrating a workflow, where a wallet requests for authenticaton tokenfor achieving the functionality of the OAuth protocol used for authentication in web applications. 

There is an endpoint that mints NFT for requesting wallets using the supplied Wallet ID and delivers it to the requestor wallet. The minted NFT has a token name equal to the public key hash of the requested wallet. This enables the receiving wallet to use the NFT for further transactions. An NFT will represent only 1 wallet.

Repository

Illustration of the use case.
The client wallet is the one that requests for an authentication NFT. The Auth SC is the smart contract that mints the auth NFT and delivers to the requested client. The client can then access protected resources through the Protected SC by supplying the auth NFT. The Protected SC can verify that the Client Wallet is the true owner of the NFT.

This project gives uses the [Plutus Platform starter project](https://github.com/input-output-hk/plutus-starter) as the template.

### Setting up
Please refer to [Setting up](https://github.com/input-output-hk/plutus-starter#setting-up) section of the plutus starter project.

### The Plutus Application Backend (PAB) example

With the PAB we can serve and interact with contracts over a web API. You can read more about the PAB here: [PAB Architecture](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

Here, the PAB is configured with one contract, the `Game` contract from `./examples/src/Plutus/Contracts/Game.hs`.

Here's an example of running and interacting with this contract via the API. For this it will help if you have `jq` installed.

1. Build the PAB executable:

```
cabal build plutus-starter-pab
```

2. Run the PAB binary:

```
cabal exec -- plutus-starter-pab
````

This will then start up the server on port 9080. The devcontainer process will then automatically expose this port so that you can connect to it from any terminal (it doesn't have to be a terminal running in the devcontainer).

First, let's verify that the game is present in the server:

3. Check what contracts are present:

```
curl -s http://localhost:9080/api/contract/definitions | jq
```

You should receive a list of contracts and the endpoints that can be called on them, and the arguments
required for those endpoints.


### How to run the demo

- Perform the setup for Plutus as described in the plutus starter preject. environemt
- Clone this repo to your local environment.
- Start a nix shell from the plutus repo from the first step  
- Bring up the PAB server and execute `run.sh` for the demo.
  - You might need to do `chmod +x run.sh` first in case execute permissions are missing.

Finally, also node that the PAB also exposes a websocket, which you can read about in
the general [PAB Architecture documentation](https://github.com/input-output-hk/plutus/blob/master/plutus-pab/ARCHITECTURE.adoc).

### Takeaway

Potentially NFTs can be used for achieving the functionality of the OAuth protocol used for authentication in web applications. For example, the way you log in to a website using your Google or Facebook account.
There is an endpoint that mints NFT for requesting wallets using the supplied Wallet ID and delivers it to the requestor wallet. The minted NFT has a token name equal to the public key hash of the requested wallet. This enables the receiving wallet to use the NFT for further transactions. An NFT will represent only 1 wallet.

**Illustration**:
Refer to the diagram below. The client wallet is the one that requests for an authentication NFT. The Auth SC is the smart contract that mints the auth NFT and delivers to the requested client. The client can then access protected resources through the Protected SC by supplying the auth NFT. The Protected SC can verify that the Client Wallet is the true owner of the NFT.
![img.png](img.png)


#### Future work
The token name used in this demo was built using the public key hash of the requestor wallet. The token name can be encoded with more information like the expiry time of the auth NFT, the list of privileges obtained by holding this NFT etc.  

### Tips
#### To use entr to watch your source file while development
```ls src/<file name>.hs | entr -r cabal build oauth-nft --dependencies-only --disable-documentation```

#### Support/Issues/Community

If you're looking for support, or would simply like to report a bug, feature
request, etc. please do so over on the main [plutus repository](https://github.com/input-output-hk/plutus).


Thanks!
