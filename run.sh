#!/bin/bash
printf "\nWelcome to this demo of NFT-OAuth. This demo shows an authentication NFT minted by an issuer and delivered to a client wallet.\n"
printf "\nThe NFT can be used to verify the identity of the NFT holder\n"
printf "\nHere, Issuer wallet (IssuerW) is the NFT issuing authority and client wallet a (ClientWOne) - ClientWOne is the client wallet\n"
printf "\n"
export IssuerW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export ClientWOne=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export ClientWTwo=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1
export ProtectedW=`curl -s -d '' http://localhost:9080/wallet/create | jq '.wiWallet.getWalletId'`
sleep 1


export IssuerW_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "AuthNFTIssuerContract", "caWallet":{"getWalletId": '$IssuerW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export ClientWOne_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "AuthNFTIssuerContract", "caWallet":{"getWalletId": '$ClientWOne'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export ClientWTwo_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "AuthNFTIssuerContract", "caWallet":{"getWalletId": '$ClientWTwo'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
export ProtectedWb_IID=$(curl -s -H "Content-Type: application/json" -X POST -d '{"caID": "ProtectedResourceContract", "caWallet":{"getWalletId": '$ProtectedW'}}' http://localhost:9080/api/contract/activate | jq .unContractInstanceId | tr -d '"')
sleep 1


printf "\n1. First, lets log the nft token name of client wallet a (ClientWOne). Note the token name\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '[]' http://localhost:9080/api/contract/instance/$ClientWOne_IID/endpoint/logWalletNftTokenName
sleep 4
printf "\n"


printf "\n2. Next, client wallet a (ClientWOne) requests for auth NFT by giving its credentials. The 'mint' endpoint of Issuer wallet IssuerW will be invoked\n"
printf "ClientWOne's credential here is its wallet ID\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"

curl -H "Content-Type: application/json" -X POST -d '{"getWalletId": '$ClientWOne'}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 4
printf "\n"


printf "\n3.a. To the see the outcome we first inspect Issuer wallet IssuerW for value it now contains\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d "\"dummy\"" http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/inspect
sleep 4
printf "\n"


printf "\n3.b. Finally we first inspect the client wallet ClientWOne to see the value it now contains\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d "\"dummy\"" http://localhost:9080/api/contract/instance/$ClientWOne_IID/endpoint/inspect
sleep 4
printf "\n"

read -n1 -r -p "Press any key to continue..." key
printf "\nSo far, you have seen that the Issuer wallet minted auth NFT with token name linked to the pkh of client wallet a (ClientWOne) and delivered it\n"


printf "\n4. In the next part of this demo, you will see that the Protected wallet checks the client wallets clientWOne and clientWTwo and identifies which one holds the authentication NFT \n"

printf "\n4.a The protected smart contract inspects client wallet one (ClientWOne) and finds the auth NFT. Requisite resource can be granted to this wallet \n"
printf "\nThe auth NFT is identified by the minting policy which belongs to the NFT issuer and the token name that is constructed from the pkh of the client wallet\n"

read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "clientWallet":{"getWalletId": '$ClientWOne'}}' http://localhost:9080/api/contract/instance/$ProtectedWb_IID/endpoint/checkAccess
sleep 4
printf "\n"

read -n1 -r -p "Press any key to continue..." key
printf "\n4.b When the protected smart contract inspects client wallet two (ClientWTwo) it does not find the auth NFT. So this wallet is denied access.\n"

read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "clientWallet":{"getWalletId": '$ClientWTwo'}}' http://localhost:9080/api/contract/instance/$ProtectedWb_IID/endpoint/checkAccess
sleep 4
printf "\n"

printf "\nTo conclude. We have seen that in this project, we emulated the OAuth protocol. Using an NFT minted by an issuer a protected resources holder was able to check the if a wallet has a valid auth NFT and then grant or deny access.\n"

printf "\nFuture improvements can be encoding list of privileges, expiration time etc. in the NFT (tokenName)\n"
printf "\nThank you for your time\n".


