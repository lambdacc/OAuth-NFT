#!/bin/bash
printf "\nWelcome to this demo of NFT-OAuth. This demo shows an authentication NFT minted by an issuer, delivered to a client wallet and then a protected resource granting and denying access to wallets based on whether they have the correct NFT.\n"
printf "\nThe NFT can be used to verify the identity of the NFT holder because its minted using pkh of the issuer and pkh of the requester\n"
printf "\n The NFT issuing authority is named 'IssuerW', the client wallets are named 'ClientWOne','ClientWTwo' and the wallet holding protected resource is called 'ProtectedW'\n"
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
sleep 2
printf "\n"
printf "\nYou can see in the server logs, wallet one's token name\n"


printf "\n2. Next, client wallet one (ClientWOne) requests for auth NFT by giving its credentials. The 'mint' endpoint of Issuer wallet IssuerW will be invoked. ClientWOne's credential that is supplied in the payload is its wallet ID\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
printf "\n"

curl -H "Content-Type: application/json" -X POST -d '{"getWalletId": '$ClientWOne'}' http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/mint
sleep 2
printf "\n"
printf "\n2. The auth NFT was minted and delivered.\n"

printf "\n3.a. To the see the outcome we first inspect Issuer wallet IssuerW for value it now contains\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d "\"dummy\"" http://localhost:9080/api/contract/instance/$IssuerW_IID/endpoint/inspect
sleep 2
printf "\n"
printf "\nYou can see in the server logs, the minted NFT is not with issuer because it must have been delivered to the client wallet\n"

sleep 2
printf "\n3.b. Next we first inspect the client wallet ClientWOne to see the value it now contains\n"
read -n1 -r -p "Press any key to continue..." key
printf "\n"
curl -H "Content-Type: application/json" -X POST -d "\"dummy\"" http://localhost:9080/api/contract/instance/$ClientWOne_IID/endpoint/inspect
sleep 2
printf "\nYou can see in the server logs, the minted NFT was indeed delivered to the client wallet\n"


read -n1 -r -p "Press any key to continue..." key
printf "\n"
printf "\nSo far, you have seen that the Issuer wallet minted auth NFT with token name linked to the pkh of client wallet a (ClientWOne) and delivered it. In the next part of this demo, you will see that the Protected wallet checks the client wallets clientWOne and clientWTwo and identifies which one holds the authentication NFT \n"

printf "\n4.a The protected smart contract inspects both client wallet for the auth NFT. Recall that the auth NFT is identified by the minting policy which belongs to the NFT issuer and the token name that is constructed from the pkh of the client wallet\n"

read -n1 -r -p "Press any key to continue..." key
printf "\n4.a The protected smart contract inspects client wallet one (ClientWOne) and finds the auth NFT. So, access is granted\n"

curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "clientWallet":{"getWalletId": '$ClientWOne'}}' http://localhost:9080/api/contract/instance/$ProtectedWb_IID/endpoint/checkAccess
sleep 4
printf "\n"
printf "\nYou can see in the server logs, that the protected contract logged ACCESS GRANTED\n"
read -n1 -r -p "Press any key to continue..." key

printf "\n"
printf "\n4.b When the protected smart contract inspects client wallet two (ClientWTwo) it does not find the auth NFT. So this wallet is denied access.\n"

printf "\n"
curl -H "Content-Type: application/json" -X POST -d '{"issuerWallet":{"getWalletId": '$IssuerW'}, "clientWallet":{"getWalletId": '$ClientWTwo'}}' http://localhost:9080/api/contract/instance/$ProtectedWb_IID/endpoint/checkAccess
sleep 2
printf "\nYou can see in the server logs, that the protected contract logged ACCESS DENIED for wallet two\n"

read -n1 -r -p "Press any key to continue..." key
printf "\n"
printf "\nTo conclude. We have seen that in this project, we emulated the OAuth protocol. Using an NFT minted by an issuer a protected resources holder was able to check the if a wallet has a valid auth NFT and then grant or deny access.\n"

sleep 2
printf "\nFuture improvements can be encoding list of privileges, expiration time etc. in the NFT\n"
sleep 1
printf "\nThank you for your time.\n"
printf "\n"


