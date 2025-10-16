curl "https://marketplace.mercata.blockapps.net/cirrus/search/BlockApps-Mercata-Asset?select=*,BlockApps-Mercata-Asset-images(key,value),BlockApps-Mercata-Asset-files(key,value),BlockApps-Mercata-Asset-fileNames(key,value)" -H "Authorization: Bearer $TOKEN" > app/files/json/assets.json

curl "https://marketplace.mercata.blockapps.net/cirrus/search/BlockApps-Mercata-Escrow?select=*,BlockApps-Mercata-Escrow-assets(key,value)" -H "Authorization: Bearer $TOKEN" > app/files/json/escrows.json

curl "https://marketplace.mercata.blockapps.net/cirrus/search/BlockApps-Mercata-Reserve?isActive=eq.true" -H "Authorization: Bearer $TOKEN" > app/files/json/reserves.json

curl "https://node1.mercata.blockapps.net/cirrus/search/BlockApps-Mercata-Asset?name=in.(ETHST,WBTCST,USDCST,USDTST,PAXGST)&select=address,root,name,itemNumber,quantity,owner,ownerCommonName,decimals" -H "Authorization: Bearer $TOKEN" > app/files/json/crypto_balances.json

# curl "https://node1.mercata.blockapps.net/cirrus/search/BlockApps-Mercata-Asset-ItemTransfers?assetName=in.(ETHST,WBTCST,USDCST,USDTST,PAXGST)" -H "Authorization: Bearer $TOKEN" > app/files/json/crypto_transfers.json
curl "https://node1.mercata.blockapps.net/cirrus/search/BlockApps-Mercata-Asset?select=name,BlockApps-Mercata-Asset-OwnershipTransfer(block_timestamp,sellerCommonName,purchaserCommonName,minItemNumber,maxItemNumber)&name=in.(ETHST,WBTCST,USDCST,USDTST,PAXGST)&order=block_timestamp.desc" -H "Authorization: Bearer $TOKEN" > app/files/json/crypto_transfers.json

curl "https://marketplace.mercata.blockapps.net/cirrus/search/history@BlockApps-Mercata-Asset?or=(name.eq.Silver%20-%20Fractional%20100%20oz%20Bars,name.eq.GOLDST,name.eq.ETHST,name.eq.WBTCST,name.eq.USDCST,name.eq.USDTST,name.eq.PAXGST,name.eq.Gold%20-%201%20Gram,name.eq.Gold%20-%201%20oz%20Coin,name.eq.CATA,name.eq.USDST)&select=address,root,block_timestamp,name,ownerCommonName,itemNumber,quantity,sale&order=block_timestamp" -H "Authorization: Bearer $TOKEN" > app/files/json/asset_history.json
curl "https://marketplace.mercata.blockapps.net/cirrus/search/history@BlockApps-Mercata-Asset?name=neq.CATA&select=address,originAddress,block_timestamp,name,ownerCommonName,itemNumber,quantity,sale&order=block_timestamp" -H "Authorization: Bearer $TOKEN" > app/files/json/asset_history.json

curl "https://marketplace.mercata.blockapps.net/cirrus/search/history@BlockApps-Mercata-Sale?select=address,block_timestamp,contract_name,assetToBeSold,isOpen,price,quantity" -H "Authorization: Bearer $TOKEN" > app/files/json/sale_history.json

curl "https://marketplace.mercata.blockapps.net/cirrus/search/history@BlockApps-Mercata-Escrow" -H "Authorization: Bearer $TOKEN" > app/files/json/escrow_history.json

stack run -- $1
