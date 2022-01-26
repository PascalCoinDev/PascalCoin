echo "Skybuck Flying here !"
echo ""
echo "Welcome, this batchfile will create a usuable PascalCoin repository for you."
echo "" 
echo "Step 1 clone your fork, this must be editted by you"
git clone https://github.com/SkybuckFlying/PascalCoin.git

echo "Step 2 change to the PascalCoin folder"
cd PascalCoin

echo "Step 3 add remote to PascalCoinDev"
git remote add PascalCoinDev https://github.com/PascalCoinDev/PascalCoin.git

echo "Step 4 add remote to PascalCoin"
git remote add PascalCoin https://github.com/PascalCoin/PascalCoin.git

echo "Step 5 fetch PascalCoinDev repository"
git fetch PascalCoinDev

echo "Step 6 fetch PascalCoin repository"
git fetch PascalCoin

echo "Step 7 create local branch for PascalCoinDev/master"
git checkout PascalCoinDev/master -b PascalCoinDevMaster

echo "Step 8 create local branch for PascalCoin/master"
git checkout PascalCoin/master -b PascalCoinMaster

echo "Step 9 configure line endings for Microsoft Windows Operating Systems"
echo "Step 9 (convert CRLF to LF on commit) and (LF to CRLF on checkouts)"
git config --global core.autocrlf true








