echo Skybuck Flying in the house ! =D
echo 
echo Welcome, this script will create an advanced PascalCoin git repository for you in 14 steps !
echo The script can be run in bash, cmd or powershell.
echo To run in bash type: ./CreateAdvancedRepository.sh 
echo make sure to use a dot and forward slash like so: ./
echo ./CreateAdvancedRepository.sh
echo 
echo To run in cmd or powershell rename to CreateAdvancedRepository.bat
echo To rename type: ren CreateAdvancedRepository.sh CreateAdvancedRepository.bat
echo Now to run in cmd or powershell type: .\CreateAdvancedRepository.bat 
echo make sure to use a dot and backward slash like so .\
echo .\CreateAdvancedRepository.bat
echo 
echo The advantages of this advanced PascalCoin repository and script are as follows:
echo 1. All local branches are re-created.
echo 2. Multiple worktrees/folders are created to work with multiple pascal coin versions at the same time.
echo 3. Remotes will be restored to fetch, pull, push as normal to remote repository.
echo 4. Mirror will be disabled after clone is done.
echo 5. Automatic line ending conversions for CR=Carrier Return and LF=Line Feed.
echo 5.0 Git stores line endings internally as LF, on windows line endings are CRLF.
echo 5.1 Line endings LF to CRLF on checkouts/switches
echo 5.2 Line endings CRLF to LF on checkins/commits
echo 
echo Lets get started:
echo 
echo Step 1 clone mirror your fork, this HTTPS URL must be editted by you.
git clone --mirror https://github.com/SkybuckFlying/PascalCoin.git

echo Step 2 change to the PascalCoin.git bare repository folder.
cd PascalCoin.git

echo Step 3 configure line endings for Microsoft Windows Operating Systems
echo Step 3 convert CRLF to LF on commit and convert LF to CRLF on checkouts
git config --global core.autocrlf true

echo Step 4 restore remotes
git config remote.origin.fetch +refs/heads/*:refs/remotes/origin/*

echo Step 5 disable mirror
git config remote.origin.mirror false

echo Step 6 fetch all remotes
git fetch --all

echo Step 7 add remote to PascalCoinDev
git remote add PascalCoinDev https://github.com/PascalCoinDev/PascalCoin.git

echo Step 8 add remote to PascalCoin
git remote add PascalCoin https://github.com/PascalCoin/PascalCoin.git

echo Step 9 fetch PascalCoinDev repository
git fetch PascalCoinDev

echo Step 10 fetch PascalCoin repository
git fetch PascalCoin

echo Step 11 create local PascalCoinDevMaster branch for remote PascalCoinDev/master branch
git branch PascalCoinDevMaster PascalCoinDev/master

echo Step 12 create local PascalCoinMaster branch for remote PascalCoin/master branch
git branch PascalCoinMaster PascalCoin/master

echo Step 13 add a worktree for local PascalCoinDevMaster branch
git worktree add ../PascalCoinDev PascalCoinDevMaster 

echo Step 14 add a worktree for PascalCoin master branch
git worktree add ../PascalCoin PascalCoinMaster

echo Step 15 is an optional manual step for you to perform:
echo Step 15 my SkybuckFlying repository does not have a default master branch but yours might...
echo Step 15 to add a worktree called Origin for your origin/master type the following two commands:
echo cd PascalCoin.git
echo git worktree add ../Origin master











