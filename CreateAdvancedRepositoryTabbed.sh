echo Skybuck Flying in the house ! =D
echo 
echo Tabbed version of CreateAdvancedRepository.sh
echo
echo Welcome, this script will create an advanced PascalCoin git repository for you in 16 steps !
echo The script can be run in bash, cmd or powershell.
echo To run in bash type: ./CreateAdvancedRepositoryTabbed.sh 
echo make sure to use a dot and forward slash like so: ./
echo ./CreateAdvancedRepositoryTabbed.sh
echo 
echo To run in cmd or powershell rename to CreateAdvancedRepositoryTabbed.bat
echo To rename type: ren CreateAdvancedRepo.sh CreateAdvancedRepositoryTabbed.bat
echo Now to run in cmd or powershell type: .\CreateAdvancedRepositoryTabbed.bat 
echo make sure to use a dot and backward slash like so .\
echo .\CreateAdvancedRepositoryTabbed.bat
echo 
echo The advantages of this advanced PascalCoin repository and script are as follows:
echo 1. All local branches are re-created.
echo 2. Multiple worktrees/folders are created to work with multiple pascal coin versions at the same time.
echo 3. Remotes will be restored to fetch, pull, push as normal to remote repository.
echo 4. Mirror will be disabled after clone is done.
echo 5. Automatic line ending conversions for CR=Carrier Return and LF=Line Feed.
echo 5.0 Git stores line endings internally as LF, on windows line endings are CRLF.
echo 5.1. Line endings LF to CRLF on checkouts/switches
echo 5.2. Line endings CRLF to LF on checkins/commits
echo 6. Automatic conversion of spaces to tabs and tabs to spaces.
echo 6.1 Two space characters are converted to one tab character on checkouts/switches.
echo 6.2 One tab character is converted to two space characters on checkins/commits.
echo
echo For advantage 6 it will be necessary to install GNU Win32 CoreUtils
echo GNUCoreUtils can be downloaded from these links:
echo http://gnuwin32.sourceforge.net/packages.html
echo which can also be found here
echo http://gnuwin32.sourceforge.net/packages/coreutils.htm
echo copy/rename GnuWin32/bin/expand.exe to GnuWin32/bin/CoreUtilsExpand.exe
echo copy/rename GnuWin32/bin/unexpand.exe to GnuWin32/bin/CoreUtilsUnExpand.exe 
echo then add core utils install path/GnuWin32/Bin to OS/System path environment variable
echo After installation restart the command prompt/bash/powershell to benefit from updated path environment variable.
echo 
echo Lets get started:
echo 
echo Step 1 clone mirror your fork, this HTTPS URL must be editted by you.
git clone --mirror https://github.com/SkybuckFlying/PascalCoin.git

echo Step 2 change to the PascalCoin.git bare repository folder.
cd PascalCoin.git

echo Step 3 add filter attributes
echo *.pas filter=tabspace2 > info/attributes
echo *.dpr filter=tabspace2 >> info/attributes
echo *.dpk filter=tabspace2 >> info/attributes
echo *.inc filter=tabspace2 >> info/attributes
echo *.dfm filter=tabspace2 >> info/attributes
echo *.xfm filter=tabspace2 >> info/attributes
echo *.fmx filter=tabspace2 >> info/attributes
echo *.lfm filter=tabspace2 >> info/attributes
echo *.nfm filter=tabspace2 >> info/attributes
echo *.dpkw filter=tabspace2 >> info/attributes

echo Step 4 add local repository smudging and cleaning
echo For this to work install GNUCoreUtils first and 
echo rename or copy expand.exe to CoreUtilsExpand.exe
echo rename or copy unexpand.exe to CoreUtilsUnExpand.exe
echo and add their location to the system path environment
echo rename is necessary to avoid name space conflict with Microsofts expand.exe
echo if GNUCoreUtils is installed later then branches/worktrees and such will
echo have to be re-checked-out to have any effect.
echo GNUCoreUtils can be downloaded from these links:
echo http://gnuwin32.sourceforge.net/packages.html
echo and here:
echo http://gnuwin32.sourceforge.net/packages/coreutils.htm
echo After installation restart the command prompt/bash/powershell to benefit from updated path environment variable.
git config filter.tabspace2.clean "CoreUtilsExpand --tabs=2 --initial"
git config filter.tabspace2.smudge "CoreUtilsUnExpand --tabs=2 --first-only"

echo Step 5 configure line endings for Microsoft Windows Operating Systems
echo Step 5 convert CRLF to LF on commit and convert LF to CRLF on checkouts
git config --global core.autocrlf true

echo Step 6 restore remotes
git config remote.origin.fetch +refs/heads/*:refs/remotes/origin/*

echo Step 7 disable mirror
git config remote.origin.mirror false

echo Step 8 fetch all remotes
git fetch --all

echo Step 9 add remote to PascalCoinDev
git remote add PascalCoinDev https://github.com/PascalCoinDev/PascalCoin.git

echo Step 10 add remote to PascalCoin
git remote add PascalCoin https://github.com/PascalCoin/PascalCoin.git

echo Step 11 fetch PascalCoinDev repository
git fetch PascalCoinDev

echo Step 12 fetch PascalCoin repository
git fetch PascalCoin

echo Step 13 create local PascalCoinDevMaster branch for remote PascalCoinDev/master branch
git branch PascalCoinDevMaster PascalCoinDev/master

echo Step 14 create local PascalCoinMaster branch for remote PascalCoin/master branch
git branch PascalCoinMaster PascalCoin/master

echo Step 15 add a worktree for local PascalCoinDevMaster branch
git worktree add ../PascalCoinDev PascalCoinDevMaster 

echo Step 16 add a worktree for PascalCoin master branch
git worktree add ../PascalCoin PascalCoinMaster

echo Step 17 is an optional manual step for you to perform:
echo Step 17 my SkybuckFlying repository does not have a default master branch but yours might...
echo Step 17 to add a worktree called Origin for your origin/master type the following two commands:
echo cd PascalCoin.git
echo git worktree add ../Origin master
