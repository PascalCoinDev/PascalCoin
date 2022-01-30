echo Skybuck Flying Here !
echo
echo This script create a filter for this git repository
echo
echo 1. To turn two spaces into one tab on checkout
echo 2. To turn one tab into two spaces on commits
echo
echo For the following Pascal/Delphi file extensions:
echo
echo pas dpr dpk inc dfm xfm fmx lfm nfm dpkw
echo
echo This script requires GnuWin32 CoreUtils package to be installed.
echo 
echo Which can be found here:
echo http://gnuwin32.sourceforge.net/packages.html
echo and here:
echo http://gnuwin32.sourceforge.net/packages/coreutils.htm
echo
echo After installation its bin folder needs to be on the path environment
echo So that expand.exe and unexpand.exe can be found and executed.
echo
echo To run this script type: ./CreateSpacesToTabsFilter.sh
echo
md ".git/info"
mkdir .git/info 
echo "*.pas filter=tabspace2" > .git/info/attributes
echo "*.dpr filter=tabspace2" >> .git/info/attributes
echo "*.dpk filter=tabspace2" >> .git/info/attributes
echo "*.inc filter=tabspace2" >> .git/info/attributes
echo "*.dfm filter=tabspace2" >> .git/info/attributes
echo "*.xfm filter=tabspace2" >> .git/info/attributes
echo "*.fmx filter=tabspace2" >> .git/info/attributes
echo "*.lfm filter=tabspace2" >> .git/info/attributes
echo "*.nfm filter=tabspace2" >> .git/info/attributes
echo "*.dpkw filter=tabspace2" >> .git/info/attributes
