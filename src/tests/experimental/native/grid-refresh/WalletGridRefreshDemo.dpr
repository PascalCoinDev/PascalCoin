program WalletGridRefreshDemo;

{ Copyright © 2026 Herman Schoenfeld <https://sphere10.com>

  Fresh offline mock wallet demo. Use the existing private-desktop broker and
  one output log filename to execute its automatic refresh scenarios.

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.
}

uses UWalletGridRefreshHarness;

{$R 'WalletGridRefreshDemo.Assets.res'}

begin
  ExitCode := RunWalletGridRefreshDemo;
end.
