unit URPC;

interface

Uses UTCPIP;

{Type
  {TRPCServer = Class(TNetTcpIpServer)
  protected
    Procedure OnNewIncommingConnection(Sender : TObject; Client : TNetTcpIpClient); override;
    procedure SetActive(const Value: Boolean); override;
  public
    Constructor Create; override;
  End;}

implementation

end.
