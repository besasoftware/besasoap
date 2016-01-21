unit bsWinInetTransporterReg;

interface
procedure Register;
implementation

uses Classes,bsWinInetTransporter;

procedure Register;
begin
  RegisterComponents('Besa', [TbsWinINetTransporter]);
end;

end.
