unit bsIndyTransporterReg;

interface
procedure Register;
implementation

uses Classes,bsIndyTransporter;

procedure Register;
begin
  RegisterComponents('Besa', [TbsIndyTransporter]);
end;

end.
