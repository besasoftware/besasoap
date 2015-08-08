unit bsSOAPClientReg;

interface

procedure Register;

implementation

uses System.Classes,bsSOAPClient;

procedure Register;
begin
  RegisterComponents('Besa', [TbsService]);
end;

end.
