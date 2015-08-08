unit bsAuthenticationReg;

interface
uses
  System.Classes,
  bsAuthentication,
  bsBasicAuthentication,
  bsUserNameAuthentication;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Besa', [TbsAuthBasic]);
  RegisterComponents('Besa', [TbsAuthUsernameToken]);
end;

end.


