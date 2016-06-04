(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Basic Authentication Library 
/// </summary>
unit bsBasicAuthentication;
{$I Besa.inc}
interface
uses
   Classes, bsClasses,bsAuthentication;

type
  ///  <summary>
  ///  TbsAuthBasic implements Basic authenticaton, adds Authorization
  ///  information in http headers.
  ///  </summary>
  [ComponentPlatformsAttribute(pidWin32 or pidWin64 or pidOSX32 {$IFDEF DELPHIXE5_UP}or pidiOSSimulator or pidiOSDevice or pidAndroid{$ENDIF})]
  TbsAuthBasic=class(TbsCustomAuthenticate)
  public
    procedure SetHeaders(Headers : TbsHeaderList);override;
  end;
implementation
uses bsUtil;

{ TbsAuthBasic }
procedure TbsAuthBasic.SetHeaders(Headers : TbsHeaderList);
begin
  Headers.Header['Authorization']:='Basic '+Base64Encode(Username+':'+Password);
  if Assigned(OnGetHeader) then
    OnGetHeader(Headers);
end;
end.
