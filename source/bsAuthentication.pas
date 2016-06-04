(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Authentication Library 
/// </summary>
unit bsAuthentication;

{$I Besa.inc}

interface
uses Classes, bsClasses, XMLIntf;

type
  ///	<summary>
  ///	  Base authentication class.
  ///	</summary>

  IBSAuthentication= interface
  ['{9BFB8FC6-23FB-4495-B88A-E629416E5F00}']
    procedure SetHeaders(Headers : TbsHeaderList);
    procedure GenerateNode( aHeaderNode : IXMLNode);
    procedure SetUsername(const Value:String);
    function GetUsername:String;
    procedure SetPassword(const Value:String);
    function GetPassword:String;
    property Username: String read GetUsername write SetUsername;
    property Password: String read GetPassword write SetPassword;
  end;

  TOnGetHeader = procedure(var HeaderList: TbsHeaderList) of object;

  TbsCustomAuthenticate =class(TComponent,IBSAuthentication)
  private
    FOnSetHeader: TOnGetHeader;
    FUsername :String;
    FPassword :String;
    procedure SetUsername(const Value:String);
    function GetUsername:String;
    procedure SetPassword(const Value:String);
    function GetPassword:String;
  public
    procedure SetHeaders(Headers : TbsHeaderList);virtual;
    procedure GenerateNode( aHeaderNode : IXMLNode); virtual;
  published
    property Username: String read GetUsername write SetUsername;
    property Password: String read GetPassword write SetPassword;
    property OnGetHeader: TOnGetHeader read FOnSetHeader write FOnSetHeader;
  end;

  TbsCustomAuthenticateClass = class of TbsCustomAuthenticate;

  TbsAuthNone =class(TbsCustomAuthenticate)
  end;

implementation

{ TbsAuthenticate }

procedure TbsCustomAuthenticate.SetUsername(const Value:String);
begin
  FUsername:=Value;
end;

function TbsCustomAuthenticate.GetUsername:String;
begin
  Result:=FUsername;
end;

procedure TbsCustomAuthenticate.SetPassword(const Value:String);
begin
  FPassword:=Value;
end;

function TbsCustomAuthenticate.GetPassword:String;
begin
  Result:=FPassword;
end;

procedure TbsCustomAuthenticate.SetHeaders(Headers : TbsHeaderList);
begin
end;

procedure TbsCustomAuthenticate.GenerateNode(aHeaderNode: IXMLNode);
begin
end;





end.
