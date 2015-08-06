unit bsClasses;

interface
uses
  System.SysUtils,System.Classes,System.TypInfo;

type
  TbsHeaderList=class(TStringList)
  private
    function GetHeader(const HeaderName: string): String;
    procedure SetHeader(const HeaderName, Value: String);
  public
    function GetHeaderName(const Header: string) : String;
    function HeaderIsExist(const HeaderName: string) : Boolean;
    function GetHeaderIndex(const HeaderName: string):Integer;
    property Header[const HeaderName: string]:String read GetHeader write SetHeader;
  end;


type
  TbsDateTimeFormat=(dfDateTime,dfDate,dfTime);
// from superobjects/supertimezone.pas...

  TbsSystemTime = record
    Year: Word;
    Month: Word;
    DayOfWeek :Word;
    Day: Word;
    Hour: Word;
    Minute: Word;
    Second: Word;
    Milliseconds: Cardinal;
  end;

  TbsISO8601=class
  private
    st: TbsSystemTime;
    dayofyear: Integer;
    week: Word;
    bias: Integer;
    havetz, havedate:Boolean;
    class function ParseISO8601Date(const ISO8601Date: String;
      var st: TbsSystemTime; var dayofyear: Integer; var week: Word;
      var bias: Integer; var havetz, havedate: Boolean): Boolean;
  public
    DateTimeFormat:TbsDateTimeFormat;
    procedure Parse(const ISO8601Date: String); overload;
    procedure Parse(const ADateTime: TDateTime); overload;
    function ToString: String;
    function ToDateTime : TDateTime;
  end;


function DelphiDateTimeToISO8601Date(const dt: TDateTime; DTFormat:TbsDateTimeFormat=dfDateTime): String;
function ISO8601DateToDelphiDateTime(const str: String; var dt: TDateTime): Boolean;

implementation
uses System.Generics.Collections;


{ TbsHeaderList }

function TbsHeaderList.GetHeaderIndex(const HeaderName: string): Integer;
var
  I : Integer;
begin
  Result := -1;
  for I := 0 to (Count-1) do
    if (GetHeaderName(Strings[I])=HeaderName) then begin
      Result := I;
      Exit;
    end;
end;

function TbsHeaderList.GetHeaderName(const Header: string): String;
begin
  Result:=Header.Substring(0,Header.IndexOf(':'));
end;

function TbsHeaderList.HeaderIsExist(const HeaderName: string): Boolean;
begin
  Result := GetHeaderIndex(HeaderName)>-1;
end;

function TbsHeaderList.GetHeader(const HeaderName: string): String;
var
  I : Integer;
  LStr:string;
begin
  Result:='';

  for I := 0 to (Count-1) do
  begin
    LStr:=Strings[I];
    if GetHeaderName(LStr)=HeaderName then
    begin
      Result := LStr.Substring(LStr.IndexOf(':')+1+1{space});
      Break;
    end;
  end;
end;

procedure TbsHeaderList.SetHeader(const HeaderName, Value: String);
var
  I : Integer;
begin
  I := GetHeaderIndex(HeaderName);
  if (I>=0) then
    Strings[I] := HeaderName+': '+Value
  else
    Add(HeaderName+': '+Value);
end;

{ TbsISO8601 }
procedure TbsISO8601.Parse(const ISO8601Date: String);
begin
  if ParseISO8601Date(ISO8601Date,st, dayofyear, week, bias, havetz, havedate) then
  begin

  end;

end;

procedure TbsISO8601.Parse(const ADateTime: TDateTime);
var
  ms :word;
begin
  FillChar(st,SizeOf(st),0);
  if ADateTime>0 then
  DecodeDate(ADateTime,st.Year,st.Month,st.Day);
  if Trunc(ADateTime)<>ADateTime then
  begin
    DecodeTime(ADateTime,st.Hour,st.Minute,st.Second,ms);
    st.Milliseconds:=ms;
  end;
end;

class function TbsISO8601.ParseISO8601Date(const ISO8601Date: String;
  var st: TbsSystemTime; var dayofyear: Integer; var week: Word;
  var bias: Integer; var havetz, havedate: Boolean): Boolean;

type
  TState = (stStart, stYear, stMonth, stWeek, stWeekDay, stDay, stDayOfYear,
    stHour, stMin, stSec, stMs, stUTC, stGMTH, stGMTM, stGMTend, stEnd);
  TPerhaps = (yes, no, perhaps);

var
  sb : TStringBuilder;
  p: Char;
  sep: TPerhaps;
  state: TState;
  pos, v,len,stri: Word;
  inctz: Boolean;

  function get(var v: Word; c: Char): Boolean; {$IFDEF HAVE_INLINE} inline; {$ENDIF}
  begin
    if (c < #256) and (Ord(c) in [Ord('0') .. Ord('9')]) then
    begin
      Result := True;
      v := v * 10 + Ord(c) - Ord('0');
    end
    else
      Result := False;
  end;
  function incp:char;
  begin
    if stri<sb.Length-1 then
    begin
       inc(stri);
       p:=sb.Chars[stri];
    end
    else p:=#0;

  end;



label
  error;
begin
  sb := TStringBuilder.Create;
  sb.Append(ISO8601Date);
  len:=sb.Length;
  stri:=0;
  p := sb.Chars[stri];
  sep := perhaps;
  state := stStart;
  pos := 0;
  inctz := False;

  FillChar(st, SizeOf(st), 0);
  dayofyear := 0;
  week := 0;
  bias := 0;
  havedate := True;
  havetz := False;

  while True do
    case state of
      stStart:
        case p of
          '0' .. '9':
            state := stYear;
          'T', 't':
            begin
              state := stHour;
              pos := 0;
              Incp;
              havedate := False;
            end;
        else
          goto error;
        end;
      stYear:
        case pos of
          0 .. 1, 3:
            if get(st.Year, p) then
            begin
              Inc(pos);
              Incp;
            end
            else
              goto error;
          2:
            case p of
              '0' .. '9':
                begin
                  st.Year := st.Year * 10 + Ord(p) - Ord('0');
                  Inc(pos);
                  Incp;
                end;
              ':':
                begin
                  havedate := False;
                  st.Hour := st.Year;
                  st.Year := 0;
                  Incp;
                  pos := 0;
                  state := stMin;
                  sep := yes;
                end;
            else
              goto error;
            end;
          4:
            case p of
              '-':
                begin
                  pos := 0;
                  Incp;
                  sep := yes;
                  state := stMonth;
                end;
              '0' .. '9':
                begin
                  sep := no;
                  pos := 0;
                  state := stMonth;
                end;
              'W', 'w':
                begin
                  pos := 0;
                  Incp;
                  state := stWeek;
                end;
              'T', 't', ' ':
                begin
                  state := stHour;
                  pos := 0;
                  Incp;
                  st.Month := 1;
                  st.Day := 1;
                end;
              #0:
                begin
                  st.Month := 1;
                  st.Day := 1;
                  state := stEnd;
                end;
            else
              goto error;
            end;
        end;
      stMonth:
        case pos of
          0:
            case p of
              '0' .. '9':
                begin
                  st.Month := Ord(p) - Ord('0');
                  Inc(pos);
                  Incp;
                end;
              'W', 'w':
                begin
                  pos := 0;
                  Incp;
                  state := stWeek;
                end;
            else
              goto error;
            end;
          1:
            if get(st.Month, p) then
            begin
              Inc(pos);
              Incp;
            end
            else
              goto error;
          2:
            case p of
              '-':
                if (sep in [yes, perhaps]) then
                begin
                  pos := 0;
                  Incp;
                  state := stDay;
                  sep := yes;
                end
                else
                  goto error;
              '0' .. '9':
                if sep in [no, perhaps] then
                begin
                  pos := 0;
                  state := stDay;
                  sep := no;
                end
                else
                begin
                  dayofyear := st.Month * 10 + Ord(p) - Ord('0');
                  st.Month := 0;
                  Incp;
                  pos := 3;
                  state := stDayOfYear;
                end;
              'T', 't', ' ':
                begin
                  state := stHour;
                  pos := 0;
                  Incp;
                  st.Day := 1;
                end;
              #0:
                begin
                  st.Day := 1;
                  state := stEnd;
                end;
            else
              goto error;
            end;
        end;
      stDay:
        case pos of
          0:
            if get(st.Day, p) then
            begin
              Inc(pos);
              Incp;
            end
            else
              goto error;
          1:
            if get(st.Day, p) then
            begin
              Inc(pos);
              Incp;
            end
            else if sep in [no, perhaps] then
            begin
              dayofyear := st.Month * 10 + st.Day;
              st.Day := 0;
              st.Month := 0;
              state := stDayOfYear;
            end
            else
              goto error;
          2:
            case p of
              'T', 't', ' ':
                begin
                  pos := 0;
                  Incp;
                  state := stHour;
                end;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stDayOfYear:
        begin
          if (dayofyear <= 0) then
            goto error;
          case p of
            'T', 't', ' ':
              begin
                pos := 0;
                Incp;
                state := stHour;
              end;
            #0:
              state := stEnd;
          else
            goto error;
          end;
        end;
      stWeek:
        begin
          case pos of
            0 .. 1:
              if get(week, p) then
              begin
                Inc(pos);
                Incp;
              end
              else
                goto error;
            2:
              case p of
                '-':
                  if (sep in [yes, perhaps]) then
                  begin
                    Incp;
                    state := stWeekDay;
                    sep := yes;
                  end
                  else
                    goto error;
                '1' .. '7':
                  if sep in [no, perhaps] then
                  begin
                    state := stWeekDay;
                    sep := no;
                  end
                  else
                    goto error;
              else
                goto error;
              end;
          end;
        end;
      stWeekDay:
        begin
          if (week > 0) and get(st.DayOfWeek, p) then
          begin
            Incp;
            v := st.Year - 1;
            v := ((v * 365) + (v div 4) - (v div 100) + (v div 400)) mod 7 + 1;
            dayofyear := (st.DayOfWeek - v) + ((week) * 7) + 1;
            if v <= 4 then
              Dec(dayofyear, 7);
            case p of
              'T', 't', ' ':
                begin
                  pos := 0;
                  Incp;
                  state := stHour;
                end;
              #0:
                state := stEnd;
            else
              goto error;
            end;
          end
          else
            goto error;
        end;
      stHour:
        case pos of
          0:
            case p of
              '0' .. '9':
                if get(st.Hour, p) then
                begin
                  Inc(pos);
                  Incp;
                end
                else
                  goto error;
              '-':
                begin
                  Incp;
                  state := stMin;
                end;
            else
              goto error;
            end;
          1:
            if get(st.Hour, p) then
            begin
              Inc(pos);
              Incp;
            end
            else
              goto error;
          2:
            case p of
              ':':
                if sep in [yes, perhaps] then
                begin
                  sep := yes;
                  pos := 0;
                  Incp;
                  state := stMin;
                end
                else
                  goto error;
              ',', '.':
                begin
                  Incp;
                  state := stMs;
                end;
              '+':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Incp;
                end
                else
                  goto error;
              '-':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Incp;
                  inctz := True;
                end
                else
                  goto error;
              'Z', 'z':
                if havedate then
                  state := stUTC
                else
                  goto error;
              '0' .. '9':
                if sep in [no, perhaps] then
                begin
                  pos := 0;
                  state := stMin;
                  sep := no;
                end
                else
                  goto error;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stMin:
        case pos of
          0:
            case p of
              '0' .. '9':
                if get(st.Minute, p) then
                begin
                  Inc(pos);
                  Incp;
                end
                else
                  goto error;
              '-':
                begin
                  Incp;
                  state := stSec;
                end;
            else
              goto error;
            end;
          1:
            if get(st.Minute, p) then
            begin
              Inc(pos);
              Incp;
            end
            else
              goto error;
          2:
            case p of
              ':':
                if sep in [yes, perhaps] then
                begin
                  pos := 0;
                  Incp;
                  state := stSec;
                  sep := yes;
                end
                else
                  goto error;
              ',', '.':
                begin
                  Incp;
                  state := stMs;
                end;
              '+':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Incp;
                end
                else
                  goto error;
              '-':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Incp;
                  inctz := True;
                end
                else
                  goto error;
              'Z', 'z':
                if havedate then
                  state := stUTC
                else
                  goto error;
              '0' .. '9':
                if sep in [no, perhaps] then
                begin
                  pos := 0;
                  state := stSec;
                end
                else
                  goto error;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stSec:
        case pos of
          0 .. 1:
            if get(st.Second, p) then
            begin
              Inc(pos);
              Incp;
            end
            else
              goto error;
          2:
            case p of
              ',', '.':
                begin
                  Incp;
                  state := stMs;
                end;
              '+':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Incp;
                end
                else
                  goto error;
              '-':
                if havedate then
                begin
                  state := stGMTH;
                  pos := 0;
                  v := 0;
                  Incp;
                  inctz := True;
                end
                else
                  goto error;
              'Z', 'z':
                if havedate then
                  state := stUTC
                else
                  goto error;
              #0:
                state := stEnd;
            else
              goto error;
            end;
        end;
      stMs:
        case p of
          '0' .. '9':
            begin
              st.Milliseconds := st.Milliseconds * 10 + Ord(p) - Ord('0');
              Incp;
            end;
          '+':
            if havedate then
            begin
              state := stGMTH;
              pos := 0;
              v := 0;
              Incp;
            end
            else
              goto error;
          '-':
            if havedate then
            begin
              state := stGMTH;
              pos := 0;
              v := 0;
              Incp;
              inctz := True;
            end
            else
              goto error;
          'Z', 'z':
            if havedate then
              state := stUTC
            else
              goto error;
          #0:
            state := stEnd;
        else
          goto error;
        end;
      stUTC: // = GMT 0
        begin
          havetz := True;
          Incp;
          if p = #0 then
            Break
          else
            goto error;
        end;
      stGMTH:
        begin
          havetz := True;
          case pos of
            0 .. 1:
              if get(v, p) then
              begin
                Incp;
                Inc(pos);
              end
              else
                goto error;
            2:
              begin
                bias := v * 60;
                case p of
                  ':': // if sep in [yes, perhaps] then
                    begin
                      state := stGMTM;
                      Incp;
                      pos := 0;
                      v := 0;
                      sep := yes;
                    end; // else goto error;
                  '0' .. '9':
                    // if sep in [no, perhaps] then
                    begin
                      state := stGMTM;
                      pos := 1;
                      sep := no;
                      Incp;
                      v := Ord(p) - Ord('0');
                    end; // else goto error;
                  #0:
                    state := stGMTend;
                else
                  goto error;
                end;

              end;
          end;
        end;
      stGMTM:
        case pos of
          0 .. 1:
            if get(v, p) then
            begin
              Incp;
              Inc(pos);
            end
            else
              goto error;
          2:
            case p of
              #0:
                begin
                  state := stGMTend;
                  Inc(bias, v);
                end;
            else
              goto error;
            end;
        end;
      stGMTend:
        begin
          if not inctz then
            bias := -bias;
          Break;
        end;
      stEnd:
        begin

          Break;
        end;
    end;


  if (st.Hour >= 24) or (st.Minute >= 60) or (st.Second >= 60) or
    (st.Milliseconds >= 1000000) or (week > 53) then
    goto error;

  Result := True;
  sb.Free;
  Exit;
error:
  Result := False;
  sb.Free;
end;


function TbsISO8601.ToDateTime: TDateTime;
begin
  Result:=EncodeDate(st.Year,st.Month,st.Day)+EncodeTime(st.Hour,st.Minute,st.Second,st.Milliseconds)
end;

function TbsISO8601.ToString: String;
const
  ISO_Fmt_Ms = '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d.%d';
  ISO_Fmt = '%.4d-%.2d-%.2dT%.2d:%.2d:%.2d';
  Date_Fmt = '%.4d-%.2d-%.2d';
  Time_Fmt = '%.2d:%.2d:%.2d';
  Time_Fmt_Ms = '%.2d:%.2d:%.2d.%d';
  TZ_Fmt = '%s%.2d:%.2d';
var
  //Local, UTC: TSystemTime;
  //tzi: TTimeZoneInformation;
  bias: TDateTime;
  h, m, d: Word;
  iso: String;
begin

  case DateTimeFormat of
    dfDateTime:
      if st.Milliseconds>0 then
        iso := Format(ISO_Fmt_Ms, [st.Year, st.Month, st.Day, st.Hour, st.Minute, st.Second, st.Milliseconds])
      else
        iso := Format(ISO_Fmt, [st.Year, st.Month, st.Day, st.Hour, st.Minute, st.Second]);

    dfDate: iso := Format(Date_Fmt, [st.Year, st.Month, st.Day]);
    dfTime:
      if st.Milliseconds>0 then
        iso := Format(Time_Fmt_Ms, [st.Hour,st.Minute, st.Second, st.Milliseconds])
      else
        iso := Format(Time_Fmt, [st.Hour,st.Minute, st.Second]);
  end;

  bias:=0;// For Now...

  if bias<0then  //Sign(bias)
    Result := iso + Format(TZ_Fmt, ['-', h, m])
  else if bias=0 then
    Result := iso + 'Z'
  else
    Result := iso + Format(TZ_Fmt, ['+', h, m]);

end;

function DelphiDateTimeToISO8601Date(const dt: TDateTime; DTFormat:TbsDateTimeFormat=dfDateTime): String;
var
  iso :TbsISO8601;
begin
  iso :=TbsISO8601.Create;
  try
    iso.Parse(dt);
    Result:=iso.ToString;
  finally
    iso.Free;
  end;
end;


function ISO8601DateToDelphiDateTime(const str: String; var dt: TDateTime): Boolean;
var
  iso :TbsISO8601;
begin
  Result:=False;
  iso :=TbsISO8601.Create;
  try
    iso.Parse(str);
    dt:=iso.ToDateTime;
    Result:=True;
  finally
    iso.Free;
  end;

end;

end.

