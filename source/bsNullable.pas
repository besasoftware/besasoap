unit bsNullable;
// from spring4d
interface

uses
  Generics.Collections, Rtti, Variants, Generics.Defaults;

const
  SCannotAssignPointerToNullable = 'Cannot Assign Pointer To Nullable';
  SNullableTypeHasNoValue        = 'Nullable Type Has No Value';

type
  {$REGION 'Nullable Types'}

  ///	<summary>
  ///	  A nullable type can represent the normal range of values for its
  ///	  underlying value type, plus an additional <c>Null</c> value.
  ///	</summary>
  ///	<typeparam name="T">
  ///	  The underlying value type of the <see cref="Nullable&lt;T&gt;" />generic
  ///	   type.
  ///	</typeparam>
  Nullable<T> = packed record
  private
    const CHasValueFlag = '@';  // DO NOT LOCALIZE
  strict private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;
  private
    ///	<summary>
    ///	  Internal use. Marks the current instance as null.
    ///	</summary>
    ///	<remarks>
    ///	  The Nullable{T} type is immutable so that this method must be
    ///	  private.
    ///	</remarks>
    procedure Clear;

    ///	<summary>
    ///	  Determines whether a variant value is null or empty.
    ///	</summary>
    class function VarIsNullOrEmpty(const value: Variant): Boolean; static;
  public
    ///	<summary>
    ///	  Initializes a new instance of the <c>Nullable{T}</c> structure to the
    ///	  specified value.
    ///	</summary>
    constructor Create(const value: T); overload;

    ///	<summary>
    ///	  Initializes a new instance of the <c>Nullable{T}</c> structure to the
    ///	  specified value.
    ///	</summary>
    constructor Create(const value: Variant); overload;

    ///	<summary>
    ///	  Retrieves the value of the current <c>Nullable{T}</c> object, or the
    ///	  object's default value.
    ///	</summary>
    function GetValueOrDefault: T; overload;

    ///	<summary>
    ///	  Retrieves the value of the current <c>Nullable{T}</c> object, or the
    ///	  specified default value.
    ///	</summary>
    ///	<param name="defaultValue">
    ///	  A value to return if the <see cref="HasValue" /> property is
    ///	  <c>false</c>.
    ///	</param>
    ///	<returns>
    ///	  The value of the <see cref="Value" /> property if the
    ///	  <see cref="HasValue" /> property is true; otherwise, the
    ///	  <paramref name="defaultValue" /> parameter.
    ///	</returns>
    ///	<remarks>
    ///	  The <see cref="GetValueOrDefault" /> method returns a value even if
    ///	  the <see cref="HasValue" /> property is false (unlike the
    ///	  <see cref="Value" /> property, which throws an exception).
    ///	</remarks>
    function GetValueOrDefault(const defaultValue: T): T; overload;

    ///	<summary>
    ///	  Determines whether two nullable value are equal.
    ///	</summary>
    ///	<remarks>
    ///	  <p> If both two nullable values are null, return true; </p>
    ///	  <p> If either one is null, return false; </p>
    ///	  <p> else compares their values as usual. </p>
    ///	</remarks>
    function Equals(const other: Nullable<T>): Boolean;

    ///	<summary>
    ///	  Gets a value indicating whether the current <c>Nullable{T}</c>structure
    ///	  has a value.
    ///	</summary>
    property HasValue: Boolean read GetHasValue;

    ///	<summary>
    ///	  Gets the value of the current <c>Nullable&lt;T&gt;</c> value.
    ///	</summary>
    ///	<exception cref="Spring|EInvalidOperationException">
    ///	  Raised if the value is null.
    ///	</exception>
    property Value: T read GetValue;

    { Operator Overloads }
    class operator Implicit(const value: Nullable<T>): T;
    class operator Implicit(const value: T): Nullable<T>;
    class operator Implicit(const value: Nullable<T>): Variant;
    class operator Implicit(const value: Variant): Nullable<T>;
    class operator Implicit(value: Pointer): Nullable<T>;
    class operator Explicit(const value: Nullable<T>): T;
    class operator Equal(const a, b: Nullable<T>) : Boolean;
    class operator NotEqual(const a, b: Nullable<T>) : Boolean;
  end;


  NullableString    = Nullable<String>;
  NullableInteger   = Nullable<Integer>;
  NullableInt64     = Nullable<Int64>;
  NullableNativeInt = Nullable<NativeInt>;
  NullableDateTime  = Nullable<TDateTime>;
  NullableCurrency  = Nullable<Currency>;
  NullableDouble    = Nullable<Double>;
  NullableBoolean   = Nullable<Boolean>;
  NullableGuid      = Nullable<TGUID>;
  NullableByte      = Nullable<Byte>;
  NullableWord      = Nullable<Word>;
  NullableLongWord  = Nullable<LongWord>;
  NullableCardinal  = Nullable<Cardinal>;
  NullableChar      = Nullable<Char>;
  NullableExtended  = Nullable<Extended>;
  NullableShortInt  = Nullable<ShortInt>;
  NullableSingle    = Nullable<Single>;
  NullableSmallInt  = Nullable<SmallInt>;


{$IFNDEF NEXTGEN}
  NullableAnsiChar   = Nullable<AnsiChar>;
  NullableWideChar   = Nullable<WideChar>;
  NullableAnsiString = Nullable<AnsiString>;
  NullableWideString = Nullable<WideString>;
{$ENDIF}

{$ENDREGION}


function TryGetUnderlyingValue(const Value: TValue;
  out underlyingValue: TValue): Boolean;

function TrySetUnderlyingValue(const Value: TValue;
  const underlyingValue: TValue): Boolean;
implementation

uses
  TypInfo, StrUtils, SysUtils;



function IsNullableType(typeInfo: PTypeInfo): Boolean;
const
  PrefixString = 'Nullable<'; // DO NOT LOCALIZE
begin
  Result := Assigned(typeInfo) and (typeInfo.Kind = tkRecord) and
    StartsText(PrefixString, GetTypeName(typeInfo));
end;

function TryGetUnderlyingTypeName(typeInfo: PTypeInfo;
  out underlyingTypeName: string): Boolean;
const
  PrefixString = 'Nullable<'; // DO NOT LOCALIZE
  PrefixStringLength = Length(PrefixString);
var
  typeName: string;
begin
  Result := IsNullableType(typeInfo);
  if Result then
  begin
    typeName := GetTypeName(typeInfo);
    underlyingTypeName := Copy(typeName, PrefixStringLength + 1,
      Length(typeName) - PrefixStringLength - 1);
  end;
end;

function TryGetUnderlyingTypeInfo(typeInfo: PTypeInfo;
  out underlyingTypeInfo: PTypeInfo): Boolean;
var
  context: TRttiContext;
  rttiType: TRttiType;
  valueField: TRttiField;
begin
  Result := IsNullableType(typeInfo);
  if Result then
  begin
    rttiType := context.GetType(typeInfo);
    valueField := rttiType.GetField('fValue');
    Result := Assigned(valueField);
    if Result then
      underlyingTypeInfo := valueField.FieldType.Handle
    else
      underlyingTypeInfo := nil;
  end;
end;

function TryGetUnderlyingValue(const Value: TValue;
  out underlyingValue: TValue): Boolean;
var
  typeInfo: PTypeInfo;
  context: TRttiContext;
  rttiType: TRttiType;
  hasValueField: TRttiField;
  instance: Pointer;
  valueField: TRttiField;
begin
  typeInfo := value.typeInfo;
  Result := True; // IsNullableType(typeInfo);
  if Result then
  begin
    rttiType := context.GetType(typeInfo);
    hasValueField := rttiType.GetField('fHasValue');
    if Assigned(hasValueField) then
    begin
      instance := value.GetReferenceToRawData;
      Result := hasValueField.GetValue(instance).AsString <> '';
      if Result then
      begin
        valueField := rttiType.GetField('fValue');
        Result := Assigned(valueField);
        if Result then
          underlyingValue := valueField.GetValue(instance);
      end;
    end;
  end;
end;

function TrySetUnderlyingValue(const Value: TValue;
  const underlyingValue: TValue): Boolean;
var
  typeInfo: PTypeInfo;
  context: TRttiContext;
  rttiType: TRttiType;
  hasValueField: TRttiField;
  instance: Pointer;
  valueField: TRttiField;
begin
  typeInfo := value.typeInfo;
  Result := True;//IsNullableType(typeInfo);
  if Result then
  begin
    rttiType := context.GetType(typeInfo);
    valueField := rttiType.GetField('fValue');
    if Assigned(valueField) then
    begin
      hasValueField := rttiType.GetField('fHasValue');
      if Assigned(hasValueField) then
      begin
        instance := value.GetReferenceToRawData;
        valueField.SetValue(instance, underlyingValue);
        if underlyingValue.IsEmpty then
          hasValueField.SetValue(instance, '')
        else
          hasValueField.SetValue(instance, '@');
      end;
    end;
  end;
end;


{$REGION 'Nullable<T>'}

constructor Nullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := CHasValueFlag;
end;

constructor Nullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    fValue := v.AsType<T>;
    fHasValue := CHasValueFlag;
  end
  else
  begin
    Clear;
  end;
end;

procedure Nullable<T>.Clear;
begin
  fHasValue := '';
  fValue := Default(T);
end;

class function Nullable<T>.VarIsNullOrEmpty(const value: Variant): Boolean;
begin
  Result := VarIsNull(value) or VarIsEmpty(value);
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := Length(fHasValue) > 0;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
  begin
    raise Exception.Create(SNullableTypeHasNoValue);
  end;
  Result := fValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := Value
  else
    Result := Default(T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := Value
  else
    Result := defaultValue;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if HasValue and other.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(Value, other.Value)
  else
    Result := HasValue = other.HasValue;
end;

class operator Nullable<T>.Implicit(const value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(value);
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): Variant;
var
  v: TValue;
begin
  if value.HasValue then
  begin
    v := TValue.From<T>(value.Value);
    Result := v.AsVariant;
  end
  else
  begin
    Result := Null;
  end;
end;

class operator Nullable<T>.Implicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result := Nullable<T>.Create(v.AsType<T>);
  end
  else
  begin
    Result.Clear;
  end;
end;

class operator Nullable<T>.Implicit(value: Pointer): Nullable<T>;
begin
  if value = nil then
  begin
    Result.Clear;
  end
  else
  begin
    raise Exception.Create(SCannotAssignPointerToNullable);
  end;
end;

class operator Nullable<T>.Explicit(const value: Nullable<T>): T;
begin
  Result := value.Value;
end;

class operator Nullable<T>.Equal(const a, b: Nullable<T>): Boolean;
begin
  Result := a.Equals(b);
end;

class operator Nullable<T>.NotEqual(const a, b: Nullable<T>): Boolean;
begin
  Result := not a.Equals(b);
end;

{$ENDREGION}


end.
