function TStringHelperEx.IsValidEmail: Boolean;
begin
  Result := TStringKit.IsValidEmail(Self);
end;

function TStringHelperEx.IsValidURL: Boolean;
begin
  Result := TStringKit.IsValidURL(Self);
end;

function TStringHelperEx.IsValidIP: Boolean;
begin
  Result := TStringKit.IsValidIP(Self);
end;

function TStringHelperEx.IsValidIPv4: Boolean;
begin
  Result := TStringKit.IsValidIPv4(Self);
end;

function TStringHelperEx.IsValidIPv6: Boolean;
begin
  Result := TStringKit.IsValidIPv6(Self);
end;

function TStringHelperEx.IsValidDate(const AFormat: string): Boolean;
begin
  Result := TStringKit.IsValidDate(Self, AFormat);
end;
