function TStringHelperEx.HexEncode: string;
begin
  Result := TStringKit.HexEncode(Self);
end;

function TStringHelperEx.HexDecode: string;
begin
  Result := TStringKit.HexDecode(Self);
end;

function TStringHelperEx.Encode64: string;
begin
  Result := TStringKit.Encode64(Self);
end;

function TStringHelperEx.Decode64: string;
begin
  Result := TStringKit.Decode64(Self);
end;

function TStringHelperEx.HTMLEncode: string;
begin
  Result := TStringKit.HTMLEncode(Self);
end;

function TStringHelperEx.HTMLDecode: string;
begin
  Result := TStringKit.HTMLDecode(Self);
end;

function TStringHelperEx.URLEncode: string;
begin
  Result := TStringKit.URLEncode(Self);
end;

function TStringHelperEx.URLDecode: string;
begin
  Result := TStringKit.URLDecode(Self);
end;
