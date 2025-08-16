function TStringHelperEx.Trim: string;
begin
  Result := TStringKit.Trim(Self);
end;

function TStringHelperEx.TrimLeft: string;
begin
  Result := TStringKit.TrimLeft(Self);
end;

function TStringHelperEx.TrimRight: string;
begin
  Result := TStringKit.TrimRight(Self);
end;

function TStringHelperEx.ToUpper: string;
begin
  Result := TStringKit.ToUpper(Self);
end;

function TStringHelperEx.ToLower: string;
begin
  Result := TStringKit.ToLower(Self);
end;

function TStringHelperEx.PadCenter(Width: Integer; PadChar: Char): string;
begin
  Result := TStringKit.PadCenter(Self, Width, PadChar);
end;

function TStringHelperEx.PadLeft(Width: Integer; PadChar: Char): string;
begin
  Result := TStringKit.PadLeft(Self, Width, PadChar);
end;

function TStringHelperEx.PadRight(Width: Integer; PadChar: Char): string;
begin
  Result := TStringKit.PadRight(Self, Width, PadChar);
end;

function TStringHelperEx.CollapseWhitespace: string;
begin
  Result := TStringKit.CollapseWhitespace(Self);
end;

function TStringHelperEx.RemoveWhitespace: string;
begin
  Result := TStringKit.RemoveWhitespace(Self);
end;

function TStringHelperEx.DuplicateText(Count: Integer): string;
begin
  Result := TStringKit.DuplicateText(Self, Count);
end;

function TStringHelperEx.Reverse: string;
begin
  Result := TStringKit.ReverseText(Self);
end;

function TStringHelperEx.Capitalize: string;
begin
  Result := TStringKit.CapitalizeText(Self);
end;

function TStringHelperEx.IsEmpty: Boolean;
begin
  Result := TStringKit.IsEmpty(Self);
end;

function TStringHelperEx.GetLength: Integer;
begin
  Result := TStringKit.GetLength(Self);
end;

function TStringHelperEx.SubString(StartPos, ALength: Integer): string;
begin
  Result := TStringKit.SubString(Self, StartPos, ALength);
end;

function TStringHelperEx.LeftStr(ALength: Integer): string;
begin
  Result := TStringKit.LeftStr(Self, ALength);
end;

function TStringHelperEx.RightStr(ALength: Integer): string;
begin
  Result := TStringKit.RightStr(Self, ALength);
end;
