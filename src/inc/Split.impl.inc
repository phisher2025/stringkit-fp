function TStringHelperEx.JoinWith(const Strings: TMatchStrings): string;
begin
  Result := TStringKit.Join(Strings, Self);
end;

function TStringHelperEx.Split(const Delimiter: string; MaxSplit: Integer; RemoveEmptyEntries: Boolean): TMatchStrings;
begin
  Result := TStringKit.Split(Self, Delimiter, MaxSplit, RemoveEmptyEntries);
end;
