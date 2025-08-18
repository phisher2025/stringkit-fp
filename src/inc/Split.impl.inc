function TStringHelperEx.JoinWith(const Strings: TStringDynArray): string;
begin
  Result := TStringKit.Join(Strings, Self);
end;

function TStringHelperEx.Split(const Delimiter: string; MaxSplit: Integer; RemoveEmptyEntries: Boolean): TStringDynArray;
begin
  Result := TStringKit.Split(Self, Delimiter, MaxSplit, RemoveEmptyEntries);
end;
