function TStringHelperEx.Truncate(MaxLength: Integer; const Ellipsis: string): string;
begin
  Result := TStringKit.Truncate(Self, MaxLength, Ellipsis);
end;

function TStringHelperEx.FormatFileSize: string;
begin
  Result := TStringKit.FormatFileSize(StrToInt64Def(Self, 0));
end;

function TStringHelperEx.FormatNumber(const ThousandSeparator: Char): string;
begin
  Result := TStringKit.FormatNumber(StrToInt64Def(Self, 0), ThousandSeparator);
end;

function TStringHelperEx.FormatFloat(Decimals: Integer; DecimalSeparator, ThousandSeparator: Char): string;
begin
  Result := TStringKit.FormatFloat(StrToFloatDef(Self, 0, DefaultFormatSettings), Decimals, DecimalSeparator, ThousandSeparator);
end;
