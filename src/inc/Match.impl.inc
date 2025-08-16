function TStringHelperEx.ExtractMatches(const Pattern: string): TMatchesResults;
begin
  Result := TStringKit.ExtractMatches(Self, Pattern);
end;

function TStringHelperEx.ExtractAllMatches(const Pattern: string): TMatchStrings;
begin
  Result := TStringKit.ExtractAllMatches(Self, Pattern);
end;

function TStringHelperEx.MatchesPattern(const Pattern: string): Boolean;
begin
  Result := TStringKit.MatchesPattern(Self, Pattern);
end;

function TStringHelperEx.ReplaceRegEx(const Pattern, Replacement: string): string;
begin
  Result := TStringKit.ReplaceRegEx(Self, Pattern, Replacement);
end;

function TStringHelperEx.ReplaceText(const OldText, NewText: string): string;
begin
  Result := TStringKit.ReplaceText(Self, OldText, NewText);
end;

function TStringHelperEx.GetWords: TMatchStrings;
begin
  Result := TStringKit.GetWords(Self);
end;

function TStringHelperEx.CountSubString(const SubStr: string): Integer;
begin
  Result := TStringKit.CountSubString(Self, SubStr);
end;

function TStringHelperEx.Contains(const SubStr: string): Boolean;
begin
  Result := TStringKit.Contains(Self, SubStr);
end;

function TStringHelperEx.StartsWith(const Prefix: string): Boolean;
begin
  Result := TStringKit.StartsWith(Self, Prefix);
end;

function TStringHelperEx.EndsWith(const Suffix: string): Boolean;
begin
  Result := TStringKit.EndsWith(Self, Suffix);
end;
