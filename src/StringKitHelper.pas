unit StringKitHelper;

{
  StringKitHelper - A string type helper for TStringKit
  
  This unit provides a string type helper (TStringHelperEx) that wraps the static
  methods of TStringKit, allowing for more natural instance-style method calls
  on string variables and literals.
  
  Example usage:
    var
      S: string;
    begin
      S := '  hello world  ';
      S := S.Trim; // Instance-style call
      
      // Original static call still works:
      S := TStringKit.Trim('  hello  ');
    end;
    
  Generated on: 2025-02-15
  Total TStringKit class functions: 120
  Helper methods generated: 98
  Methods skipped (no string parameter): 22
  
  Skipped methods (no string parameter):
    - IsWhiteSpace, Min2, Min3, Max2, ToRoman, ToOrdinal, NumberToWords, etc.
}

{$mode objfpc}{$H+}{$J-}
{$modeswitch typehelpers}
interface

uses
  Classes, SysUtils, StringKit, RegExpr;

type
  { TStringHelperEx }
  TStringHelperEx = type helper for string
  public
    // String manipulation
    function Trim: string; inline;
    function TrimLeft: string; inline;
    function TrimRight: string; inline;
    function ToUpper: string; inline;
    function ToLower: string; inline;
    function PadCenter(Width: Integer; PadChar: Char = ' '): string; inline;
    function PadLeft(Width: Integer; PadChar: Char = ' '): string; inline;
    function PadRight(Width: Integer; PadChar: Char = ' '): string; inline;
    function CollapseWhitespace: string; inline;
    function RemoveWhitespace: string; inline;
    function DuplicateText(Count: Integer): string; inline;
    function Reverse: string; inline;
    function Capitalize: string; inline;
    
    // String matching and extraction
    function ExtractMatches(const Pattern: string): TMatchesResults; inline;
    function ExtractAllMatches(const Pattern: string): TMatchStrings; inline;
    function MatchesPattern(const Pattern: string): Boolean; inline;
    function ReplaceRegEx(const Pattern, Replacement: string): string; inline;
    function ReplaceText(const OldText, NewText: string): string; inline;
    function GetWords: TMatchStrings; inline;
    function CountSubString(const SubStr: string): Integer; inline;
    function Contains(const SubStr: string): Boolean; inline;
    function StartsWith(const Prefix: string): Boolean; inline;
    function EndsWith(const Suffix: string): Boolean; inline;
    function IsEmpty: Boolean; inline;
    function GetLength: Integer; inline;
    function SubString(StartPos, ALength: Integer): string; inline;
    function LeftStr(ALength: Integer): string; inline;
    function RightStr(ALength: Integer): string; inline;
    
    // String similarity and comparison
    function LevenshteinDistance(const S: string): Integer; inline;
    function LevenshteinSimilarity(const S: string): Double; inline;
    function HammingDistance(const S: string): Integer; inline;
    function JaroSimilarity(const S: string): Double; inline;
    function JaroWinklerSimilarity(const S: string): Double; inline;
    function LongestCommonSubsequence(const S: string): string; inline;
    function LCSSimilarity(const S: string): Double; inline;
    function IsFuzzyMatch(const S: string; Threshold: Double = 0.7; Method: Integer = 0): Boolean; inline;
    
    // Case conversion
    function ToTitleCase: string; inline;
    function ToCamelCase: string; inline;
    function ToPascalCase: string; inline;
    function ToSnakeCase: string; inline;
    function ToKebabCase: string; inline;
    
    // Validation
    function IsValidEmail: Boolean; inline;
    function IsValidURL: Boolean; inline;
    function IsValidIP: Boolean; inline;
    function IsValidIPv4: Boolean; inline;
    function IsValidIPv6: Boolean; inline;
    function IsValidDate(const AFormat: string): Boolean; inline;
    
    // Formatting
    function Truncate(MaxLength: Integer; const Ellipsis: string = '...'): string; inline;
    function FormatFileSize: string; inline;
    function FormatNumber(const ThousandSeparator: Char = ','): string; inline;
    function FormatFloat(Decimals: Integer = 2; DecimalSeparator: Char = '.'; ThousandSeparator: Char = ','): string; inline;
    
    // Number conversions
    function ToRoman: string; inline;
    function ToOrdinal: string; inline;
    function NumberToWords: string; inline;
    function FromRoman: Integer; inline;
    // Phonetic algorithms
    function Soundex: string; inline;
    function Metaphone: string; inline;
    function CountWords: Integer; inline;
    function FleschKincaidReadability: Double; inline;
    function GenerateNGrams(N: Integer): TMatchStrings; inline;
    
    // Encoding/decoding
    function Encode64: string; inline;
    function Decode64: string; inline;
    function HTMLEncode: string; inline;
    function HTMLDecode: string; inline;
    function URLEncode: string; inline;
    function URLDecode: string; inline;
    function HexEncode: string; inline;
    function HexDecode: string; inline;
    
    // String splitting and joining
    function JoinWith(const Strings: TMatchStrings): string; inline;
    function Split(const Delimiter: string; MaxSplit: Integer = 0; RemoveEmptyEntries: Boolean = False): TMatchStrings; inline;
  end;

implementation

{ TStringHelperEx }

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

function TStringHelperEx.LevenshteinDistance(const S: string): Integer;
begin
  Result := TStringKit.LevenshteinDistance(Self, S);
end;

function TStringHelperEx.LevenshteinSimilarity(const S: string): Double;
begin
  Result := TStringKit.LevenshteinSimilarity(Self, S);
end;

function TStringHelperEx.HammingDistance(const S: string): Integer;
begin
  Result := TStringKit.HammingDistance(Self, S);
end;

function TStringHelperEx.JaroSimilarity(const S: string): Double;
begin
  Result := TStringKit.JaroSimilarity(Self, S);
end;

function TStringHelperEx.JaroWinklerSimilarity(const S: string): Double;
begin
  Result := TStringKit.JaroWinklerSimilarity(Self, S);
end;

function TStringHelperEx.LongestCommonSubsequence(const S: string): string;
begin
  Result := TStringKit.LongestCommonSubsequence(Self, S);
end;

function TStringHelperEx.LCSSimilarity(const S: string): Double;
begin
  Result := TStringKit.LCSSimilarity(Self, S);
end;

function TStringHelperEx.IsFuzzyMatch(const S: string; Threshold: Double; Method: Integer): Boolean;
begin
  Result := TStringKit.IsFuzzyMatch(Self, S, Threshold, Method);
end;

function TStringHelperEx.ToTitleCase: string;
begin
  Result := TStringKit.ToTitleCase(Self);
end;

function TStringHelperEx.ToCamelCase: string;
begin
  Result := TStringKit.ToCamelCase(Self);
end;

function TStringHelperEx.ToPascalCase: string;
begin
  Result := TStringKit.ToPascalCase(Self);
end;

function TStringHelperEx.ToSnakeCase: string;
begin
  Result := TStringKit.ToSnakeCase(Self);
end;

function TStringHelperEx.ToKebabCase: string;
begin
  Result := TStringKit.ToKebabCase(Self);
end;

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

function TStringHelperEx.Truncate(MaxLength: Integer; const Ellipsis: string): string;
begin
  Result := TStringKit.Truncate(Self, MaxLength, Ellipsis);
end;

function TStringHelperEx.ToRoman: string;
begin
  Result := TStringKit.ToRoman(StrToIntDef(Self, 0));
end;

function TStringHelperEx.ToOrdinal: string;
begin
  Result := TStringKit.ToOrdinal(StrToIntDef(Self, 0));
end;

function TStringHelperEx.NumberToWords: string;
begin
  Result := TStringKit.NumberToWords(StrToInt64Def(Self, 0));
end;

function TStringHelperEx.FromRoman: Integer;
begin
  Result := TStringKit.FromRoman(Self);
end;

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

function TStringHelperEx.JoinWith(const Strings: TMatchStrings): string;
begin
  Result := TStringKit.Join(Strings, Self);
end;

function TStringHelperEx.Split(const Delimiter: string; MaxSplit: Integer; RemoveEmptyEntries: Boolean): TMatchStrings;
begin
  Result := TStringKit.Split(Self, Delimiter, MaxSplit, RemoveEmptyEntries);
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

function TStringHelperEx.Soundex: string;
begin
  Result := TStringKit.Soundex(Self);
end;

function TStringHelperEx.Metaphone: string;
begin
  Result := TStringKit.Metaphone(Self);
end;

function TStringHelperEx.CountWords: Integer;
begin
  Result := TStringKit.CountWords(Self);
end;

function TStringHelperEx.FleschKincaidReadability: Double;
begin
  Result := TStringKit.FleschKincaidReadability(Self);
end;

function TStringHelperEx.GenerateNGrams(N: Integer): TMatchStrings;
begin
  Result := TStringKit.GenerateNGrams(Self, N);
end;

end.
