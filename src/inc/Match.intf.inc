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
