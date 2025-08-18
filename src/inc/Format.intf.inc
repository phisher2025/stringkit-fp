// Formatting
function Truncate(MaxLength: Integer; const Ellipsis: string = '...'): string; inline;
function FormatFileSize: string; inline;
function FormatNumber(const ThousandSeparator: Char = ','): string; inline;
function FormatFloat(Decimals: Integer = 2; DecimalSeparator: Char = '.'; ThousandSeparator: Char = ','): string; inline;
