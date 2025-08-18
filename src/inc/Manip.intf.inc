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
function IsEmpty: Boolean; inline;
function GetLength: Integer; inline;
function SubString(StartPos, ALength: Integer): string; inline;
function LeftStr(ALength: Integer): string; inline;
function RightStr(ALength: Integer): string; inline;
