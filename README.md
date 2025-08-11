# 🔧 StringKit-FP: A cool way to work with strings

[![FPC](https://img.shields.io/badge/Free%20Pascal-3.2.2-blue.svg)](https://www.freepascal.org/)
[![Lazarus](https://img.shields.io/badge/Lazarus-3.6+-blue.svg)](https://www.lazarus-ide.org/)
[![License](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE.md)
[![Documentation](https://img.shields.io/badge/Docs-Available-brightgreen.svg)](docs/)
[![Tests](https://img.shields.io/badge/Tests-Passing-brightgreen.svg)](tests/)
[![Status](https://img.shields.io/badge/Status-Development-yellow.svg)]()
[![Version](https://img.shields.io/badge/Version-1.0.0-blueviolet.svg)]()


## 🌟 Why StringKit?

StringKit-FP is a comprehensive string manipulation toolkit for Free Pascal that goes far beyond basic operations. Whether you're building web applications, data processing tools, or text analysis systems, StringKit provides the utilities you need.

**Key Strengths:**

- 🎯 **Comprehensive**: 90+ string operations covering validation, transformation, analysis, and encoding
- 🚀 **Zero Dependencies**: Uses only standard Free Pascal RTL units
- 📊 **Text Analysis**: Readability scoring, n-gram generation, and word counting
- 🔍 **Fuzzy Matching**: Multiple similarity algorithms (Levenshtein, Jaro-Winkler, LCS)
- 🎵 **Phonetic Matching**: Soundex and Metaphone for name similarity
- 🌐 **Web-Ready**: HTML/URL encoding, email/IP validation
- 🔢 **Format Conversion**: Roman numerals, file sizes, number formatting
- ⚡ **Static Methods**: No object creation needed - just call and go!

## ✨ Features at a Glance

### 🔤 **Case Conversion & Formatting**
- `ToUpper()`, `ToLower()`, `ToTitleCase()`
- `ToCamelCase()`, `ToPascalCase()`, `ToSnakeCase()`, `ToKebabCase()`
- `PadLeft()`, `PadRight()`, `PadCenter()` with custom characters
- `Truncate()` with ellipsis support
- `CapitalizeText()` for title-style capitalization

### ✅ **Validation & Pattern Matching**
- `IsValidEmail()`, `IsValidURL()`, `IsValidIP()`, `IsValidIPv4()`, `IsValidIPv6()`
- `IsValidDate()` with custom format strings
- `MatchesPattern()` for regex validation
- `ExtractMatches()` and `ExtractAllMatches()` for pattern extraction

### 🔍 **Fuzzy Matching & Similarity**
- `LevenshteinDistance()` and `LevenshteinSimilarity()`
- `HammingDistance()` for equal-length strings
- `JaroSimilarity()` and `JaroWinklerSimilarity()`
- `LongestCommonSubsequence()` and `LCSSimilarity()`
- `IsFuzzyMatch()` with multiple algorithms

### 🎵 **Phonetic Algorithms**
- `Soundex()` for name similarity (Russell-Odell algorithm)
- `Metaphone()` for English pronunciation matching

### 🔢 **Number & Format Conversion**
- `ToRoman()` and `FromRoman()` for Roman numerals (1-3999)
- `FormatFileSize()` with binary prefixes (B, KB, MB, GB, TB)
- `FormatNumber()` and `FormatFloat()` with thousand separators
- `ToOrdinal()` for ordinal numbers (1st, 2nd, 3rd, etc.)
- `NumberToWords()` for converting numbers to English words

### 🌐 **Encoding & Web Utilities**
- `HTMLEncode()` and `HTMLDecode()` for safe HTML content
- `URLEncode()` and `URLDecode()` for web parameters
- `HexEncode()` and `HexDecode()` for hexadecimal conversion

### 📊 **Text Analysis**
- `CountWords()` and `GetWords()` for text parsing
- `FleschKincaidReadability()` for readability scoring
- `GenerateNGrams()` for NLP applications

### 🛠️ **String Utilities**
- `Split()` and `Join()` with advanced options
- `ReplaceText()` and `ReplaceRegEx()` for substitutions
- `Contains()`, `StartsWith()`, `EndsWith()` for testing
- `CollapseWhitespace()`, `RemoveWhitespace()` for cleaning
- `DuplicateText()`, `ReverseText()` for manipulation
- `GetLength()`, `SubString()`, `LeftStr()`, `RightStr()` for extraction
- `CountSubString()` for counting occurrences

## 💻 Installation (Lazarus IDE)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/stringkit-fp
```

2. Open / start a new project in Lazarus IDE

3. Go to `Package` → `Open Package File (.lpk)...`

4. Navigate to the StringKit packages in the `packages/lazarus/` folder and select `stringkit_fp.lpk`

5. In the package window that opens, click `Compile`

6. Click `Use → Add to Project` to install the package

The StringKit package is now ready to use in your Lazarus project.

## 💻 Installation (General)

1. Clone the repository:

```bash
git clone https://github.com/ikelaiah/stringkit-fp
```

2. Add the source directory to your project's search path.


## 📝 Library Usage

```pascal
uses
  // String manipulation unit
  StringKit;           // String operations
```

## 🚀 Quick Start

### 🔤 Case Conversion & Formatting

```pascal
var
  Text: string;
begin
  // Case conversions
  Text := TStringKit.ToCamelCase('hello world');     // Returns: 'helloWorld'
  Text := TStringKit.ToPascalCase('hello world');    // Returns: 'HelloWorld'
  Text := TStringKit.ToSnakeCase('HelloWorld');      // Returns: 'hello_world'
  Text := TStringKit.ToKebabCase('HelloWorld');      // Returns: 'hello-world'
  Text := TStringKit.ToTitleCase('hello world');     // Returns: 'Hello World'
  
  // Padding and formatting
  Text := TStringKit.PadLeft('123', 8, '0');         // Returns: '00000123'
  Text := TStringKit.PadRight('Name', 10, '.');      // Returns: 'Name......'
  Text := TStringKit.PadCenter('Hi', 10, '-');       // Returns: '----Hi----'
  Text := TStringKit.Truncate('Very long text', 10); // Returns: 'Very lo...'
  Text := TStringKit.CapitalizeText('hello world');  // Returns: 'Hello World'
end;
```

### ✅ Validation & Pattern Matching

```pascal
var
  Matches: TMatchesResults;
  AllMatches: TMatchStrings;
  i: Integer;
begin
  // Built-in validators
  if TStringKit.IsValidEmail('user@example.com') then
    WriteLn('Valid email');
  if TStringKit.IsValidURL('https://example.com') then
    WriteLn('Valid URL');
  if TStringKit.IsValidIPv4('192.168.1.1') then
    WriteLn('Valid IPv4');
  if TStringKit.IsValidDate('2023-12-25', 'yyyy-mm-dd') then
    WriteLn('Valid date');
    
  // Pattern matching and extraction
  if TStringKit.MatchesPattern('ABC123', '^[A-Z]{3}\d{3}$') then
    WriteLn('Matches pattern');
    
  // Extract all matches with position info
  Matches := TStringKit.ExtractMatches('Call 555-1234 or 555-5678', '\d{3}-\d{4}');
  for i := 0 to High(Matches) do
    WriteLn(Format('Found: %s at position %d', [Matches[i].Text, Matches[i].Position]));
    
  // Extract just the matched text
  AllMatches := TStringKit.ExtractAllMatches('Emails: a@b.com, c@d.net', '\w+@\w+\.\w+');
  for i := 0 to High(AllMatches) do
    WriteLn('Email: ' + AllMatches[i]);
end;
```

### 🔍 Fuzzy Matching & Similarity

```pascal
var
  Distance: Integer;
  Similarity: Double;
begin
  // String distance algorithms
  Distance := TStringKit.LevenshteinDistance('kitten', 'sitting'); // Returns: 3
  Distance := TStringKit.HammingDistance('karolin', 'kathrin');     // Returns: 3
  
  // Similarity ratios (0.0 to 1.0)
  Similarity := TStringKit.LevenshteinSimilarity('test', 'best');   // Returns: ~0.75
  Similarity := TStringKit.JaroSimilarity('MARTHA', 'MARHTA');      // Returns: ~0.94
  Similarity := TStringKit.JaroWinklerSimilarity('MARTHA', 'MARHTA'); // Higher than Jaro
  
  // Fuzzy matching with threshold
  if TStringKit.IsFuzzyMatch('apple', 'appel', 0.8) then
    WriteLn('Close match found');
    
  // Longest common subsequence
  WriteLn(TStringKit.LongestCommonSubsequence('ABCDEFG', 'ABDZEFXG')); // Returns: 'ABDEG'
end;
```

### 🎵 Phonetic Matching

```pascal
var
  Code1, Code2: string;
begin
  // Soundex for name matching
  Code1 := TStringKit.Soundex('Robert');  // Returns: 'R163'
  Code2 := TStringKit.Soundex('Rupert');  // Returns: 'R163'
  if Code1 = Code2 then
    WriteLn('Names sound similar');
    
  // Metaphone for pronunciation
  Code1 := TStringKit.Metaphone('knight');   // Returns: 'NT'
  Code2 := TStringKit.Metaphone('night');    // Returns: 'NT'
  if Code1 = Code2 then
    WriteLn('Words sound the same');
end;
```

### 🔢 Number & Format Conversion

```pascal
var
  Roman: string;
  Number: Integer;
  Formatted: string;
begin
  // Roman numerals
  Roman := TStringKit.ToRoman(1994);        // Returns: 'MCMXCIV'
  Number := TStringKit.FromRoman('MCMXCIV'); // Returns: 1994
  
  // File size formatting
  Formatted := TStringKit.FormatFileSize(1048576);    // Returns: '1.00 MB'
  Formatted := TStringKit.FormatFileSize(1500000000); // Returns: '1.40 GB'
  
  // Number formatting
  Formatted := TStringKit.FormatNumber(1234567);           // Returns: '1,234,567'
  Formatted := TStringKit.FormatFloat(12345.67, 2);       // Returns: '12,345.67'
  Formatted := TStringKit.FormatFloat(1234.5, 3, ',', '.'); // Returns: '1.234,500'
  
  // Ordinal and word conversion
  Formatted := TStringKit.ToOrdinal(21);              // Returns: '21st'
  Formatted := TStringKit.NumberToWords(123);         // Returns: 'one hundred and twenty-three'
end;
```

### 🌐 Encoding & Web Utilities

```pascal
var
  Encoded, Decoded: string;
begin
  // HTML encoding for safe web content
  Encoded := TStringKit.HTMLEncode('<p class="bold">Text</p>');
  // Returns: '&lt;p class=&quot;bold&quot;&gt;Text&lt;/p&gt;'
  
  Decoded := TStringKit.HTMLDecode('&lt;p&gt;Hello &amp; World&lt;/p&gt;');
  // Returns: '<p>Hello & World</p>'
  
  // URL encoding for web parameters
  Encoded := TStringKit.URLEncode('Hello World!');     // Returns: 'Hello+World%21'
  Decoded := TStringKit.URLDecode('Hello+World%21');   // Returns: 'Hello World!'
  
  // Hexadecimal encoding
  Encoded := TStringKit.HexEncode('Hello');            // Returns: '48656C6C6F'
  Decoded := TStringKit.HexDecode('48656C6C6F');       // Returns: 'Hello'
end;
```

### 📊 Text Analysis

```pascal
var
  WordCount: Integer;
  Readability: Double;
  NGrams: TMatchStrings;
  i: Integer;
begin
  // Basic text statistics
  WordCount := TStringKit.CountWords('Hello, world! How are you?'); // Returns: 5
  
  // Readability scoring (0-100, higher = easier)
  Readability := TStringKit.FleschKincaidReadability('The quick brown fox jumps.');
  WriteLn(Format('Readability score: %.1f', [Readability]));
  
  // N-gram generation for NLP
  NGrams := TStringKit.GenerateNGrams('the quick brown fox', 2); // Bigrams
  for i := 0 to High(NGrams) do
    WriteLn('Bigram: ' + NGrams[i]);
  // Output: 'the quick', 'quick brown', 'brown fox'
end;
```

### 🛠️ String Utilities

```pascal
var
  Parts: TMatchStrings;
  Joined: string;
  i: Integer;
begin
  // Splitting and joining
  Parts := TStringKit.Split('apple,banana,cherry', ',');
  for i := 0 to High(Parts) do
    WriteLn('Part: ' + Parts[i]);
    
  Joined := TStringKit.Join(Parts, ' | '); // Returns: 'apple | banana | cherry'
  
  // Text replacement
  Joined := TStringKit.ReplaceText('Hello World', 'World', 'Pascal');
  // Returns: 'Hello Pascal'
  
  Joined := TStringKit.ReplaceRegEx('Phone: 123-456-7890', '(\d{3})-(\d{3})-(\d{4})', '($1) $2-$3');
  // Returns: 'Phone: (123) 456-7890'
  
  // String testing
  if TStringKit.StartsWith('Hello World', 'Hello') then
    WriteLn('Starts with Hello');
  if TStringKit.EndsWith('test.txt', '.txt') then
    WriteLn('Is a text file');
  if TStringKit.Contains('Hello World', 'World') then
    WriteLn('Contains World');
    
  // Text cleaning
  Joined := TStringKit.CollapseWhitespace('  Multiple   spaces  '); // Returns: ' Multiple spaces '
  Joined := TStringKit.RemoveWhitespace('  No spaces  ');          // Returns: 'Nospaces'
  
  // String extraction and manipulation
  Joined := TStringKit.LeftStr('Hello World', 5);     // Returns: 'Hello'
  Joined := TStringKit.RightStr('Hello World', 5);    // Returns: 'World'
  Joined := TStringKit.SubString('Hello World', 7, 5); // Returns: 'World'
  Joined := TStringKit.DuplicateText('Hi! ', 3);      // Returns: 'Hi! Hi! Hi! '
  
  // String analysis
  WriteLn(TStringKit.GetLength('Hello'));             // Returns: 5
  WriteLn(TStringKit.CountSubString('ababab', 'ab')); // Returns: 3
end;
```


## 📖 System Requirements

### Tested Environments

| Module                          | Windows 11 | Ubuntu 24.04.2 |
|---------------------------------|------------|----------------|
| StringKit                 | ✅         | ✅             |

### Dependencies

- Windows
  - No external dependencies required
- Linux
  - No external dependencies required
- Uses only standard Free Pascal RTL units

### Build Requirements

- Free Pascal Compiler (FPC) 3.2.2+
- Lazarus 4.0+
- Basic development tools (git, terminal, etc)

## 📚 Documentation

For detailed documentation, see:

- 📋 [Cheat Sheet](docs/cheat-sheet.md)
- 📝 [Strings](docs/StringKit.Strings.md)
 

## 💬 Community & Support

- **Questions?** [Open a discussion](https://github.com/ikelaiah/stringkit-fp/discussions)
- **Found a bug?** [Report an issue](https://github.com/ikelaiah/stringkit-fp/issues)


## ✅ Testing

1. Open the `TestRunner.lpi` using Lazarus IDE
2. Compile the project
3. Run the Test Runner:

```bash
$ cd tests
$ ./TestRunner.exe -a --format=plain
```

## 🗺️ Future Goals

- **Unicode Support Enhancement**: Improve multi-byte character handling for international text processing
- **Performance Optimization**: Add optimized versions of core algorithms for large-scale text processing
- **Package Manager Integration**: Support for Free Pascal or Lazarus package managers


## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

1. Fork the Project
2. Create your Feature Branch (git checkout -b feature/AmazingFeature)
3. Commit your Changes (git commit -m 'Add some AmazingFeature')
4. Push to the Branch (git push origin feature/AmazingFeature)
5. Open a Pull Request

## ⚖️ License

This project is licensed under the MIT License - see the [LICENSE](LICENSE.md) file for details.

## 🙏 Acknowledgments

- FPC Team for Free Pascal
- Contributors and maintainers



---

*Feedback and suggestions are welcome! See the [issues](https://github.com/ikelaiah/stringkit-fp/issues) page to contribute ideas or track progress.*