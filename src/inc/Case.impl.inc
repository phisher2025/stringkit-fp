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
