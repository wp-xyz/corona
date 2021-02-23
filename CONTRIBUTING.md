# How to contribute

I'm very happy that anyone wants to contribute!

## Indentation

Use two `<space>`.

No `<tab>` please.

## Line length

Lines should not be longer than about 80 characters. 

Longer lines should be wrapped and indented by two `<space>` relative to the starting line. Line breaks after the operator, please.

Example
```pascal
var
  Name: String;
...
  Name := 'Lorem ipsum dolor sit amet, consetetur sadipscing elitr, ' +
    'sed diam nonumy eirmod tempor invidunt ut labore et dolore magna ' +
    ' aliquyam erat, sed diam voluptua. At vero eos et accusam et justo ' +
    'duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata ' +
    'sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit amet, ' +
    'consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut ' +
    'labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et ' +
    'accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren, ' +
    'no sea takimata sanctus est Lorem ipsum dolor sit amet.';
```

## Empty lines

Empty lines may be used within the code to group line logically.

No empty lines after header of a function/procedure and after declarations.


## Declarations

No `<space>` before, one `<space>` after colon.
  
Example
```pascal
var
  Name: String;
```

## Assignment

One `<space>` before `:=` and one after.

Example
```pascal
var
  Name: String;
begin
  Name := 'My Name';
end.
```

## Constant assignment

One `<space>` before `=` and one after.

Example
```pascal
const
  cName = 'My Name';
```

## Operators, Parentheses

One `<space>` before and one after the operator. Spaces may be dropped to enhance readability.
No space after opening and before closing parenthesis.

Example
```pascal
var
  x: Integer;
  
  x := 1 + (2 - x * 2) * 5;
  // or:
  x := 1 + (2 - x*2) * 5;
```

## Comparison

One `<space>` before `=` and one after.

Example
```pascal
var
  index: Integer;
begin
  if index = 1 then
  begin
    // Do something
  end;
end.
```

## `begin..end` blocks

All `if`, `for`, etc... will have a `begin..end` block unless the code block is a short one-liner.

Example
```pascal
begin
  if FValue = aValue1 then
  begin
    a := 1;
    b := 2;
  end;
  
  if FValue = aValue2 then exit;
```
  
## Uses clause format

- Indent first unit by two `<space>`
- Several units can be in the same line, separated by comma and `<space>`
- Group by units by packages, project units at the end.

Example
```pascal
uses
  SysUtils, StrUtils, Classes, Controls, Forms,   // System units
  TAGraph, TASeries,                              // TAChart units
  cGlobal, cMain;                                 // Project units  
```

## Classes interface declaration

Always include `TObject` in the `class()`.

Always include `private`, `protected`, `public` and `published` even if there is nothing to declare.

Try not to repeat already used sections unless a group having a certain purpose must be held together for clarity.

Property's `read` and `write` in the same line unless line gets longer than 80 chars - add a line break here before `read` and indent by two `<space>`.

Example
```pascal
type
  TSomeClass = class(TObject)
  private
    FShortProp: String;
    FLongPropery: TLongPropertyString;
  protected
  public
    property ShortProp: String read FShortProp write FShortProp;
    property LongProperty: TLongPropertyString 
      read FLongProperty write FLongProperty;
  published
  end;
```

## Commits

Short description of what is changed and why.

Keep commits as "atomic" as possible.
