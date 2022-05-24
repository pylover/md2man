
## Description

Input markdown must have one single number sign `#` as the title and every 
double number sign `##` will be rendered as a section.

Anything decorated with backticks `\`` and or double star `\*\*` signs is bold 
text.

Decorate with single `*` to get italic text.


## Markdown Examples

```
# Foo

`Foo` is an awesome package.

## Synopsis

foo [-V/--version] [-v/--verbose] FILENAME

## Bar

Bar is awesome too.

```

## CLI Examples

```
md2man --author Alice --email alice@example.com --section 2 --revision 3.2 -o foo.1
```

```
md2man --section 3 --revision "GNU" --book "Linux Programmer's Manual"
```


## Contributing

https://github.com/pylover/md2man
