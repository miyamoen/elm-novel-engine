# Novel Parser developed by Elm

## BNF

- novel := factor | factor novel
- factor := text | at
- text := string
- at = "@"
