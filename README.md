# Novel Parser developed by Elm

## BNF

- <novel> = <factor> | <factor><novel>
- <factor> = <text> | <line> | <at><eol> | <eol>
- <text> = <string><eol> | <string><at><eol> | <string><at><text>
- <line> = <at><string><space><text>
- <at> = "@" | "＠"
- <space> = "　" | " "