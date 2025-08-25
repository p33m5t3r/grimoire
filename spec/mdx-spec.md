# Grimoire MDX Specification

## Core Principles
1. **Blocks are separated by double newlines** (`\n\n`)
2. **Special blocks have explicit terminators** to allow internal newlines
3. **Inline formatting cannot overlap** (start what you finish first)
4. **Explicit syntax for display vs inline** elements

## Block Types

### Text Block (Paragraph)
- Any text not matching other block patterns
- Can span multiple single-newline-separated lines
- Contains inline elements (parsed in phase 2)
- Terminated by `\n\n`

```
This is a paragraph that can
span multiple lines.

This is a new paragraph.
```

### Header Block
- `# Header 1` or `## Header 2` 
- Must be on its own line
- Terminated by `\n\n`

```
# Section Title

## Subsection Title
```

### Code Block
- Starts with ` ``` ` optionally followed by language
- Contains raw text (no markdown parsing)
- Can contain empty lines
- Terminated by ` ``` ` on its own line

```
```haskell
main :: IO ()
main = putStrLn "preserves *asterisks* literally"

-- can have empty lines
```
```

### Display Image Block  
- `!!![alt text](url)` or `!!![alt text](url){width}`
- Must be on its own line
- Terminated by `\n\n`

```
!!![A large diagram](diagram.png){80}
```

### Display Math Block
- `$$` on its own line starts
- `$$` on its own line ends
- LaTeX math between (no markdown parsing)

```
$$
\int_0^\infty e^{-x^2} dx = \frac{\sqrt{\pi}}{2}
$$
```

### Quote Block
- Lines starting with `> `
- Consecutive quoted lines form one block
- Can contain inline markdown
- Terminated by `\n\n`

```
> This is a quote that can
> span multiple lines.
```

### List Block
- Lines starting with `- ` (unordered) or `1. ` (ordered)  
- Indentation via spaces (2 spaces per level) for nesting
- Consecutive list items form one block
- Terminated by `\n\n`

```
- Item 1
- Item 2
  - Nested item
  - Another nested
- Item 3

1. First
2. Second
```

### HTML Block
- Starts with `<html>`
- Contains raw HTML (no markdown parsing)
- Can contain empty lines
- Terminated by `</html>`

```
<html>
<table>
  <tr><td>preserves *asterisks*</td></tr>
</table>
</html>
```

### Footnote Definition Block
- `~~~n` starts (where n is a number)
- Contains full MDX content (including code blocks, math, etc.)
- Terminated by `~~~`

```
~~~1
This is a footnote that can have multiple paragraphs.

It can even have code blocks:
```python
print("hello from footnote")
```

And display math:
$$
\sum_{i=1}^n i = \frac{n(n+1)}{2}
$$
~~~
```

### Dropdown Block
- `::: teaser text` starts the block
- Content on following lines
- Contains full MDX content (including code blocks, math, etc.)
- Terminated by `:::`

```
::: Click to expand
This is hidden content.

It can have multiple paragraphs, code blocks:
```haskell
factorial n = product [1..n]
```

And even math:
$$E = mc^2$$
:::
```

## Inline Elements (within text blocks only)

### Text Formatting
- `*bold*` 
- `_italic_`
- `{color|colored text}` (e.g., `{pink|text}`, `{red|text}`)
- Formats cannot overlap: `*bold _and italic_*` is valid, `*bold _mixed* italic_` is NOT

### Code
- `` `inline code` ``
- No nested markdown parsing inside

### Math  
- `$inline math$`
- LaTeX syntax, no nested markdown

### Links
- External: `[text](https://example.com)`
- Internal: `[text](@internal-id)`

### Images (inline)
- `![alt](url)` within a paragraph

### Footnote References
- `[^n]` where n matches a footnote definition

## Escaping
- Backslash escapes special characters: `\*`, `\_`, `\$`, `` \` ``, `\[`, etc.
- In code blocks and math: no escaping needed
- In HTML blocks: no escaping needed

## Examples of Valid/Invalid

### Valid
```
This is *bold* and this is _italic_ and this is *_bold italic_*.

Here's a link to [my post](@post-1) and an external [site](https://x.com).

A paragraph with inline ![icon](icon.png) image.

!!![Display image on its own](banner.jpg)
```

### Invalid  
```
This is *bold _but this* is broken_.  // Overlapping formats

![centered](img.png)  // Must use !!! for display
```

## Fence Types Summary
- ` ``` ` = Code blocks (preserve literally, no MDX parsing)
- ` ::: ` = Dropdowns (parse MDX inside, UI collapsible)
- ` ~~~ ` = Footnotes (parse MDX inside, reference via `[^n]`)

## Parsing Strategy
1. **Phase 1**: Split on blocks, identify block types, preserve raw content
2. **Phase 2**: Parse inline elements in text blocks, dropdowns, footnotes
3. **Never parse markdown inside**: code blocks, HTML blocks, math blocks