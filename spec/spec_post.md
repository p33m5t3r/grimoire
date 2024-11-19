title:  this is a title
teaser: this is a teaser
date:   11/02/24
tags:   tag1, tag2, tag3
series: some-post-sequence
part:   1
thumb:  image.jpg
hide:   false


paragraph1 sentence 1.
paragraph1 sentence 2.

*paragraph2 sentence 1 (bold).*
_paragraph2 sentence 2 (italic)._
paragraph2 sentence 3 (plaintext).

# this is a section header

## this is a subsection header

this is inline math: $a^2 + b^2 = c^2$

and this is display math: $$ \R \cong \frac{C}{(x^2 + 1)} $$

I can put links to defns, like the one for [abelian groups](@abelian_group)

which should have an entry in some similar file of the form
```
DEFN abelian_group https://link-text-here
<insert custom markdown>
```


```python
print("this is what a python code snippet looks like")
```

`this is normal inline code`

`fmt:hs this is hs formatted inline code`

this is a link [link-text](https://ncatlab.org)

this is an image in a paragraph, right aligned ![caption-text](image.jpg)

![this is a standalone, centered image](image.png)

below is a table of images

<table class="img-table">
  <tr>
    <td><img src="img1.jpg"></td>
    <td><img src="img2.jpg"></td>
  </tr>
</table>

below is a normal table
<table>
  <tr>
    <th> col 1 </th>
    <th> col 2 </th>
  </tr>
  <tr>
    <td> a11 </td>
    <td> a12 </td>
  </tr>
  <tr>
    <td> a21 </td>
    <td> a22 </td>
  </tr>
</table>

this text has a footnote [^1]

[^1]: this is the text of the footnote

%% this is a dropdown
%% and this is the stuff inside the dropdown!

this is a new paragraph on my but still inside the dropdown!
%%

this is a paragraph below the dropdown
