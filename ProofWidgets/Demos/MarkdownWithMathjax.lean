import ProofWidgets.Component.MarkdownWithMathjax
import ProofWidgets.Component.HtmlDisplay

open ProofWidgets ProofWidgets.Jsx

def markdown : String := "
# Heading

Lorem ipsum *dolor* **sit** `amet`, consectetur adipiscing elit,
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

## Subheading 1

Let's try to write some math.
Suppose we have some $a,b \\in \\mathbb{Z}$, and some function $f : \\mathbb{R} \\to \\mathbb{R}$.
We can write some displayed math as follows:
```math
\\int_0^\\infty f(x)^a = g(b).
```

## Subheading 2
Now let's try some more markdown.
For example, we can
[link to the leanprover community webpage](https://leanprover-community.github.io).

## Subheading 3

Now for some lists:

- First item
- Second item

1. First item
2. Second item
"

#html <MarkdownWithMathjax markdown={markdown} />
