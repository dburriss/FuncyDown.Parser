# FuncyDown Parser

A parser for transforming markdown text into [FuncyDown](https://github.com/GDATASoftwareAG/FuncyDown) `Document` instance. The library also has a very an experimental transform pipeline for manipulating the content of a document in a strongly typed way using the `Element` DUs from the FuncyDown library.

## Usage

### Parsing

```fsharp
open FuncyDown
// assuming `mdText` contains a valid markdown string
let doc = Document.parse mdText
```

### Transforming

> Note: Experimental and so API will likely change.

The following puts a period at the end of every paragraph.

```fsharp
// doc is a `Document` instance
let amend _ el =
        match el with
        | Paragraph text -> [Paragraph (text + ".")]
        | _ -> [el]
let transformations = [amend]
let transformed = Transformer.transform transformations doc
```

Here is an example using the `Context` object to detect being the last element and adding a copyright notice.

```fsharp
// doc is a `Document` instance
let copyright : Transform =
        fun ctx el ->
            if Option.isNone (ctx.NextElement()) then
                [el; Paragraph "Copyright 2021 Devon Burriss"]
            else [el]

    let transformations = [copyright]
    let transformed = Transformer.transform transformations doc
```