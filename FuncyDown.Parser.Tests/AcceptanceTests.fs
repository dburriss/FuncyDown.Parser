module AcceptanceTests

open FuncyDown
//open FuncyDown.Document
open FuncyDown.Element
open Xunit

let mdText = """# 1. Use ARDs

Date: 2020-12-29

## Status

Accepted

## Context

The issue motivating this decision, and any context that influences or constrains the decision.

## Decision

The change that we're proposing or have agreed to implement.

## Consequences

What becomes easier or more difficult to do and any risks introduced by the change that will need to be mitigated.

"""

[<Fact>]
let ``Parse full markdown text returns expected elements`` () =
    let doc = Document.parse mdText
    Assert.Equal(10, doc.Elements.Length)
    
[<Fact>]
let ``Transform md file to cancel status`` () =
    let doc = Document.parse mdText
    let cancel : Transform =
        fun ctx el ->
            match el with
            | Header h when h.Size = H2 && h.Text = "Status" ->
                ctx.Set<bool>(true) ; [el] // must return current element
            | Paragraph text ->
                match (ctx.Get<bool>()) with
                | Some true ->
                    ctx.Set<bool>(false)
                    [Paragraph "Cancelled"]
                | _ -> [el]
            | _ -> [el]
            
            
    let transformations = [cancel]
    let transformed = Transformer.transform transformations doc |> Document.asString
    Assert.Contains("Cancelled", transformed)
    