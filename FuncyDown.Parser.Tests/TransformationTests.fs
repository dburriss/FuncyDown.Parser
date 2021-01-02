module TransformationTests

open System
open FuncyDown
open FuncyDown.Document
open FuncyDown.Element
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Transforming empty doc results in empty doc`` () =
    let doc = emptyDocument
    let transformations = []
    let transformed = Transformer.transform transformations doc
    Assert.Equal(doc, transformed)
    
[<Fact>]
let ``No transformation results in same doc`` () =
    let doc = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph"
    let transformations = []
    let transformed = Transformer.transform transformations doc
    Assert.Equal(doc, transformed)
      
[<Fact>]
let ``Can add element`` () =
    let doc = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph"
    let expected = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph"
                                                      |> addParagraph "Some text in a paragraph"
    let duplicateParagraph _ el =
        match el with
        | Paragraph _ -> [el; el]
        | _ -> [el]
    let transformations = [duplicateParagraph]
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
        
[<Fact>]
let ``Can remove element`` () =
    let doc = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph"
    let expected = emptyDocument |> addH1 "A heading"
    let duplicateParagraph _ el =
        match el with
        | Paragraph _ -> []
        | _ -> [el]
    let transformations = [duplicateParagraph]
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
         
[<Fact>]
let ``Can change element content`` () =
    let doc = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph"
    let expected = emptyDocument |> addH1 "A heading" |> addParagraph "Some changed text in a paragraph"
    let changeContent _ el =
        match el with
        | Paragraph _ -> [Paragraph "Some changed text in a paragraph"]
        | _ -> [el]
    let transformations = [changeContent]
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
          
[<Fact>]
let ``Can amend element content`` () =
    let doc = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph"
    let expected = emptyDocument |> addH1 "A heading" |> addParagraph "Some text in a paragraph 1"
    let amend _ el =
        match el with
        | Paragraph text -> [Paragraph (text + " 1")]
        | _ -> [el]
    let transformations = [amend]
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
          
[<Fact>]
let ``Use context to specify that previous heading was Status`` () =
    let doc =
              emptyDocument
              |> addH1 "A heading"
              |> addParagraph "Some text in a paragraph"
              |> addH2 "Status"
              |> addParagraph "Accepted"
              |> addH2 "Mental stated"
              |> addParagraph "Accepted"
              
    let expected =
                  emptyDocument
                  |> addH1 "A heading"
                  |> addParagraph "Some text in a paragraph"
                  |> addH2 "Status"
                  |> addParagraph "Cancelled"
                  |> addH2 "Mental stated"
                  |> addParagraph "Accepted"
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
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
    
[<Fact>]
let ``Add a copyright at end of file`` () =
    let doc =
              emptyDocument
              |> addH1 "A heading"
              |> addParagraph "Some text in a paragraph"
              
    let expected =
                  emptyDocument
                  |> addH1 "A heading"
                  |> addParagraph "Some text in a paragraph"
                  |> addParagraph "Copyright 2020 Chimplab"
                  
    let copyright : Transform =
        fun ctx el ->
            if Option.isNone (ctx.NextElement()) then
                [el; Paragraph "Copyright 2020 Chimplab"]
            else [el]

    let transformations = [copyright]
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
       
[<Fact>]
let ``Add a heading`` () =
    let doc =
              emptyDocument
              |> addParagraph "Some text in a paragraph"
              
    let expected =
                  emptyDocument
                  |> addH1 "A heading"
                  |> addParagraph "Some text in a paragraph"
                  
    let copyright : Transform =
        fun ctx el ->
            if Option.isNone (ctx.PreviousElement()) then
                [Header { Size = H1; Text = "A heading" }; el]
            else [el]

    let transformations = [copyright]
    let transformed = Transformer.transform transformations doc
    Assert.Equal(expected, transformed)
    