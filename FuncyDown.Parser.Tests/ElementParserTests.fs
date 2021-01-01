module ElementParserTests

open System
open Xunit
open FuncyDown
open FuncyDown.Element
open FParsec

let text = "Some text"

let test p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

[<Fact>]
let ``Horizontal parser`` () =
    let actual = test ElementParser.horizontalRule ($"---{Environment.NewLine}")
    Assert.Equal( HorizontalRule, actual)
    
[<Fact>]
let ``Blockquote parser`` () =
    let actual = test ElementParser.blockQuote $">{text}"
    let expected = (BlockQuote text)
    Assert.Equal(expected, actual)
    
[<Fact>]
let ``BlockCode parser basic`` () =
    let code =  "```
let x = 1
```"
    let actual = test ElementParser.blockCode code
    let expected = (BlockCode { Language = None; Code = """let x = 1""" })
    Assert.Equal(expected, actual)
    
[<Fact>]
let ``BlockCode parser with lang`` () =
    let code =  """```fsharp
printfn "Hello world!" 
```
"""
    // TODO: 31/12/2020 dburriss@xebia.com | Should this newline be consumed?
    let actual = test ElementParser.blockCode code
    let expected = (BlockCode { Language = Some "fsharp"; Code = """printfn "Hello world!" """ })
    Assert.Equal(expected, actual)
    
[<Fact>]
let ``InlineCode parser`` () =
    let code =  """`printfn "Hello world!" `"""
    let actual = test ElementParser.inlineCode code
    let expected = (InlineCode { Language = None; Code = """printfn "Hello world!" """ })
    Assert.Equal(expected, actual)

[<Fact>]
let ``Text parser`` () =
    let actual = test ElementParser.text " x"
    let expected = (Element.Text " x")
    Assert.Equal(expected, actual)
    