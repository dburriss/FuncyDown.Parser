module DocumentTests

open System
open FuncyDown.Document
open Xunit
open Swensen.Unquote

let text = "Some text"

let testRoundTrip doc =
    let expected = doc
    let md = expected |> asString
    let actual = parse md
    Assert.Equal(expected, actual)


[<Fact>]
let ``Empty md is empty document`` () =
    let expected = emptyDocument
    testRoundTrip expected

[<Fact>]
let ``Newline`` () =
    let expected = emptyDocument |> addNewline
    testRoundTrip expected
    
[<Fact>]
let ``Horizontal rule`` () =
    let expected = emptyDocument |> addHorizontalRule
    testRoundTrip expected
    
[<Fact>]
let ``BlockQuote text`` () =
    let expected = emptyDocument |> addBlockQuote text
    testRoundTrip expected
  
[<Fact>]
let ``BlockCode without lang`` () =
    let expected = emptyDocument |> addBlockCode { Language = None; Code = """printfn "Hello world!" """ }
    testRoundTrip expected
    
[<Fact>]
let ``BlockCode with lang`` () =
    let expected = emptyDocument |> addBlockCode { Language = Some "fsharp"; Code = """printfn "Hello world!" """ }
    testRoundTrip expected
       
[<Fact>]
let ``InlineCode`` () =
    let expected = emptyDocument |> addInlineCode { Language = None; Code = """printfn "Hello world!" """ }
    testRoundTrip expected
        
[<Fact>]
let ``Image with no title`` () =
    let expected = emptyDocument |> addImage { AltText = "some alt"; Target = "/some/target"; Title = None }
    testRoundTrip expected
           
[<Fact>]
let ``Image with title`` () =
    let expected = emptyDocument |> addImage { AltText = "some alt"; Target = "/some/target"; Title = Some "A title" }
    testRoundTrip expected
          
[<Fact>]
let ``Link with no title`` () =
    let expected = emptyDocument |> addLink { Text = "some alt"; Target = "/some/target"; Title = None }
    testRoundTrip expected
           
[<Fact>]
let ``Link with title`` () =
    let expected = emptyDocument |> addLink { Text = "some alt"; Target = "/some/target"; Title = Some "A title" }
    testRoundTrip expected
              
[<Fact>]
let ``Unordered list no indents`` () =
    let expected = emptyDocument |> addUnorderedList [
        { Text = "one"; Intend = 0 }
        { Text = "two"; Intend = 0 }
    ]
    testRoundTrip expected
                  
[<Fact>]
let ``Unordered list with indents`` () =
    let expected = emptyDocument |> addUnorderedList [
        { Text = "one.1"; Intend = 0 }
        { Text = "one.2"; Intend = 0 }
        { Text = "two.1"; Intend = 1 }
        { Text = "two.2"; Intend = 1 }
    ]
    testRoundTrip expected
                  
[<Fact>]
let ``Ordered list no indents single`` () =
    let expected = emptyDocument |> addOrderedList [
        { Text = "one"; Intend = 0 }
    ]
    testRoundTrip expected
                      
[<Fact>]
let ``Ordered list no indents`` () =
    let expected = emptyDocument |> addOrderedList [
        { Text = "one"; Intend = 0 }
        { Text = "two"; Intend = 0 }
    ]
    testRoundTrip expected
                  
[<Fact>]
let ``Ordered list with indents`` () =
    let expected = emptyDocument |> addOrderedList [
        { Text = "one.1"; Intend = 0 }
        { Text = "one.2"; Intend = 0 }
        { Text = "two.1"; Intend = 1 }
        { Text = "two.2"; Intend = 1 }
    ]
    testRoundTrip expected
    
[<Fact>]
let ``Strikethrough`` () =
    let expected = emptyDocument |> addStrikeThrough text
    testRoundTrip expected
    
[<Fact>]
let ``Strong emphasis`` () =
    let expected = emptyDocument |> addStrongEmphasis text
    testRoundTrip expected
    
[<Fact>]
let ``Emphasis`` () =
    let expected = emptyDocument |> addEmphasis text
    testRoundTrip expected
    
[<Fact>]
let ``Text with words`` () =
    let expected = emptyDocument |> addText text
    testRoundTrip expected
       
[<Fact>]
let ``Paragraph`` () =
    let expected = emptyDocument |> addParagraph text
    testRoundTrip expected
     
[<Fact(Skip = "Not implemented yet")>]
let ``Table`` () =
    let expected = emptyDocument |> addTable [] []
    testRoundTrip expected
         
[<Fact>]
let ``Heading 1`` () =
    let expected = emptyDocument |> addH1 text
    testRoundTrip expected
          
[<Fact>]
let ``Heading 2`` () =
    let expected = emptyDocument |> addH2 text
    testRoundTrip expected
          
[<Fact>]
let ``Heading 3`` () =
    let expected = emptyDocument |> addH3 text
    testRoundTrip expected
          
[<Fact>]
let ``Heading 4`` () =
    let expected = emptyDocument |> addH4 text
    testRoundTrip expected
          
[<Fact>]
let ``Heading 5`` () =
    let expected = emptyDocument |> addH5 text
    testRoundTrip expected
          
[<Fact>]
let ``Heading 6`` () =
    let expected = emptyDocument |> addH6 text
    testRoundTrip expected
