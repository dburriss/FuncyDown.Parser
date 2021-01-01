﻿namespace FuncyDown
open System
open FParsec
open FuncyDown.Element

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

module String =
    let emptyToOption s = if String.IsNullOrEmpty s then None else Some s

module ParserHelpers =
    // https://www.quanttec.com/fparsec/tutorial.html#fs-value-restriction
    
    let ws0 : Parser<_> = spaces 
    let empty : Parser<_> = ws0.>>.eof
    
    // Applies popen, then pchar repeatedly until pclose succeeds,
    // returns the string in the middle
    let manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    let indents :Parser <_> = many (satisfy (fun c -> c = '\t'))

module ElementParser =
    open ParserHelpers
    let horizontalRule : Parser<_> =
        let s =  "---"
        (skipString s)>>?newlineReturn HorizontalRule
    let blockQuote : Parser<_> = (skipString ">")>>.ws0>>.restOfLine true |>> BlockQuote

    let blockCode : Parser<_> =
        let codeQuotes = skipString "```"
        let codeQuotesAndNewLine =
             (newline >>? skipString "```" .>>? newline)
             <|> (newline >>? skipString "```")
             <|> (skipString "```" .>>? newline)
             <|> (skipString "```")

        let lang = manyCharsTill anyChar newline
        let code = manyCharsTill anyChar codeQuotesAndNewLine
        let langAndCodeStrings = codeQuotes >>? lang .>>. code
        
        let toLang s = String.emptyToOption s
        let toCode (langS, codeS) = { Language = (toLang langS); Code = codeS }
        langAndCodeStrings |>> (toCode >> BlockCode)

    let inlineCode : Parser<_> =
        let codeQuotes = skipString "`"
        let code = manyCharsTill anyChar codeQuotes
        let toCode s = { Language = None; Code = s }
        codeQuotes >>. code |>> (toCode >> InlineCode)
    
    let image : Parser<_> =
        let alt = skipString "![" >>. manyCharsTill anyChar (skipChar ']')
        let src = skipChar '(' >>. manyCharsTill anyChar (skipChar ')') |>> fun s -> (s,String.Empty)
        let srcAndTitle = (skipChar '(' >>. manyCharsTill anyChar (skipString " \""))
                          .>>.(manyCharsTill anyChar (skipChar '"' >>. ws0 >>. skipChar ')'))
        let toTitle s = String.emptyToOption s
        pipe2 alt (attempt srcAndTitle <|> src) (fun a (tar,tit) -> Image { AltText = a;  Target = tar; Title = toTitle tit})
            
    let link : Parser<_> =
        let alt = skipString "[" >>. manyCharsTill anyChar (skipChar ']')
        let src = skipChar '(' >>. manyCharsTill anyChar (skipChar ')') |>> fun s -> (s,String.Empty)
        let srcAndTitle = (skipChar '(' >>. manyCharsTill anyChar (skipString " \""))
                          .>>.(manyCharsTill anyChar (skipChar '"' >>. ws0 >>. skipChar ')'))
        let toTitle s = String.emptyToOption s
        pipe2 alt (attempt srcAndTitle <|> src) (fun a (tar,tit) -> Link { Text = a;  Target = tar; Title = toTitle tit})
    
    let private unorderedListRest : Parser<_> =
        let indents = many (satisfy (fun c -> c = '\t'))
        let items = indents.>>.?(skipString "* ">>?(manyCharsTill anyChar newline))
        let list = many items
        list
            
    let private orderedListRest : Parser<_> =
        let indents = many (satisfy (fun c -> c = '\t'))
        let ordered = (skipSatisfy isDigit .>>. skipString ". ")
        let items = indents.>>.(ordered>>.(manyCharsTill anyChar newline))
        let list = many items
        list
    
    let list : Parser<_> =
        let prepend = fun (h,ts) -> List.append [(List.empty,h)] ts
        
        let orderedFirst = (skipSatisfy isDigit .>>. skipString ". ")>>.(manyCharsTill anyChar newline)
        
        let ordered = orderedFirst.>>.orderedListRest
                        |>> fun (h,ts) -> List.append [(List.empty,h)] ts
                        |>> (List.map (fun (ts,s) -> { Text = s; Intend = ts |> List.length }) >> OrderedList)
                      
        let unorderedFirst = manyCharsBetween (skipString "* ") newline anyChar
        
        let unordered = unorderedFirst.>>.unorderedListRest
                      |>> prepend
                      |>> (List.map (fun (ts,s) -> { Text = s; Intend = ts |> List.length }) >> UnorderedList)
        (ordered <|> unordered)
        
    let text : Parser<_> = (restOfLine true |>> Text) <|> (empty >>% Text "")
    let elements = choice
                       [
                         horizontalRule
                         blockQuote
                         blockCode
                         inlineCode
                         image
                         link
                         list
                         // text must be last
//                         text
                       ] <?> "Markdown elements parser"
    let markdownParser = (manyTill elements eof) <?> "Markdown parser"

module Document =
    open System.Diagnostics
    open FuncyDown.Document
    open FParsec
    let parse (markdown : string) : Document =
        if String.IsNullOrWhiteSpace markdown then
            { Elements = List.empty }
        else
            let parser = ElementParser.markdownParser
            match (run parser markdown) with
            | Success (xs,_,_) -> { Elements = xs }
            | Failure (errorMsg,error,state) ->
                Debug.WriteLine(sprintf "ERROR: %A" error)
                Debug.WriteLine(sprintf "USER STATE: %A" state)
                
                errorMsg |> failwith errorMsg
        