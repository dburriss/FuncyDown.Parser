namespace FuncyDown

module ElementParser =
    
    open System
    open FParsec
    open FuncyDown.Element
    // https://www.quanttec.com/fparsec/tutorial.html#fs-value-restriction
    type UserState = unit
    type Parser<'t> = Parser<'t, UserState>
    let ws0 : Parser<_> = spaces 
    let empty : Parser<_> = ws0.>>.eof
    
    // Applies popen, then pchar repeatedly until pclose succeeds,
    // returns the string in the middle
    let private manyCharsBetween popen pclose pchar = popen >>? manyCharsTill pchar pclose
    
    // Parses any string between popen and pclose
    let private anyStringBetween popen pclose = manyCharsBetween popen pclose anyChar

    // Parses any string between double quotes
    let private tripeQuote : Parser<_> = skipString "```" |> anyStringBetween <| skipString "```"
    let horizontalRule =
        let s =  "---"
        (skipString s)>>.newlineReturn HorizontalRule
    let blockQuote = (skipString ">")>>.ws0>>.restOfLine true |>> BlockQuote

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
        
        let toLang (s : string) = if String.IsNullOrWhiteSpace s then None else Some s
        let toCode (langS, codeS) = { Language = (toLang langS); Code = codeS }
        langAndCodeStrings |>> (toCode >> BlockCode)

    let inlineCode =
        let codeQuotes = skipString "`"
        let code = manyCharsTill anyChar codeQuotes
        let toCode s = { Language = None; Code = s }
        codeQuotes >>? code |>> (toCode >> InlineCode)
    
    let image =
        let alt = skipString "![" >>. manyCharsTill anyChar (skipChar ']')
        let src = skipChar '(' >>. manyCharsTill anyChar (skipChar ')') |>> fun s -> (s,String.Empty)
        let srcAndTitle = (skipChar '(' >>. manyCharsTill anyChar (skipString " \""))
                          .>>.(manyCharsTill anyChar (skipChar '"' >>. ws0 >>. skipChar ')'))
        let toTitle s = if String.IsNullOrEmpty s then None else Some s 
        pipe2 alt (attempt srcAndTitle <|> src) (fun a (tar,tit) -> Image { AltText = a;  Target = tar; Title = toTitle tit})
        
    let text = restOfLine true |>> Text
    let elements = choice
                       [
                         horizontalRule
                         blockQuote
                         blockCode
                         inlineCode
                         image
                         // text must be last
                         text
                       ]
    let markdownParser = (manyTill elements empty)

module Document =
    open System
    open FuncyDown.Document
    open FParsec
    let parse (markdown : string) : Document =
        if String.IsNullOrWhiteSpace markdown then
            { Elements = List.empty }
        else
            let parser = ElementParser.markdownParser
            match (run parser markdown) with
            | Success (xs,_,_) -> { Elements = xs }
            | Failure (errorMsg, _,_) -> errorMsg |> failwith errorMsg
        