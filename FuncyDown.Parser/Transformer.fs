namespace FuncyDown

open System.Collections.Generic
open FuncyDown.Element

type Context =
    abstract member PreviousElement : unit -> Element option
    abstract member NextElement : unit -> Element option
    abstract member Index : unit -> int
    abstract member Set<'a> : 'a -> unit
    abstract member Get<'a> : unit -> 'a option
    abstract member Add<'a> : 'a -> unit
    abstract member Retrieve<'a> : unit -> 'a array

type TransformationContext(elements) =
    let items = Dictionary<string,obj>()
    let collections = Dictionary<string,obj ResizeArray>()
    let mutable arr : Element array = elements |> List.toArray
    member private this.TypeKey<'a>() = typeof<'a>.FullName
    member val index = 0 with get, set
    
    interface Context with
        member this.PreviousElement() = if this.index <= 0 then None else Some arr.[this.index-1]
        member this.NextElement() =
            if this.index < arr.Length - 1 then
                Some arr.[this.index+1]
            else None
        member this.Index() = this.index
        member this.Set<'a>(value : 'a) =
            let k = this.TypeKey<'a>()
            if items.ContainsKey(k) then
                items.[k] <- value
            else items.Add(k, value)
        member this.Get<'a>() =
            let k = this.TypeKey<'a>()
            if items.ContainsKey(k) then
                Some (items.[k] :?> 'a)
            else None
        member this.Add<'a>(value : 'a) =
            let k = this.TypeKey<'a>()
            if not(collections.ContainsKey(k)) then
                collections.Add(this.TypeKey<'a>(), ResizeArray())
            collections.[k].Add(value)
            
        member this.Retrieve<'a>() =
            let k = this.TypeKey<'a>()
            if collections.ContainsKey(k) then
                collections.[k] |> Seq.toArray |> Array.map (fun o -> o :?> 'a)
            else Array.empty
                
        
type Transform = Context -> Element -> Element list

module Transformer =
    open FuncyDown.Document
    
    let transform (transformations : Transform list) (doc : Document) =
        let context = TransformationContext(doc.Elements)
        let elements = doc.Elements |> List.toArray
        let defaultTransform ctx el = [el]
        let transformations =
            if transformations.Length = 0 then
                [defaultTransform]
            else transformations
        let applyTransformations i el =
            let element = elements.[i]
            transformations
            |> List.map (fun f ->
                context.index <- i
                f context element)
            |> List.concat
        {
            Elements = doc.Elements |> List.mapi applyTransformations |> List.concat
        }