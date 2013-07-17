module Tester.Main

open FParsec
open System

type Json =
    | JNumber of float
    | JString of string
    | JList of Json list
    | JObject of Map<string, Json>
    with
        override this.ToString() = sprintf "%+A" this

let parser, parserRef = createParserForwardedToRef<Json, unit>()

let float_ : Parser<float, unit> =
    pfloat

let stringInnard : Parser<string, unit> =
    manySatisfy (fun c -> c <> '"')

let string_ : Parser<string, unit> =
    between (pchar '"') (pchar '"') stringInnard

let listInnard : Parser<Json list, unit> =
    sepBy (parser .>> spaces) (pchar ',' .>> spaces)

let list_ : Parser<Json list, unit> =
    between (pchar '[' .>> spaces) (spaces >>. pchar ']') listInnard

let objectKeyValue : Parser<string * Json, unit> =
    string_ .>>. (spaces >>. pchar ':' >>. spaces >>. parser)

let objectInnard : Parser<Map<string, Json>, unit> =
    sepBy (objectKeyValue .>> spaces) (pchar ',' .>> spaces) |>> Map.ofSeq

let object_ : Parser<Map<string, Json>, unit> =
    between (pchar '{' .>> spaces) (spaces >>. pchar '}') objectInnard

do parserRef := choice [object_ |>> JObject; list_ |>> JList; string_ |>> JString; float_ |>> JNumber]
