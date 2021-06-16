module Yamlr

open System
open System.IO

type Yaml =
    | Null  
    | Bool    of bool
    | String  of string
    | Number  of decimal
    | Float   of float            
    | List    of elements : Yaml array
    | Mapping of scalars : (string * Yaml) array
   
module Yaml =    
    [<Literal>] 
    let private tabChar = ' '

    [<Literal>]
    let private quoteChar = '''

    [<Literal>]
    let private listStr = '-'

    [<Literal>]
    let private scalarStr = ':'

    [<Literal>]
    let private trueStr = "true"

    [<Literal>]
    let private falseStr = "false"

    let serialize (yaml : Yaml) = 
        let w = new StringWriter(Globalization.CultureInfo.InvariantCulture)

        let newLine () = w.WriteLine ()

        let rec serializeYaml indent yaml =             
            let tab = new String(tabChar, indent)

            match yaml with
            | Null           -> w.Write String.Empty
            | Bool b         -> w.Write (if b then trueStr else falseStr)
            | Number n       -> w.Write n
            | Float f        -> w.Write f
            | String s       -> 
                w.Write quoteChar
                w.Write s
                w.Write quoteChar

            | List elements  -> 
                for i = 0 to elements.Length - 1 do
                    if i > 0 || indent > 0 then newLine ()

                    if tab.Length > 0 then w.Write tab

                    w.Write listStr
                    w.Write tabChar

                    serializeYaml indent elements.[i]

            | Mapping scalars ->
                for i = 0 to scalars.Length - 1 do                    
                    if i > 0 || indent > 0 then newLine ()
                    
                    if tab.Length > 0 then w.Write tab

                    let (k, v) = scalars.[i]
                    
                    w.Write k
                    w.Write scalarStr
                    w.Write tabChar
                    
                    let indent = indent + 4
                    
                    serializeYaml indent v                    

        serializeYaml 0 yaml

        w.GetStringBuilder().ToString()