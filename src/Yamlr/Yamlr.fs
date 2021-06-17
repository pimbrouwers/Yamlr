namespace Yamlr

open System
open System.IO

type Yaml =
    | YamlNull  
    | YamlBool     of bool
    | YamlString   of string
    | YamlNumber   of decimal
    | YamlFloat    of float            
    | YamlSequence of elements : Yaml array
    | YamlMapping  of scalars : (string * Yaml) array

module private Constants = 
    [<Literal>] 
    let TabChar = ' '

    [<Literal>]
    let TabWidth = 4
    
    [<Literal>]
    let ListChar = '-'

    [<Literal>]
    let NullChar = ""

    [<Literal>]
    let QuoteChar = '''

    [<Literal>]
    let ScalarChar = ':'

    [<Literal>]
    let TrueStr = "true"

    [<Literal>]
    let FalseStr = "false"

open Constants

module Yaml =        
    let serialize (yaml : Yaml) : string = 
        let w = new StringWriter(Globalization.CultureInfo.InvariantCulture)

        let increaseIndent indent = 
            indent + TabWidth

        let newLine () =
            w.WriteLine ()

        let rec serializeYaml indent yaml =             
            let tab = new String(TabChar, indent)

            match yaml with
            | YamlNull     -> w.Write NullChar
            | YamlBool b   -> w.Write (if b then TrueStr else FalseStr)
            | YamlNumber n -> w.Write n
            | YamlFloat f  -> w.Write f
            | YamlString s -> 
                w.Write QuoteChar
                w.Write s
                w.Write QuoteChar

            | YamlSequence elements -> 
                for i = 0 to elements.Length - 1 do
                    if i > 0 || indent > 0 then newLine ()

                    if tab.Length > 0 then w.Write tab

                    w.Write ListChar
                    w.Write TabChar

                    serializeYaml indent elements.[i]

            | YamlMapping scalars ->
                for i = 0 to scalars.Length - 1 do                    
                    if i > 0 || indent > 0 then newLine ()
                    
                    if tab.Length > 0 then w.Write tab

                    let (k, v) = scalars.[i]
                    
                    w.Write k
                    w.Write ScalarChar
                    w.Write TabChar
                    
                    serializeYaml (increaseIndent indent) v                    

        serializeYaml 0 yaml

        w.GetStringBuilder().ToString()