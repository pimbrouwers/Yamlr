namespace Yamlr

open System
open System.IO
open Constants
open System.Web

type internal YamlSerializer (yaml : YamlValue) =
    /// Serializes the YamlValue to the specified System.IO.TextWriter.
    member _.Serialize (w : StringWriter) =    
        let increaseIndent indent = 
            indent + TabWidth

        let rec serializeYaml (indent : int) (yaml : YamlValue) =
            let tab = new String(TabChar, indent)

            match yaml with
            | YamlNull     -> w.Write NullChar
            | YamlBool b   -> w.Write (if b then TrueStr else FalseStr)
            | YamlNumber n -> w.Write n
            | YamlFloat f  -> w.Write f
            | YamlString s -> 
                w.Write DoubleQuoteChar
                w.Write s
                w.Write DoubleQuoteChar

            | YamlList elements -> 
                for i = 0 to elements.Length - 1 do
                    if i > 0 || indent > 0 then w.WriteLine ()

                    if tab.Length > 0 then w.Write tab

                    w.Write ListChar
                    w.Write TabChar

                    let element = elements.[i]
                    match element with
                    | YamlList _ 
                    | YamlMap _  -> serializeYaml (increaseIndent indent) element
                    | _          -> serializeYaml indent element

            | YamlMap scalars ->
                for i = 0 to scalars.Length - 1 do                    
                    if i > 0 || indent > 0 then w.WriteLine ()
                    
                    if tab.Length > 0 then w.Write tab

                    let (k, v) = scalars.[i]
                    
                    w.Write k
                    w.Write MapChar
                    w.Write TabChar
                    
                    serializeYaml (increaseIndent indent) v                    

        serializeYaml 0 yaml

        w.GetStringBuilder().ToString()
