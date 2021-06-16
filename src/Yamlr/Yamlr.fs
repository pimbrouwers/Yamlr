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

    member this.WriteTo (w : TextWriter) = 
        let rec serialize indent yaml =             
            let tab = new String(' ', indent)

            match yaml with
            | Null           -> w.Write String.Empty
            | Bool b         -> w.Write (if b then "true" else "false")
            | String s       -> w.Write("'{0}'", s)
            | Number n       -> w.Write(n)
            | Float f        -> w.Write(f)
            | List elements  ->                 
                for i = 0 to elements.Length - 1 do
                    if i > 0 || indent > 0 then w.WriteLine()
                    w.Write(tab)
                    w.Write("- ")
                    serialize indent elements.[i]
            | Mapping scalars ->
                for i = 0 to scalars.Length - 1 do
                    let (k, v) = scalars.[i]
                    if i > 0 || indent > 0 then w.WriteLine()
                    w.Write("{0}{1}: ", tab, k)
                    let indent = indent + 4
                    serialize indent v                    

        serialize 0 this

    override this.ToString() =
        let w = new StringWriter(Globalization.CultureInfo.InvariantCulture)
        this.WriteTo(w)
        w.GetStringBuilder().ToString()

module Yaml =    
    let serialize (yaml : Yaml) = yaml.ToString()