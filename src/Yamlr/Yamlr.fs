namespace Yamlr

open System
open System.Globalization
open System.IO
open System.Text

type Yaml =
    | YamlNull  
    | YamlBool   of bool
    | YamlString of string
    | YamlNumber of decimal
    | YamlFloat  of float            
    | YamlList   of elements : Yaml array
    | YamlMap    of elements : (string * Yaml) array

module internal StringParser =         
    let parseWith (tryParseFunc: string -> bool * _) = 
        tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None
      
    let parseInt16          cul = parseWith (fun str -> Int16.TryParse(str, NumberStyles.Currency, cul))
    let parseInt32          cul = parseWith (fun str -> Int32.TryParse(str, NumberStyles.Currency, cul))
    let parseInt64          cul = parseWith (fun str -> Int64.TryParse(str, NumberStyles.Currency, cul))
    let parseInt            cul = parseInt32 cul
    let parseFloat          cul = parseWith (fun str -> Double.TryParse(str, NumberStyles.Currency, cul))
    let parseDecimal        cul = parseWith (fun str -> Decimal.TryParse(str, NumberStyles.Currency, cul))
    let parseDateTime       cul = parseWith (fun str -> DateTime.TryParse(str, cul, DateTimeStyles.AllowWhiteSpaces ||| DateTimeStyles.RoundtripKind))
    let parseDateTimeOffset cul = parseWith (fun str -> DateTimeOffset.TryParse(str, cul, DateTimeStyles.AllowWhiteSpaces ||| DateTimeStyles.RoundtripKind))
    let parseTimeSpan       cul = parseWith (fun str-> TimeSpan.TryParse(str, cul))
    let parseBoolean            = parseWith Boolean.TryParse
    let parseGuid               = parseWith Guid.TryParse

module internal Constants = 
    [<Literal>]
    let CommentChar = '#'

    [<Literal>]
    let ComplexKeyChar = '?'

    [<Literal>] 
    let TabChar = ' '

    [<Literal>]
    let TabWidth = 4
    
    [<Literal>]
    let ListChar = '-'

    [<Literal>]
    let InlineListStartChar = '['

    [<Literal>]
    let MapChar = ':'
    
    [<Literal>]
    let InlineMapStartChar = '{'

    [<Literal>]
    let NullChar = ""

    [<Literal>]
    let QuoteChar = '''

    [<Literal>]
    let DoubleQuoteChar = '"'

    [<Literal>]
    let TrueStr = "true"

    [<Literal>]
    let FalseStr = "false"

    let whitespace = (char)32

open Constants

type internal YamlDeserializer (rd : StreamReader) =        
    let getInlineContent (sep : char array) (line : string) : string array = 
        if line.Length < 2 then [||]
        else 
            // strip enclosing chars (i.e., '[]' or '{}')
            line.Substring(1, line.Length - 2).Trim().Split(sep)

    let parseScalar (str : string) : Yaml =
        let str = str.Trim ()
        let c = if str.Length = 0 then Char.MinValue else str.[0]

        let isTrueBool (s : string) = 
            match s with 
            | "true" | "True" | "TRUE" 
            | "yes" | "Yes" | "YES"
            | "on" | "On" | "ON"
            | "y" | "Y" -> true
            | _ -> false

        let isFalseBool (s : string) = 
            match s with 
            | "false" | "False" | "FALSE" 
            | "no" | "No" | "NO"
            | "off" | "Off" | "OFF"
            | "N" | "N" -> true
            | _ -> false

        let parseNum (s : string) = 
            match StringParser.parseDecimal CultureInfo.InvariantCulture s with
            | Some d -> YamlNumber d
            | None   -> 
                match StringParser.parseFloat CultureInfo.InvariantCulture s with
                | Some f -> YamlFloat f
                | None   -> failwithf "Encountered invalid scalar: %s" s

        let parseString (s : string) =
            if s.Length < 2 then NullChar
            else s.Substring(1, s.Length - 2)

        match c with
        | Char.MinValue               -> YamlNull
        | '-'                         -> parseNum str
        | c when Char.IsDigit(c)      -> parseNum str
        | _ when isTrueBool str       -> YamlBool true
        | _ when isFalseBool str      -> YamlBool false        
        | QuoteChar | DoubleQuoteChar -> YamlString (parseString str)
        | _                           -> YamlString str

    let parsePairing (str : string) : string * Yaml =        
        let str = str.Trim ()
        let c = if str.Length = 0 then Char.MinValue else str.[0]
        
        // walk string until we fine the ':' key/value separator
        let rec locateQuotedKeyTerminator (delimiter : char) (i: int) (str : string) =
            let strLen = str.Length
            let twoBack = if i > 1 then str.[i-2] else Char.MinValue
            let oneBack = if i > 0 then str.[i-1] else Char.MinValue
            let curr = if i < strLen - 1 then str.[i] else whitespace
            
            if twoBack = delimiter && oneBack = ':' && Char.IsWhiteSpace curr then i - 1
            elif i = strLen then -1
            else locateQuotedKeyTerminator delimiter (i + 1) str
            
        let isDelimited, colonIndex =             
            match c with
            | QuoteChar       -> true, locateQuotedKeyTerminator ''' 0 str
            | DoubleQuoteChar -> true, locateQuotedKeyTerminator '"' 0 str
            | ComplexKeyChar  -> false, -1                
            | _               -> false, str.IndexOf(':')

        if colonIndex = -1 then 
            failwithf "Encountered invalid key/value: %s" str

        // trim leading and trailing delimiter for delimited (i.e. single- and double-quote keys)
        let startIndex = if isDelimited then 1 else 0
        let endIndex = if isDelimited then colonIndex - 2 else colonIndex
        let key = str.Substring(startIndex, endIndex)

        let value = parseScalar (str.Substring(colonIndex + 1))
        
        key, value

    let rec parseLines (hasMore : bool) (rd : StreamReader) =         
        match readLine rd with
        | true, line -> parseValue line            
        | _          -> YamlNull

    and readLine (rd : StreamReader) : bool * string  =                        
        // continue reading until we find a suitable line (i.e., non-empty and non-comment)
        if not(rd.EndOfStream) then 
            let line = (rd.ReadLine ()).Trim()
            
            if line = NullChar || line.[0] = CommentChar then 
                readLine rd
            else 
                true, line
        else 
            false, NullChar

    and parseValue (line : string) =         
        match line.[0] with
        | InlineListStartChar -> 
            // split line into string array and parse as scalars
            getInlineContent [|','|] line
            |> Array.map parseScalar
            |> YamlList 

        | InlineMapStartChar -> 
            // split line into string array and parse as key/value pairs
            getInlineContent [|','|] line
            |> Array.map parsePairing
            |> YamlMap

        | ListChar ->            
            YamlList [||]

        | _ -> parseScalar line
        
    member _.Deserialize () : Yaml =        
        let yaml = parseLines true rd                  
        yaml

type internal YamlSerializer (yaml : Yaml) =
    let w = new StringWriter(CultureInfo.InvariantCulture)

    let increaseIndent indent = 
        indent + TabWidth

    let newLine () =
        w.WriteLine ()

    member _.Serialize () =
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

            | YamlList elements -> 
                for i = 0 to elements.Length - 1 do
                    if i > 0 || indent > 0 then newLine ()

                    if tab.Length > 0 then w.Write tab

                    w.Write ListChar
                    w.Write TabChar

                    serializeYaml indent elements.[i]

            | YamlMap scalars ->
                for i = 0 to scalars.Length - 1 do                    
                    if i > 0 || indent > 0 then newLine ()
                    
                    if tab.Length > 0 then w.Write tab

                    let (k, v) = scalars.[i]
                    
                    w.Write k
                    w.Write MapChar
                    w.Write TabChar
                    
                    serializeYaml (increaseIndent indent) v                    

        serializeYaml 0 yaml

        w.GetStringBuilder().ToString()

module Yaml =
    let private newYamlReader (yamlStream : Stream) : StreamReader = 
        new StreamReader(yamlStream, Encoding.UTF8, true, 1024)
    
    let deserialize (yamlStr : String) : Yaml =
        use str = new MemoryStream()
        use w = new StreamWriter(str)
        w.Write(yamlStr)
        w.Flush ()
        str.Position <- 0L        
        use rd = newYamlReader str
        let parser = YamlDeserializer(rd)
        parser.Deserialize ()
    
    let deserializeStream (yamlStream : Stream) : Yaml =
        use rd = newYamlReader yamlStream
        let parser = YamlDeserializer(rd)
        parser.Deserialize ()
    
    let serialize (yaml : Yaml) : string = 
        let serializer = YamlSerializer(yaml)
        serializer.Serialize()