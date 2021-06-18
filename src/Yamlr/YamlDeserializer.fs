﻿namespace Yamlr

open System
open System.Globalization
open System.IO
open Constants

type internal YamlDeserializer (rd : StreamReader) =
    let whitespace = (char)32

    let getInlineContent (sep : char array) (line : string) : string array = 
        if line.Length < 2 then [||]
        else 
            // strip enclosing chars (i.e., '[]' or '{}')
            line.Substring(1, line.Length - 2).Trim().Split(sep)

    let parseScalar (str : string) : YamlValue =
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
                | None   -> YamlString s // can't parse the numeric so return as YamlString

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

    let parsePairing (str : string) : string * YamlValue =        
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
            // can't find ':' separator so return as a null value pair with string as key
            str, YamlNull
        else 
            // trim leading and trailing delimiter for delimited (i.e. single- and double-quote keys)
            let startIndex = if isDelimited then 1 else 0
            let endIndex = if isDelimited then colonIndex - 2 else colonIndex
            let key = str.Substring(startIndex, endIndex)
    
            let value = parseScalar (str.Substring(colonIndex + 1))
            
            key, value

    let rec parseValue () =         
        match readLine rd with
        | false, _   -> YamlNull
        | true, line -> 
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

    member _.Deserialize () : YamlValue =        
        let yaml = parseValue ()
        yaml
