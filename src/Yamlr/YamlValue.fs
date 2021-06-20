namespace Yamlr

open System

/// Represents a YAML value. Large numbers that do not fit in the
/// Decimal type are represented using the Float case, while
/// smaller numbers are represented as decimals to avoid precision loss.
type YamlValue =
    | YamlNull  
    | YamlBool   of bool
    | YamlString of string
    | YamlNumber of decimal
    | YamlFloat  of float            
    | YamlList   of elements : YamlValue array
    | YamlMap    of elements : (string * YamlValue) array

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

    let NullChar = String.Empty

    [<Literal>]
    let QuoteChar = '''

    [<Literal>]
    let DoubleQuoteChar = '"'

    [<Literal>]
    let TrueStr = "true"

    [<Literal>]
    let FalseStr = "false"
