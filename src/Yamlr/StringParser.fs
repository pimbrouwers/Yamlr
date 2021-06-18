﻿namespace Yamlr

open System
open System.Globalization

/// Conversions from string to string/int/int64/decimal/float/boolean/datetime/timespan/guid options
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

