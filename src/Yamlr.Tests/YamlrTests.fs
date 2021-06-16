module Yamlr.Tests

open Xunit
open Yamlr
open FsUnit.Xunit

[<Fact>]
let ``Yaml.Null serializes to empty string`` () =    
    Yaml.Null |> Yaml.serialize |> should equal ""

[<Fact>]
let ``Yaml.Bool serializes to boolean string`` () =    
    Yaml.Bool false |> Yaml.serialize |> should equal "false"

[<Fact>]
let ``Yaml.String serializes to provided string quoted`` () =    
    let str = "yamlr"
    let quotedStr = sprintf "'%s'" str
    str |> Yaml.String |> Yaml.serialize |> should equal quotedStr

[<Fact>]
let ``Yaml.Number serializes to provided number for int`` () =    
    let num = 1
    let numStr = "1"
    num |> decimal |> Yaml.Number |> Yaml.serialize |> should equal numStr

[<Fact>]
let ``Yaml.Number serializes to provided number for decimal`` () =    
    let dec = 1.1M
    let decStr = "1.1"
    dec |> Yaml.Number |> Yaml.serialize |> should equal decStr

[<Fact>]
let ``Yaml.Float serializes to provided number for float`` () =    
    let dec = 1.1
    let decStr = "1.1"
    dec |> Yaml.Float |> Yaml.serialize |> should equal decStr

[<Fact>]
let ``Yaml.List serializes to list of serialized Yaml`` () =    
    let expected = "- 'test'
- 1.0
- false"
    Yaml.List [| Yaml.String "test"; Yaml.Number 1.0M; Yaml.Bool false |]
    |> Yaml.serialize
    |> should equal expected

[<Fact>]
let ``Yaml.Mapping serializes to nested serialized Yaml scalars`` () =    
    let expected = "name: 'yamlr'
version: 1.0
kind: 'library'
beta: false
versionHistory: 
    - 0.1
    - 0.2
    - 0.3
    - 1.0
api: 
    serialize: 'converts yaml to string'
    parse: 'convert string to yaml'
    metadata: 
        license: 'apache'
        keywords: 
            - 'serializer'
            - 'f#'"
    Yaml.Mapping [|
        "name", Yaml.String "yamlr"
        "version", Yaml.Number 1.0M
        "kind", Yaml.String "library"
        "beta", Yaml.Bool false
        "versionHistory", Yaml.List [| 
            Yaml.Number 0.1M
            Yaml.Number 0.2M
            Yaml.Number 0.3M
            Yaml.Number 1.0M
        |]
        "api", Yaml.Mapping [| 
            "serialize", Yaml.String "converts yaml to string"
            "parse", Yaml.String "convert string to yaml" 
            "metadata", Yaml.Mapping [|
                "license", Yaml.String "apache"
                "keywords", Yaml.List [|
                    Yaml.String "serializer"
                    Yaml.String "f#"
                |]
            |]
        |]
    |]
    |> Yaml.serialize
    |> should equal expected
