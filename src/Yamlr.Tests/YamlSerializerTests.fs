module YamlSerializer.Tests

open Xunit
open Yamlr
open FsUnit.Xunit

[<Fact>]
let ``YamlNull serializes to empty string`` () =    
    YamlNull |> Yaml.serialize |> should equal ""

[<Fact>]
let ``YamlBool serializes to boolean string`` () =    
    YamlBool false |> Yaml.serialize |> should equal "false"

[<Fact>]
let ``YamlString serializes to provided string quoted`` () =    
    let str = "yamlr"
    let quotedStr = sprintf "'%s'" str
    str |> YamlString |> Yaml.serialize |> should equal quotedStr

[<Fact>]
let ``YamlNumber serializes to provided number for int`` () =    
    let num = 1
    let numStr = "1"
    num |> decimal |> YamlNumber |> Yaml.serialize |> should equal numStr

[<Fact>]
let ``YamlNumber serializes to provided number for decimal`` () =    
    let dec = 1.1M
    let decStr = "1.1"
    dec |> YamlNumber |> Yaml.serialize |> should equal decStr

[<Fact>]
let ``YamlFloat serializes to provided number for float`` () =    
    let dec = 1.1
    let decStr = "1.1"
    dec |> YamlFloat |> Yaml.serialize |> should equal decStr

[<Fact>]
let ``YamlList serializes to list of serialized Yaml`` () =    
    let expected = "- 'test'
- 1.0
- false"
    YamlList [| YamlString "test"; YamlNumber 1.0M; YamlBool false |]
    |> Yaml.serialize
    |> should equal expected

[<Fact>]
let ``YamlMapping serializes to nested serialized Yaml scalars`` () =    
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
    YamlMap [|
        "name", YamlString "yamlr"
        "version", YamlNumber 1.0M
        "kind", YamlString "library"
        "beta", YamlBool false
        "versionHistory", YamlList [| 
            YamlNumber 0.1M
            YamlNumber 0.2M
            YamlNumber 0.3M
            YamlNumber 1.0M
        |]
        "api", YamlMap [| 
            "serialize", YamlString "converts yaml to string"
            "parse", YamlString "convert string to yaml" 
            "metadata", YamlMap [|
                "license", YamlString "apache"
                "keywords", YamlList [|
                    YamlString "serializer"
                    YamlString "f#"
                |]
            |]
        |]
    |]
    |> Yaml.serialize
    |> should equal expected
