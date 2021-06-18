module YamlDeserializer.Tests

open Xunit
open Yamlr
open FsUnit.Xunit

[<Fact>]
let ``Scalar Empty string produces YamlNull`` () =
    "" |> Yaml.deserialize |> should equal YamlNull

[<Fact>]
let ``Scalar decimal string produces YamlNumber`` () =
    "1.0" |> Yaml.deserialize |> should equal (YamlNumber 1.0M)

[<Fact>]
let ``Scalar bool string produces YamlBool`` () =
    "false" |> Yaml.deserialize |> should equal (YamlBool false)

[<Fact>]
let ``Scalar string literal produces YamlString`` () =
    "yamlr" |> Yaml.deserialize |> should equal (YamlString "yamlr")

[<Fact>]
let ``Scalar single-quoted string literal produces YamlString`` () =
    "'yamlr'" |> Yaml.deserialize |> should equal (YamlString "yamlr")

[<Fact>]
let ``Scalar double-quoted string literal produces YamlString`` () =
    "\"yamlr\"" |> Yaml.deserialize |> should equal (YamlString "yamlr")

[<Fact>]
let ``Inline list produces YamlList`` () =
    "['yamlr', 1.0, false, ]" 
    |> Yaml.deserialize 
    |> should equal (YamlList [| 
        YamlString "yamlr"
        YamlNumber 1.0M
        YamlBool false
        YamlNull
    |])

[<Fact>]
let ``Inline map produces YamlMap`` () =
    "{name: \"yamlr\", version: 1.0, beta: false, issues:}"
    |> Yaml.deserialize
    |> should equal (YamlMap [|
        "name", YamlString "yamlr"
        "version", YamlNumber 1.0M
        "beta", YamlBool false
        "issues", YamlNull
    |])

[<Fact>]
let ``Inline map with single-quoted keys produces YamlMap`` () =
    "{'name': \"yamlr\", 'version': 1.0, 'beta': false, 'issues':}"
    |> Yaml.deserialize
    |> should equal (YamlMap [|
        "name", YamlString "yamlr"
        "version", YamlNumber 1.0M
        "beta", YamlBool false
        "issues", YamlNull
    |])

[<Fact>]
let ``Inline map with double-quoted keys produces YamlMap`` () =
    "{\"name\": \"yamlr\", \"version\": 1.0, \"beta\": false, \"issues\":}"
    |> Yaml.deserialize
    |> should equal (YamlMap [|
        "name", YamlString "yamlr"
        "version", YamlNumber 1.0M
        "beta", YamlBool false
        "issues", YamlNull
    |])

[<Fact>]
let ``Multiline list produces YamlList`` () =
    "- 'test
- 1.0
- false"
    |> Yaml.deserialize
    |> should equal (YamlList [| YamlString "test"; YamlNumber 1.0M; YamlBool false |])

[<Fact>]
let ``Multiline mapping produces YamlMap`` () =    
    "name: 'yamlr'
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
    |> Yaml.deserialize
    |> should equal (YamlMap [|
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
    |])