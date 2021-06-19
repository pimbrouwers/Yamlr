module YamlDeserializer.Tests

open Xunit
open Yamlr
open FsUnit.Xunit

[<Fact>]
let ``Scalar Empty string produces YamlNull`` () =
    "" |> Yaml.deserialize |> should equal YamlNull

[<Theory>]
[<InlineData("1.0", 1.0)>]
[<InlineData("99", 99)>]
let ``Scalar decimal string produces YamlNumber`` (str : string, value : decimal) =
    str |> Yaml.deserialize |> should equal (YamlNumber value)

[<Theory>]
[<InlineData("false", false)>]
[<InlineData("False", false)>]
[<InlineData("FALSE", false)>]
[<InlineData("no", false)>]
[<InlineData("No", false)>]
[<InlineData("NO", false)>]
[<InlineData("off", false)>]
[<InlineData("Off", false)>]
[<InlineData("OFF", false)>]
[<InlineData("n", false)>]
[<InlineData("N", false)>]
[<InlineData("true", true)>]
[<InlineData("True", true)>]
[<InlineData("TRUE", true)>]
[<InlineData("yes", true)>]
[<InlineData("Yes", true)>]
[<InlineData("YES", true)>]
[<InlineData("on", true)>]
[<InlineData("On", true)>]
[<InlineData("ON", true)>]
[<InlineData("y", true)>]
[<InlineData("Y", true)>]
let ``Scalar bool string produces YamlBool`` (str : string, value : bool) =
    str |> Yaml.deserialize |> should equal (YamlBool value)

[<Theory>]
[<InlineData("yamlr", "yamlr")>]
[<InlineData("'yamlr'", "yamlr")>]
[<InlineData("\"yamlr\"", "yamlr")>]
let ``Scalar string literal produces YamlString`` (str : string, value : string) =
    str |> Yaml.deserialize |> should equal (YamlString value)

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
    "- 'test'
- 1.0
- false"
    |> Yaml.deserialize
    |> should equal (YamlList [| YamlString "test"; YamlNumber 1.0M; YamlBool false |])

[<Fact>]
let ``Multiline nested list produces YamlList`` () =
    "- 'test'
- 1.0
-
    - 'serializer'
    - 'f#'
- false"
    |> Yaml.deserialize
    |> should equal (YamlList [| YamlString "test"; YamlNumber 1.0M; YamlList [| YamlString "serializer"; YamlString "f#" |]; YamlBool false |])

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

