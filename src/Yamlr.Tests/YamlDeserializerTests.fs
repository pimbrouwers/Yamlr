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

//[<Fact>]
//let ``Multiline list produces YamlList`` () =
//    "- 'test'\r\n- 1.0\r\n- false"
//    |> Yaml.deserialize
//    |> should equal (YamlList [| YamlString "test"; YamlNumber 1.0M; Yaml.YamlBool false |])