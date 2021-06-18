module Yamlr.Yaml

open System
open System.Globalization
open System.IO
open System.Text

let private newYamlReader (yamlStream : Stream) : StreamReader = 
    new StreamReader(yamlStream, Encoding.UTF8, true, 1024)

let deserialize (yamlStr : String) : YamlValue =
    use str = new MemoryStream()
    use w = new StreamWriter(str)
    w.Write(yamlStr)
    w.Flush ()
    str.Position <- 0L        
    use rd = newYamlReader str
    let parser = YamlDeserializer(rd)
    parser.Deserialize ()

let deserializeStream (yamlStream : Stream) : YamlValue =
    use rd = newYamlReader yamlStream
    let parser = YamlDeserializer(rd)
    parser.Deserialize ()

let serialize (yaml : YamlValue) : string = 
    let w = new StringWriter(CultureInfo.InvariantCulture)
    let serializer = YamlSerializer(yaml)
    serializer.Serialize w