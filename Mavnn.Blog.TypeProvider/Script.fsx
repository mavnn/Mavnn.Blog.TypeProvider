#r "bin/Debug/Mavnn.Blog.TypeProvider.dll"
open Mavnn.Blog.TypeProvider

let simpleNode = Provided.Simple("simple", System.Guid.NewGuid(), "A config string")
let splitNode = Provided.Split("split", System.Guid.NewGuid(), "More config")

simpleNode.InputPorts.Input
splitNode.OutputPorts.Output1
splitNode.OutputPorts.Output2

