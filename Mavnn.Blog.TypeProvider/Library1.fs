module Mavnn.Blog.TypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

type Id () =
    member val UniqueId = Guid() with get, set
    member val Name = "" with get, set

let private embeddedId (id : Id) =
    let guid = sprintf "%A" (id.UniqueId)
    let name = id.Name
    <@ Id(UniqueId = Guid(guid), Name = name) @>

type Port () =
    member val Id = Id() with get, set
    member val Type = "" with get, set

let private embeddedPort (port : Port) =
    let idExpr = embeddedId port.Id
    let type' = port.Type
    <@ Port(Id = %idExpr, Type = type') @>

type Node () =
    member val Id = Id() with get, set
    member val Ports = Collections.Generic.List<Port>() with get, set

let private embeddedNode (node : Node) =
    let idExpr = embeddedId node.Id
    let portsExpr adder = 
        <@
            let outPorts = Collections.Generic.List<Port>()
            (%adder) outPorts
            outPorts
        @>
    let adder =
        let portExprs =
            Seq.map (fun port -> embeddedPort port) (node.Ports)
            |> Seq.toList
        let rec builder expr remaining =
            match remaining with
            | h::t ->
                builder
                    <@ fun (ports : Collections.Generic.List<Port>) ->
                            (%expr) ports
                            ports.Add(%h) @>
                    t
            | [] ->
                expr
        builder
            <@ fun (ports : Collections.Generic.List<Port>) -> () @>
            portExprs
    <@ Node(Id = %idExpr, Ports = (%portsExpr adder)) @>

type InputPort = | InputPort of Port
type OutputPort = | OutputPort of Port

type nodeInstance =
    {
        Node : Node
        InstanceId : Id
        Config : string
    }

module NodeInstance =
    let create node name guid config =
        { Node = node; InstanceId = Id(Name = name, UniqueId = guid); Config = config }    

[<TypeProvider>]
type MavnnProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Mavnn.Blog.TypeProvider.Provided"
    let asm = Assembly.GetExecutingAssembly()

    let mavnnProvider = ProvidedTypeDefinition(asm, ns, "MavnnProvider", Some(typeof<obj>))

    let parameters = [ProvidedStaticParameter("PathToJson", typeof<string>)]

    do mavnnProvider.DefineStaticParameters(parameters, fun typeName args ->
        let pathToJson = args.[0] :?> string

        let provider = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>, HideObjectMethods = true)

        let nodes = JsonConvert.DeserializeObject<seq<Node>>(IO.File.ReadAllText(pathToJson))
                    |> Seq.map (fun n -> n.Id.UniqueId.ToString(), n)
                    |> Map.ofSeq

        let GetNode id =
            nodes.[id]

        let ports =
            nodes
            |> Map.toSeq
            |> Seq.map (fun (_, node) -> node.Ports)
            |> Seq.concat
            |> Seq.map (fun p -> p.Id.UniqueId.ToString(), p)
            |> Map.ofSeq

        let GetPort id =
            ports.[id]

        let addInputPort (inputs : ProvidedTypeDefinition) (port : Port) =
            let port = ProvidedProperty(
                            port.Id.Name, 
                            typeof<InputPort>, 
                            GetterCode = fun args -> 
                                let id = port.Id.UniqueId.ToString()
                                let expr = embeddedPort <| GetPort id
                                <@@ %expr |> InputPort @@>)
            inputs.AddMember(port)

        let addOutputPort (outputs : ProvidedTypeDefinition) (port : Port) =
            let port = ProvidedProperty(
                            port.Id.Name, 
                            typeof<OutputPort>, 
                            GetterCode = fun args -> 
                                let id = port.Id.UniqueId.ToString()
                                let expr = embeddedPort <| GetPort id
                                <@@ %expr |> OutputPort @@>)
            outputs.AddMember(port)

        let addPorts inputs outputs (portList : seq<Port>) =
            portList
            |> Seq.iter (fun port -> 
                            match port.Type with
                            | "input" -> addInputPort inputs port
                            | "output" -> addOutputPort outputs port
                            | _ -> failwithf "Unknown port type for port %s/%s" port.Id.Name (port.Id.UniqueId.ToString()))

        let createNodeType id (node : Node) =
            let nodeType = ProvidedTypeDefinition(node.Id.Name, Some typeof<nodeInstance>)
            let ctor = ProvidedConstructor(
                        [
                            ProvidedParameter("Name", typeof<string>)
                            ProvidedParameter("UniqueId", typeof<Guid>)
                            ProvidedParameter("Config", typeof<string>)
                        ],
                        InvokeCode =
                            fun [name;unique;config] -> 
                                let nodeExpr = embeddedNode <| GetNode id
                                <@@ NodeInstance.create (%nodeExpr) (%%name:string) (%%unique:Guid) (%%config:string) @@>)
            nodeType.AddMember(ctor)

            let addInputOutput () =
                let outputs = ProvidedTypeDefinition("Outputs", Some typeof<obj>)
                let outputCtor = ProvidedConstructor([], InvokeCode = fun args -> <@@ obj() @@>)
                outputs.AddMember(outputCtor)
                outputs.HideObjectMethods <- true

                let inputs = ProvidedTypeDefinition("Inputs", Some typeof<obj>)
                let inputCtor = ProvidedConstructor([], InvokeCode = fun args -> <@@ obj() @@>)
                inputs.AddMember(inputCtor)
                inputs.HideObjectMethods <- true
                addPorts inputs outputs node.Ports

                // Add the inputs and outputs types of nested types under the Node type
                nodeType.AddMembers([inputs;outputs])

                // Now add some instance properties to expose them on a node instance.
                let outputPorts = ProvidedProperty("OutputPorts", outputs, [],
                                    GetterCode = fun args -> <@@ obj() @@>)
                let inputPorts = ProvidedProperty("InputPorts", inputs, [],
                                    GetterCode = fun args -> <@@ obj() @@>)
                [inputPorts;outputPorts]

            nodeType.AddMembersDelayed(addInputOutput)

            provider.AddMember(nodeType)

        let createTypes pathToJson =
            nodes |> Map.map createNodeType |> Map.toList |> List.iter (fun (k, v) -> v)
            
        createTypes pathToJson
        
        provider)

    do
        this.AddNamespace(ns, [mavnnProvider])

[<assembly:TypeProviderAssembly>]
do ()