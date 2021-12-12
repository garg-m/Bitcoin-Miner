#r "nuget: Akka.FSharp" 
#r "nuget: Akka.TestKit"
#r "nuget: Akka"
open System
open Akka.FSharp
open Akka.Actor
open System.Security.Cryptography
open System.Diagnostics

     
type Command = 
| Start
| Continue
| Message of string
| Exit
    


Console.WriteLine "Enter the value of k: "
let zeroes = System.Console.ReadLine()
let mutable compareString = ""
let zero = int zeroes
for i = 1 to zero do
    compareString <- String.Concat(compareString, "0")

let myActorSystem = System.create "MyActorSystem" <| Configuration.load ()

let proc = Process.GetCurrentProcess()
let cpu_time_stamp = proc.TotalProcessorTime
let sw = Stopwatch.StartNew()


module Actors =

    let InputGeneratorActor (hashConverter: IActorRef) (mailbox: Actor<_>) message =
        
            let (|Message|Exit|) (str:string) =
                        match str.ToLower () with
                        | "exit" -> Exit
                        | _ -> Message(str)

            let rec sendToHashConverter () =
                let r = Random()
                let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
                let sz = Array.length chars in
                let str1 = String(Array.init 9 (fun _ -> chars.[r.Next sz]))
                let str = "gargm;" + str1
                
                hashConverter <! str
                sendToHashConverter()

            match box message with
            | :? Command as command -> 
                match command with
                | start -> sendToHashConverter ()
            | _ -> () 


    let hashConverterActor (output: IActorRef) (mailbox: Actor<_>) message =

                let HashWithHyphen = 
                    System.Text.Encoding.ASCII.GetBytes(message.ToString()) 
                    |> (new SHA256Managed()).ComputeHash 
                    |> System.BitConverter.ToString
                
                let WithoutHyphen = HashWithHyphen.Replace("-", "")
                let finalHash = string WithoutHyphen
                if finalHash.[0..(zero-1)] = compareString then
                    output <! message
                    output <! finalHash

                    let cpuTime = (proc.TotalProcessorTime-cpu_time_stamp).TotalMilliseconds
                    sw.Stop()
                    printfn "CPU time = %dms" (int64 cpuTime)
                    printfn "REAL time = %fms" sw.Elapsed.TotalMilliseconds
                    mailbox.Context.System.Terminate () |> ignore


    let outputActor message =
            let str = string message
            printfn"%s" str
        
 
let outputActor = spawn myActorSystem "outputActor" (actorOf Actors.outputActor)
let hashConverterActor = spawn myActorSystem "hashConverterActor" (actorOf2 (Actors.hashConverterActor outputActor))
let InputGeneratorActor = spawn myActorSystem "InputGeneratorActor" (actorOf2 (Actors.InputGeneratorActor hashConverterActor))

        

InputGeneratorActor <! Start

myActorSystem.WhenTerminated.Wait ()
