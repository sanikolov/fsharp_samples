module Seriallog

open System.Threading
open System

type Msgs = Exit of AsyncReplyChannel<string> | Message of string

type SerialLogging() = 
    let mbox = MailboxProcessor.Start(fun inbox -> 
        let rec loop () = async {
                let! inp = inbox.Receive()
                match inp with
                | Message msg -> printfn "async %d: %s" Thread.CurrentThread.ManagedThreadId msg
                                 Console.Out.Flush()
                                 return! loop ()
                | Exit(ch) -> ch.Reply(String.Empty)
            }
        loop ()
        )

    member this.Write msg = mbox.Post (Message("thr " + (string) Thread.CurrentThread.ManagedThreadId + ": " + msg))
    member this.Exit () = mbox.PostAndReply(Exit)