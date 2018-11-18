open System
open System.Threading
open System.IO
open Renci.SshNet
open System.Text.RegularExpressions
open Seriallog

type stdoutput = string
type stderror = string
type exit_code = int
exception SshFailure of string * stdoutput * stderror * exit_code

let crlf = [|'\r'; '\n'|]

let execute_command (log: SerialLogging) host port user (passwd: string) script files =
   use user_pwd = new PasswordAuthenticationMethod(user, passwd)
   let (auth: AuthenticationMethod[]) = [|user_pwd|]
   let conn = ConnectionInfo(host, port, user, auth)
   let run_command cmd ignore_errors =
       use client = new SshClient(conn) 
       client.Connect()
       use cmd = client.CreateCommand(cmd)
       let outp = cmd.Execute()
       client.Disconnect()
       let ret = (host, outp.TrimEnd(crlf), cmd.Error.TrimEnd(crlf), cmd.ExitStatus)
       if cmd.ExitStatus <> 0 && not ignore_errors then raise(SshFailure ret)
       ret

   log.Write(sprintf "sshing to %s\n" host)
   let remote_dir = "/tmp/sshtmp_" + string Thread.CurrentThread.ManagedThreadId
   try
       try // create upload dir
           run_command ("mkdir " + remote_dir) false |> ignore
           // upload
           use sftp = new SftpClient(conn)
           sftp.Connect()
           sftp.ChangeDirectory(remote_dir)
           script :: files |> Seq.iter (fun file -> log.Write(sprintf "transferring %s to %s\n" file host)
                                                    use str = File.OpenRead(file)                                                    
                                                    sftp.UploadFile(str, Path.GetFileName(file), true))
           sftp.Disconnect()
           // execute script
           let rem_script = remote_dir + "/" + Path.GetFileName(script)
           run_command ("chmod u+x " + rem_script) false |> ignore
           run_command rem_script false
       finally
           run_command ("rm " + remote_dir + "/*") true |> ignore
           run_command ("rmdir " + remote_dir) true |> ignore
   with | SshFailure(h,a,b,c) -> (h,a,b,c)
        | _ as ex -> (host, ex.Message, String.Empty, 1)

// the format of the hosts file is <hostname>, <port>, <userid>, <passwd>
// separated by whitespace
let read_hosts hfile =
    File.ReadAllLines(hfile)
    |> Seq.map (fun line -> let flds = Regex.Split(line, "\s+")
                            try
                               Some (flds.[0], int flds.[1], flds.[2], flds.[3])
                            with _ -> None)
    |> Seq.choose id

[<EntryPoint>]
let main argv =
    let stopw = System.Diagnostics.Stopwatch.StartNew()
    // Intended usage: program <host_file> <script> [file1 file2 ...]
    // copy file1, file2... to these hosts, execute the script, then delete files and script
    // and return (host, stdout, stderr, exit code) for each host
    let log = SerialLogging()
    let harr = read_hosts argv.[0]
    let script = argv.[1]
    let files = argv.[2..] |> Array.toList
    harr
    |> Seq.map (fun (host, port, user, pw) -> async {return execute_command log host port user pw script files })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Seq.iter (fun (host, out, err, rc) -> 
                log.Write(sprintf "host='%s', stdout='%s', stderr='%s', rc=%d\n" host out err rc))
    log.Exit () |> ignore  // wait for mailbox output to complete
    stopw.Stop()
    printfn "%.2f seconds elapsed" (stopw.Elapsed.TotalMilliseconds / 1000.0)
    0