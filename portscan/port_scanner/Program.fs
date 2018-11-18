open System
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open System.Diagnostics
open System.Text.RegularExpressions
open System.IO

type Bytes = byte [] * int  // sub array of specified length
type Pkt = TCP of Bytes | ICMP of Bytes
type Opaque = Socket * int

let LOCAL_PORT = 13398us
let INITIAL_SEQ_NUM = 0x12345678
let UDP_HDR_SIZE = 8
let ICMP_HDR_SIZE = 8
let MIN_TCPHDR_SIZE = 20
let MIN_IPHDR_SIZE = 20
let endp = IPEndPoint(1L,1)  // satisfies API requirements, not useful otherwise

type Socket with
    member this.ReadAsync(buf: byte[], offset: int, size: int) =
        let error_code = ref SocketError.Success
        let end_action p =
            //printfn "thread ID %d" Thread.CurrentThread.ManagedThreadId
            this.EndReceive(p)
        let begin_action (callback, state) = this.BeginReceive(buf, offset, size, SocketFlags.None, 
                                                               error_code, callback, state)
        Async.FromBeginEnd(begin_action, end_action)
 
    member this.WriteAsync(buf: byte[], offset: int, size: int) =
        let begin_action (callback, state) = this.BeginSendTo(buf, offset, size, SocketFlags.None, endp, callback, state)
        Async.FromBeginEnd(begin_action, this.EndSendTo)

let str s = (string s).ToLower()
// convert 2 or 4 byte arrays from their native representation to a big endian
let to_bigendian (a: byte[]) = a |> if BitConverter.IsLittleEndian then Array.rev else id // pure
let cp_fields cnt offset (da: byte[]) (sa: byte[]) = // dst array <- src array
    [|0..cnt-1|] |> Seq.iter (fun idx -> da.[offset+idx] <- sa.[idx])
let arr4 (x: int) = BitConverter.GetBytes x     // pure
let arr2 (x: uint16) = BitConverter.GetBytes x  // pure
let int2 (a: byte[]) off = BitConverter.ToInt16(a, off) // pure, 2 byte array as param, net byte order result
let int4 (a: byte[]) off = BitConverter.ToInt32(a, off) // pure, 4 byte array as param, net byte order result

// apply filter and return Some tuple if it matches
// anything else including failures result in None
let process_packet tcp_filter icmp_filter (buf: byte[]) len =
    if len < MIN_IPHDR_SIZE then None
    else begin
        let ipv = buf.[0]
        let ip_version = ipv >>> 4
        let iplen = ipv &&& 0xfuy
        if ip_version <> 4uy || iplen < 5uy then None
        else begin
            let proto = buf.[9]
            let src_ip = int4 buf 12
            let dst_ip = int4 buf 16
            let hlen = (int iplen) * 4
            match proto with
            | 6uy -> // TCP
                if len < hlen + MIN_TCPHDR_SIZE then None
                else begin
                   let sport = int2 buf hlen |> IPAddress.NetworkToHostOrder |> uint16
                   let dport = int2 buf (hlen+2) |> IPAddress.NetworkToHostOrder |> uint16
                   let seq = int4 buf (hlen+4)
                   let ack = int4 buf (hlen+8)
                   let flags = buf.[hlen+13] &&& 0xffuy // SYN|ACK
                   if tcp_filter flags (IPAddress.NetworkToHostOrder ack) dport then
                       Some (proto, src_ip, dst_ip, sport, dport)
                   else
                       None
                end
            | 1uy -> // ICMP, look for port unreachable msgs here
                if len < hlen + ICMP_HDR_SIZE + MIN_IPHDR_SIZE + UDP_HDR_SIZE then None // icmp hdr + min ip hdr + min udp hdr
                else begin
                   let icmp_type = buf.[hlen]
                   let icmp_code = buf.[hlen+1]
                   let ipoff = hlen + ICMP_HDR_SIZE   // offset to inner ip header in err msg
                   let ipv = buf.[ipoff]
                   let ip_version = ipv >>> 4
                   let iplen = ipv &&& 0xfuy
                   // inner header validation
                   if ip_version <> 4uy || iplen < 5uy || len < hlen + ICMP_HDR_SIZE + 4*(int iplen) + UDP_HDR_SIZE then
                       None
                   else begin
                       let inner_proto = buf.[ipoff + 9]
                       let udp_off = ipoff + 4*(int iplen)
                       if icmp_type <> 3uy && icmp_code <> 3uy && inner_proto <> byte ProtocolType.Udp then None
                       else begin
                           let sport = int2 buf udp_off |> IPAddress.NetworkToHostOrder |> uint16
                           let dport = int2 buf (udp_off + 2) |> IPAddress.NetworkToHostOrder |> uint16
                           if icmp_filter sport then
                               Some (proto, src_ip, dst_ip, sport, dport)
                           else
                               None
                       end
                   end
                end
            | _ -> None
         end
    end       

let checksum (buf: byte[]) = // pure
    let len = Array.length buf
    let rec sum i acc =
        if i < len then (
            let a = ((int buf.[i]) <<< 8) &&& 0xff00
            let b = (int buf.[i+1]) &&& 0xff
            sum (i+2) (acc + a + b)
        ) else
           acc
    let rec strip_hi16 s =
        let hi16 = s >>> 16
        if hi16 = 0 then s else strip_hi16 (s &&& 0xffff) + hi16
    ~~~(strip_hi16 <| sum 0 0) |> uint16

// pure. {src_ip:uint32, dst_ip:uint32, zero:byte, proto:byte, len:uint16} 
let checksum_pseudo (data: byte[]) offset dst_ip src_ip proto =
    let pseudo_len = 4 + 4 + 1 + 1 + 2 // pseudo header length
    let buflen = Array.length data - offset // l4 part of header only
    let sumlen = pseudo_len + buflen
    let padded_len = if (sumlen &&& 0xf) <> 0 then 16 + (sumlen &&& ~~~0xf) else sumlen
    let buf = Array.create padded_len 0uy
    dst_ip |> cp_fields 4 0 buf
    src_ip |> cp_fields 4 4 buf
    buf.[9] <- proto
    buflen |> uint16 |> arr2 |> to_bigendian |> cp_fields 2 10 buf
    for idx in 0..buflen-1 do
        buf.[pseudo_len+idx] <- data.[offset+idx]
    done
    checksum buf

// sends packet synchronously because no benefits redound if we switch to async mode
// function can be better parameterized but given the current requirements there is no need to do
let send_packet (sock: Socket) proto (port: uint16) (src_ip: byte[]) (dst_ip: byte[]) =
    let is_tcp = proto = ProtocolType.Tcp
    let l4len = if is_tcp then MIN_TCPHDR_SIZE else UDP_HDR_SIZE
    let pktlen = MIN_IPHDR_SIZE + l4len  // ip hdr size + tcp, udp hdr size, empty payload
    let ip = Array.create pktlen 0uy
    ip.[0] <- 0x45uy // ipv4, 5*4 bytes is the hdr size, endianness does not matter in bytes
    pktlen |> uint16 |> arr2 |> to_bigendian |> cp_fields 2 2 ip // total len
    port |> arr2 |> to_bigendian |> cp_fields 2 4 ip  // ID field
    // "do not fragment" in 2nd least significant bit | with 0 offset, i.e. 0100000000000000t
    (1us <<< 14) ||| 0x0us |> arr2 |> to_bigendian |> cp_fields 2 6 ip
    ip.[8] <- 250uy  // ttl
    let proto_num = byte proto
    ip.[9] <- proto_num
    src_ip |> cp_fields 4 12 ip
    dst_ip |> cp_fields 4 16 ip
    // L4 header setup below
    LOCAL_PORT |> arr2 |> to_bigendian |> cp_fields 2 MIN_IPHDR_SIZE ip // fixed src port
    port |> arr2 |> to_bigendian |> cp_fields 2 (MIN_IPHDR_SIZE+2) ip   // dst port
    if is_tcp then begin
        INITIAL_SEQ_NUM |> arr4 |> to_bigendian |> cp_fields 4 (MIN_IPHDR_SIZE+4) ip // seq #, ack # ok as 0
        // data offset is 5, SYN bitfield is only one that is set
        (5us <<< 12) ||| 0x2us |> arr2 |> to_bigendian |> cp_fields 2 (MIN_IPHDR_SIZE+12) ip
        0x1000us |> arr2 |> to_bigendian |> cp_fields 2 (MIN_IPHDR_SIZE+14) ip // windows size 0x1000
        (* all other fields can be left blank, they are already 0 *)
        let tcp_sum = checksum_pseudo ip MIN_IPHDR_SIZE dst_ip src_ip proto_num
        tcp_sum |> arr2 |> to_bigendian |> cp_fields 2 (MIN_IPHDR_SIZE+16) ip
    end
    else begin // udp
        UDP_HDR_SIZE |> uint16 |> arr2 |> to_bigendian |> cp_fields 2 (MIN_IPHDR_SIZE+4) ip // hdr length
        let udp_sum = checksum_pseudo ip MIN_IPHDR_SIZE dst_ip src_ip proto_num
        udp_sum |> arr2 |> to_bigendian |> cp_fields 2 (MIN_IPHDR_SIZE+6) ip
    end
    checksum ip |> arr2 |> to_bigendian |> cp_fields 2 10 ip
    sock.SendTo(ip, endp)
    //sock.WriteAsync(ip, 0, pktlen)   
    // async version can overrun remote host's ring buffer easily unless throttled

let route_lookup (dst: byte[]) = // obtain the local addr to be used to reach dst
    use s = new Socket(AddressFamily.InterNetwork, SocketType.Dgram, ProtocolType.Udp)
    let addr = IPEndPoint(IPAddress dst, 1) // port # is irrelevant
    s.Connect addr
    let ep = s.LocalEndPoint :?> IPEndPoint
    ep.Address.GetAddressBytes()

let start_scan (sock_sends: Socket) (sock_recvs: Socket) protocol dst_host lb ub 
    (services: Map<(uint16*string),Set<string>>) test_mode =
    let range = [|lb..ub|]
    let is_tcp = protocol = ProtocolType.Tcp
    let buflen = sock_recvs.ReceiveBufferSize
    let buf = Array.create buflen 0uy
    let tcp_filter flags ack dport = // business logic separation from pkt decode
        flags = 0x12uy && dport = LOCAL_PORT && ack = INITIAL_SEQ_NUM+1
    let icmp_filter sport = sport = LOCAL_PORT
    let my_ip = route_lookup dst_host  // local ip addr to fill in raw sock msg
    use sem = new Semaphore(0,1)
    use cancel_token = new CancellationTokenSource()
    let result_set = ref Set.empty
    let rec receive_loop set = // there is no recursion termination here, other than cancellation
         // it's like an infinite loop except it can be re-entered on a different thread
        async { // this is an inherently async workflow, a powerful (i.e. light weight) construct
            result_set := set // save away persistent set in case workflow gets cancelled
            let! recvd = sock_recvs.ReadAsync(buf, 0, buflen) // thread released to pool if no data yet
            // control flow resumes here automagically when are data consumed, possibly on a different thread
            let reply = match process_packet tcp_filter icmp_filter buf recvd with
                        | None -> None
                        | Some (proto, src, dst, sport, dport) ->
                           Some (if is_tcp then sport else dport)
            return! receive_loop (if reply <> None then Set.add (Option.get reply) set else set)
        }

    let send_loop (throttle: int) proto src dst =
        range
        |> Array.iter (fun port -> Thread.Sleep throttle; send_packet sock_sends proto port src dst |> ignore)
        Thread.Sleep 5000    // wait 5 seconds for straggling replies to arrive, if any
        cancel_token.Cancel()  // indicate to receive_loop and all others that it is time to quit

    // 2 artificial async wrappers follow, neither is inherently async under the covers, thus there is
    // no hidden benefit. It is somewhat analogous to a "(future (computation ..)), (deref @fut)" in clojure
    let async_receive = async { // this async wrapper returns immediately
           sem.WaitOne() |> ignore
           let! ret = receive_loop Set.empty
           return ret
        }
         
    let async_send = async { // async wrapper returns immediately, not evaluated until later
           let msec = if is_tcp then 5 else 1003 // throttle sends
           sem.Release() |> ignore  // unblock receive loop
           do! Async.Sleep 500  // give receive_loop a head start. Release this thread to thr pool until then
           send_loop msec protocol my_ip dst_host
           return Set.empty // unused
        }
    let result = try
                     [|async_receive; async_send|] // launch these 2 in parallel
                     |> Async.Parallel   // on the next line this thread blocks awaiting completion of other 2
                     |> (fun comp -> Async.RunSynchronously(comp, -1, cancel_token.Token)) // infinite wait due to -1
                 // cancellation initiated in send_loop will result in exception below
                 with :? System.OperationCanceledException -> [|!result_set; Set.empty|] // snapshot of persistent set
    let returned_ports = result.[0]
    let live_ports = (if is_tcp then returned_ports else Set.difference (Set.ofSeq range) returned_ports) |> Set.toArray
    let proto_str = str protocol
    let svc_names = live_ports |> Array.map (fun port -> match Map.tryFind (port, proto_str) services with
                                                         | None -> "unknown"
                                                         | Some svc -> String.Join("|", Set.toArray svc)
                                            )
    let zipped = Array.zip live_ports svc_names
    let qualifier = if is_tcp then "" else "possibly "
    let joined = String.Join("; ", zipped)
    if test_mode then
        printfn "%s %s" proto_str (String.Join(" ", Array.sort live_ports |> Array.map string))
    else
        printfn "%slive %s ports: %s" qualifier proto_str (if joined.Length > 0 then joined else "none found")

    zipped, joined

// returns a map of [port, proto] to a set of service names
let port_to_service_map file =
    try // 1 off regex use below at init time, no need to precompile for repeated matching
        let re_wsp = Regex(@"^\s*$")
        let re_comment = Regex(@"^\s*#.*$")
        let re_col3 = Regex(@"^(\S+)\s+(\d+)/(\S+).*")
        File.ReadAllLines file
        |> Seq.fold (fun map line ->
                     let m1 = re_wsp.Match(line)
                     let m2 = re_comment.Match(line)
                     if m1.Success || m2.Success then map   // skip over irrelevant lines
                     else begin
                         let m3 = re_col3.Match(line)
                         if m3.Success then
                             let g = m3.Groups
                             let (svc, port, proto) = (g.[1].Value, uint16 g.[2].Value, g.[3].Value.ToLower())
                             let key = (port, proto)
                             let value = svc
                             let set = match Map.tryFind key map with
                                       | None -> Set.empty
                                       | Some prev -> prev
                             if proto = "tcp" || proto = "udp" then Map.add key (Set.add value set) map else map
                         else
                             map
                     end
                    ) Map.empty
     with | _ as e -> printfn "Error while folding services: %s" e.Message
                      Map.empty

[<EntryPoint>]
let main argv =
    let prog = AppDomain.CurrentDomain.FriendlyName
    try
        // validation follows, on invalid input reasonable defaults may be chosen
        if Array.length argv < 4 then (
            printfn "Usage: %s [tcp|udp] host port_from port_to <-test-mode>" prog
            printfn "Example: ./%s tcp 192.168.1.1 1 65535" prog
            Environment.Exit 1
        )
        let proto = if argv.[0] = "udp" then ProtocolType.Udp else ProtocolType.Tcp
        let flt (a: IPAddress) =  a.AddressFamily = AddressFamily.InterNetwork
        let (errmsg, v4addr) = try "", Some (Dns.GetHostEntry(argv.[1]).AddressList |> Array.filter flt |> Seq.head)
                               with | :? ArgumentException  | :? SocketException as e -> e.Message, None
        if v4addr = None then
            printfn "%s: unable to determine ipv4 address of %s [%s]" prog argv.[1] errmsg
            Environment.Exit 2

        let host = Option.get v4addr
        let addr = host.GetAddressBytes()
        let (lb, err1) = try uint16 argv.[2], "" with | _ as e -> 1us, e.Message + " " + argv.[2]      // default to 1
        let (ub, err2) = try uint16 argv.[3], "" with | _ as e -> 65535us, e.Message + " " + argv.[3]  // default to max ushort
        let err3 = if lb <= ub then "" else "lower bound cannot be higher than upper bound"
        if err1.Length > 0 || err2.Length > 0 || err3.Length > 0 then
            printfn "port range is incorrect: %s %s %s" err1 err2 err3
            Environment.Exit 3
        
        let test_mode = Array.length argv = 5 && argv.[4] = "-test-mode"
        let services = port_to_service_map "/etc/services" // (port, proto) -> set of svc names for that port
        let open_raw () =
            let fam = AddressFamily.InterNetwork
            let raw = SocketType.Raw
            if proto = ProtocolType.Tcp then (
                let tcp = new Socket(fam, raw, ProtocolType.Tcp)    // sends and receives
                tcp, tcp
            ) else (
                let udp = new Socket(fam, raw, ProtocolType.Udp)    // sends only
                let icmp = new Socket(fam, raw, ProtocolType.Icmp)  // recv port unreachable msgs for udp only
                udp, icmp // could be enhanced in future to: udp for sends, udp and icmp for receives
            )

        // sockets to perform sends and receives
        let (sock_sends, sock_recvs) = open_raw ()   // raw sockets to perform sends and receives
        sock_sends.SetSocketOption(SocketOptionLevel.IP, SocketOptionName.HeaderIncluded, true)
        if not test_mode then
            printfn "%s: started a %s port scan of %A in the port range %d .. %d" prog (str proto) host lb ub

        start_scan sock_sends sock_recvs proto addr lb ub services test_mode |> ignore

        // Dispose() not needed strictly speaking because of delayed finalization on finalizer GC queue
        // but it's good practice anyway if this function is entered thousands of times (here only once)
        [sock_sends; sock_recvs] |> Seq.iter (fun sock -> sock.Dispose())
        0  // success
    with _ as e -> printfn "%s: abnormal termination with error %s" prog e.Message
                   4  // error code