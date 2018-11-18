open System.Net
open System.Net.Sockets
open System.Net.NetworkInformation

// obtain the local addr to be used to reach dst
let route_lookup (dst: byte[]) =
    let fam = if Array.length dst = 4 then AddressFamily.InterNetwork else AddressFamily.InterNetworkV6
    use s = new Socket(fam, SocketType.Dgram, ProtocolType.Udp)
    let addr = IPEndPoint(IPAddress dst, 1) // port # is irrelevant
    s.Connect addr
    let ep = s.LocalEndPoint :?> IPEndPoint
    IPAddress(ep.Address.GetAddressBytes())

let _ =
    try
        let host = fsi.CommandLineArgs.[1]
        let dst = try IPAddress.Parse host
                  with _ -> Dns.GetHostEntry(host).AddressList |> Seq.head
        let local = route_lookup (dst.GetAddressBytes())
        let adapter = NetworkInterface.GetAllNetworkInterfaces()
                      |> Seq.filter (fun ad ->
                                     ad.GetIPProperties().UnicastAddresses
                                     |> Seq.exists (fun uni -> uni.Address = local))
                      |> Seq.head
        printfn "to reach dst %A packets would leave by adapter %A with src ip %A" dst adapter.Name local
    with _ as e -> printfn "error: %s" e.Message
