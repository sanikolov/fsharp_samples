The scanner is implemented in F# and requires mono on linux systems.
To install it on a debian like distribution type:

**$ sudo apt-get install fsharp**

Either run the scanner as root or grant the mono-sgen executable the capability
to open raw sockets:

**$ sudo setcap cap_net_raw+ep /usr/bin/mono-sgen**

To compile the executable bytecode from sources type:

**$  xbuild pscanner.fsproj**

in the same directory where pscanner.fsproj is located.
The bytecode executable port_scanner.exe will be generated in bin/Debug by default

Examples:

1) UDP

$ ./port_scanner.exe udp 192.168.1.106 1 65535

this will take 65535*1.003 + 5 seconds to complete, about 18.26 hours
where 1003 ms is the pause between datagrams.

2) TCP

$ ./port_scanner.exe tcp 192.168.1.106 1 65535

this should take about 65535*0.002 + 5 = approx. 136 seconds to complete
where 2ms is the pause between outgoing SYNs.
