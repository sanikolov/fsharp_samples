I was missing the very useful "/sbin/ip route get" linux command on windows
and decided to write this short script as replacement. No packets are actually sent.

Examples:

$ fsharpi ip_route_get.fsx ipv6.google.com

to reach dst 2607:f8b0:4006:80f::200e packets would leave by adapter "teredo" with src ip 2001:0:53aa:68c:30db:27a7:e7d9:3a6b

C:\bin>fsi ip_route_get.fsx 2607:f8b0:4006:806::200e

to reach dst 2607:f8b0:4006:806::200e packets would leave by adapter "Teredo Tunneling Pseudo-Interface" with src ip 2001:0:9de8:62bd:a021:21d8:e7d9:256b

C:\bin>fsi ip_route_get.fsx 192.168.23.9

to reach dst 192.168.23.9 packets would leave by adapter "PPP VPN1" with src ip 192.168.8.12
