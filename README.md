This package contains persistent instances and utility functions 
for [iproute](https://github.com/kazu-yamamoto/iproute) types IP and
IPRange, mapping them to INET and CIDR PostgreSQL types respectively. It
was tested with PostgreSQL versions 9.3 and 9.4, and is expected to work
with other versions supporting INET and CIDR types. Compatibility with
any other SQL database isn't guaranteed.
