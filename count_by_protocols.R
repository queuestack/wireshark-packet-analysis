library('tidyverse')
setwd("/Users/Q/GoogleDrive/2018-2/ComputerNetworks/homework/homework1/browser_data")
theme_update(text=element_text(family="NanumGothic"))


path = str_c(1, ".csv")
capture = read_csv(path)
packets_num_by_protocols = capture %>% group_by(Protocol) %>% count()
names(packets_num_by_protocols)[length(packets_num_by_protocols)] = 1
packets_num_by_protocols
spread(packets_num_by_protocols, Protocol, -Protocol)

for (i in 2:60) {
  path = str_c(i, ".csv")
  capture = read_csv(path)
  packet_num_by_protocols = capture %>% group_by(Protocol) %>% count()
  names(packet_num_by_protocols)[length(packet_num_by_protocols)] = i
  packets_num_by_protocols = full_join(packets_num_by_protocols, packet_num_by_protocols, by="Protocol")
}

# Transpose
packets_num_by_protocols = as_tibble(cbind(nms = names(packets_num_by_protocols), t(packets_num_by_protocols)))
colnames(packets_num_by_protocols) = packets_num_by_protocols[1, ] # the first row will be the header
packets_num_by_protocols = packets_num_by_protocols[-1, ]       

# As numeric
packets_num_by_protocols = packets_num_by_protocols %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .))) %>% 
  mutate_all(funs(as.numeric(.)))

ALLJOYN_NS = packets_num_by_protocols %>% group_by(`ALLJOYN-NS`) %>% count()
ARP = packets_num_by_protocols %>% group_by(ARP) %>% count()
DHCP = packets_num_by_protocols %>% group_by(DHCP) %>% count()
DNS = packets_num_by_protocols %>% group_by(DNS) %>% count()
ICMPv6 = packets_num_by_protocols %>% group_by(ICMPv6) %>% count()
IGMPv2 = packets_num_by_protocols %>% group_by(IGMPv2) %>% count()
MDNS = packets_num_by_protocols %>% group_by(MDNS) %>% count()
TCP = packets_num_by_protocols %>% group_by(TCP) %>% count()
TLSv1.2 = packets_num_by_protocols %>% group_by(TLSv1.2) %>% count()
SSDP = packets_num_by_protocols %>% group_by(SSDP) %>% count()
TLSv1 = packets_num_by_protocols %>% group_by(TLSv1) %>% count()
UDP = packets_num_by_protocols %>% group_by(UDP) %>% count()
BROWSER = packets_num_by_protocols %>% group_by(BROWSER) %>% count()
HTTP = packets_num_by_protocols %>% group_by(HTTP) %>% count()
NBNS = packets_num_by_protocols %>% group_by(NBNS) %>% count()
TLSv1.3 = packets_num_by_protocols %>% group_by(TLSv1.3) %>% count()
NTP = packets_num_by_protocols %>% group_by(NTP) %>% count()

# Draw Graph
ggplot(ALLJOYN_NS,aes(x=`ALLJOYN-NS`, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(ARP,aes(x=ARP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(DHCP,aes(x=DHCP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(DNS,aes(x=DNS, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(ICMPv6,aes(x=ICMPv6, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(IGMPv2,aes(x=IGMPv2, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(MDNS,aes(x=MDNS, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(TCP,aes(x=TCP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(TLSv1.2,aes(x=TLSv1.2, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(SSDP,aes(x=SSDP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(TLSv1,aes(x=TLSv1, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(UDP,aes(x=UDP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(BROWSER,aes(x=BROWSER, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(HTTP,aes(x=HTTP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(NBNS,aes(x=NBNS, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(TLSv1.3,aes(x=TLSv1.3, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")

ggplot(NTP,aes(x=NTP, y=n)) +
  geom_bar(stat="identity", width = 0.5) + 
  labs(y="Number of packets")


# Test
