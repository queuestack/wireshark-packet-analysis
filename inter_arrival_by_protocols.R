library('tidyverse')
setwd("/Users/Q/GoogleDrive/2018-2/ComputerNetworks/homework/homework1/browser_data")
theme_update(text=element_text(family="NanumGothic"))
options(pillar.sigfig = 8)

NUM_CAPTURES = 60

multi_interval_times = list()

# 1회차
interval_times_by_protocol = list()
path = str_c(1, ".csv")
capture = read_csv(path)
packets = capture %>% group_by(Protocol) %>% select(Time, Protocol)
protocols = c(packets %>% count() %>% select(Protocol))$Protocol

for (i in (1:length(protocols))) {
  # 해당 프로토콜에 해당하는 패킷들을 선택한다
  protocol = protocols[i]
  arrival_times = packets %>% filter(Protocol == as.name(protocol)) %>% ungroup() %>% select(Time)
  arrival_times = c(arrival_times)$Time
  interval_times = c()
  
  # 프로토콜이 도착한 시간들 사이의 간격을 계산한다
  for (j in (1:length(arrival_times) - 1)) {
    interval_time = arrival_times[j + 1] - arrival_times[j]
    interval_times = c(interval_times, interval_time)
  }
  
  interval_times_by_protocol[[as.name(protocol)]] = interval_times
}
multi_interval_times[[1]] = interval_times_by_protocol

for (k in 2:NUM_CAPTURES) {
  path = str_c(k, ".csv")
  capture = read_csv(path)
  packets = capture %>% group_by(Protocol) %>% select(Time, Protocol)
  
  #protocols = c(packets %>% count() %>% select(Protocol))$Protocol
  protocols = c(
    "ALLJOYN-NS",
    "ARP",
    "DHCP",
    "DNS",
    "ICMPv6",
    "IGMPv2",
    "MDNS",
    "TCP",
    "TLSv1.2",
    "SSDP",
    "TLSv1",
    "UDP",
    "BROWSER",
    "HTTP",
    "NBNS",
    "TLSv1.3",
    "NTP"
  )

  interval_times_by_protocol = list()
  for (i in (1:length(protocols))) {
    # 해당 프로토콜에 해당하는 패킷들을 선택한다
    protocol = protocols[i]
    arrival_times = packets %>% filter(Protocol == as.name(protocol)) %>% ungroup() %>% select(Time)
    arrival_times = c(arrival_times)$Time
    interval_times = c()
    
    # 프로토콜이 도착한 시간들 사이의 간격을 계산한다
    for (j in (1:length(arrival_times) - 1)) {
      interval_time = arrival_times[j + 1] - arrival_times[j]
      interval_times = c(interval_times, interval_time)
    }
    
    
    interval_times_by_protocol[[as.name(protocol)]] = interval_times
  } 
  multi_interval_times[[k]] = interval_times_by_protocol
  
}
# 하나로 합치기

TCP= c()
for (i in 1:NUM_CAPTURES) {
  TCP = c(TCP, multi_interval_times[[i]]["TCP"])
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
protocol_interval_times = tibble(
  `ALLJOYN-NS` = 0,
  ARP = 0,
  DHCP = 0,
  DNS = 0,
  ICMPv6 = 0,
  IGMPv2 = 0,
  MDNS = 0,
  TCP = 0,
  TLSv1.2 = 0,
  SSDP = 0,
  TLSv1 = 0,
  UDP = 0,
  BROWSER = 0,
  HTTP = 0,
  NBNS = 0,
  TLSv1.3 = 0,
  NTP = 0
)