# CentOS security log parser

Command line utility could be used for parsing standard security logs in CentOS, resided in `/var/log/secure*`.

Utility obtain unique IP-addreses from logs and show how many attempts there were detected for each address.


## Usage
Just run it on the log file as follows:

```
./secuparse -f logfile
```

and you will get table with results in the descending order:

```
+-------------------+------------+
|         IP        |  Attempts  |
+-------------------+------------+
|    23.96.234.243  |    1361    |
|    202.171.33.35  |    1001    |
|    202.194.81.24  |     286    |
|   159.203.69.155  |     236    |
|   91.197.232.103  |     198    |
|   104.198.39.141  |     194    |
|     185.13.38.99  |     169    |
|   183.129.255.34  |     160    |
|     31.207.47.36  |     152    |
|      192.99.59.3  |     143    |
+-------------------+------------+
```
