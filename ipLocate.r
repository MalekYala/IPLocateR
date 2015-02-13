telize <- function(ip, format = ifelse(length(ip)==1,'list','dataframe'))
{
    if (1 == length(ip))
    {
        # a single IP address
        require(rjson)
        url <- paste(c("http://telize.com/geoip/", ip), collapse='')
        ret <- fromJSON(readLines(url, warn=FALSE))
        if (format == 'dataframe')
            ret <- data.frame(t(unlist(ret)))
        print(ret)
	fi <- write.table(ret, file="/home/nemo/amoworldwide/data.csv", sep="\t", col.names=FALSE, append=TRUE)
	return(ret)
    } else {
        ret <- data.frame()
        for (i in 1:length(ip))
        {
            r <- telize(ip[i], format="dataframe")
            ret <- rbind(ret, r)
        }
	print(ret)
	fi <- write.table(ret, file="/home/nemo/amoworldwide/data.csv", sep="\t", col.names=FALSE, append=TRUE)	
        return(ret)
    }
}

#error handlers if ip cannot be identified
try.ip <- function(ip) suppressWarnings(try(telize(ip), silent = TRUE))
is.ok <- function(x) !inherits(x, "try-error")

#read the input file
file <- read.csv(file="users.csv", header=TRUE, sep="\t")
#identify the ip row
rawips <- file$ip
#pull the ip strigs in a xxx.xxx.xxx format
iplist <- sub("([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+).*","\\1.\\2.\\3.\\4",rawips)
#display the ips for the user
print(iplist)
#execute
out <- lapply(c(iplist), try.ip)

