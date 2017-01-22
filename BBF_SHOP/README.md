# BBF

## 一些业务上的约定

- 会员发展部(组)
- 会员增值部(组)
- 售后服务部(组)

小能数据主要在会员发展部
呼入数据主要在会员发展部
外呼数据主要在会员增值部

## 部署

### window系统上直接运行

`Rscript -e "shiny::runApp('e:/dev/bbf', host='0.0.0.0', port=80)"`

### linux系统上安装shiny server进行部署

文件夹权限问题导致无法打开项目: `chmod -R 777 BBF/`
centos克隆github私有项目: `git clone https://funnng@github.com/funnng/BBF.git`

### RStudio

`sudo yum install --nogpgcheck xxx.rpm`
`sudo yum remove rstudio-server`

### shiny-server

`status shiny-server`
`sudo restart shiny-server`
`sudo stop shiny-server`
`sudo start shiny-server`
`sudo reload shiny-server`

## 报表及代码风格约定

### 报表Toolbar

```
xxxUI <- function(id){
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
            column(2, ...),
            column(2, ...),
            column(2, ...)
        ),
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
```

## 无法启动rstudio-server

`rstudio-server verify-installation`
21 Mar 2016 03:00:31 [rserver] ERROR system error 98 (Address already in use); OCCURRED AT: rstudio::core::Error rstudio::core::http::initTcpIpAcceptor(rstudio::core::http::SocketAcceptorService<boost::asio::ip::tcp>&, const std::string&, const std::string&) /root/rstudio/src/cpp/core/include/core/http/TcpIpSocketUtils.hpp:103; LOGGED FROM: int main(int, char* const*) /root/rstudio/src/cpp/server/ServerMain.cpp:438
rstudio-server start/running, process 10711

解决:

1) check the process that used 8787

`sudo fuser 8787/tcp`

2) with the -k option to kill all process.

`sudo fuser -k 8787/tcp`

3) Start RStudio Server

`sudo rstudio-server start`

