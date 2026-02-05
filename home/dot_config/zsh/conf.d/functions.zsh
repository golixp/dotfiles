#!/usr/bin/env zsh

### 用户函数 ###

# 代理开启
proxy_on() {
    # proxy_on 支持默认参数
    # proxy_on 127.0.0.1 7890
    local PROXY_HOST="${1:-127.0.0.1}"
    local PROXY_PORT="${2:-7897}"

    export http_proxy="http://${PROXY_HOST}:${PROXY_PORT}"
    export https_proxy="http://${PROXY_HOST}:${PROXY_PORT}"
    export all_proxy="socks5://${PROXY_HOST}:${PROXY_PORT}"
    export no_proxy="localhost,127.0.0.1,::1,10.0.0.0/8,192.168.0.0/16"

    echo "Proxy ON → ${PROXY_HOST}:${PROXY_PORT}"
}
# 代理关闭
proxy_off() {
    unset http_proxy https_proxy all_proxy no_proxy
}
