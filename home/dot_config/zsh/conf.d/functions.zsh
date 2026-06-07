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
    export all_proxy="http://${PROXY_HOST}:${PROXY_PORT}"
    export no_proxy="localhost,127.0.0.1,::1,10.0.0.0/8,192.168.0.0/16"
    export HTTP_PROXY="$http_proxy"
    export HTTPS_PROXY="$https_proxy"
    export ALL_PROXY="$all_proxy"
    export NO_PROXY="$no_proxy"

    local -a node_options
    node_options=(${=NODE_OPTIONS:-})
    if (( ${node_options[(Ie)--use-env-proxy]} == 0 )); then
        node_options+=(--use-env-proxy)
    fi
    export NODE_OPTIONS="${(j: :)node_options}"

    echo "Proxy ON → ${PROXY_HOST}:${PROXY_PORT}"
}
# 代理关闭
proxy_off() {
    unset http_proxy https_proxy all_proxy no_proxy
    unset HTTP_PROXY HTTPS_PROXY ALL_PROXY NO_PROXY

    local -a node_options
    node_options=(${=NODE_OPTIONS:-})
    node_options=(${node_options:#--use-env-proxy})
    if (( ${#node_options} > 0 )); then
        export NODE_OPTIONS="${(j: :)node_options}"
    else
        unset NODE_OPTIONS
    fi
}
