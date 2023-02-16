package com.github.apuex.cmcc.cint4;

/**
 * 局站类型
 */
public enum EnumSiteType {
    DATACENTER(1) //数据中心
    , ROOM(2) //通信机楼
    , LOCALTRANS(3) //传输节点
    , STATION(4) //通信基站
    ;

    EnumSiteType(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
