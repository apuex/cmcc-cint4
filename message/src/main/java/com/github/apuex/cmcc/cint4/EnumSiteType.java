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

    public int getValue() {
        return this.value;
    }

    public static EnumSiteType fromValue(int v) {
        switch(v) {
        case 1: return DATACENTER;
        case 2: return ROOM;
        case 3: return LOCALTRANS;
        case 4: return STATION;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
