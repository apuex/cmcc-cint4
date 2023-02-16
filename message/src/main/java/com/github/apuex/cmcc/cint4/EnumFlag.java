package com.github.apuex.cmcc.cint4;

/**
 * 告警标志
 */
public enum EnumFlag {
    BEGIN(0) //开始
    , END(1) //结束
    ;

    EnumFlag(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
