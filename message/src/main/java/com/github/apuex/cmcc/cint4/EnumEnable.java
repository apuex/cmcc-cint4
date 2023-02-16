package com.github.apuex.cmcc.cint4;

/**
 * 使能的属性
 */
public enum EnumEnable {
    DISABLE(0) //禁止/不能
    , ENABLE(1) //开放/能
    ;

    EnumEnable(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
