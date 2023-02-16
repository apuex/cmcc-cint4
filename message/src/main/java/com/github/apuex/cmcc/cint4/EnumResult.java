package com.github.apuex.cmcc.cint4;

/**
 * 报文返回结果
 */
public enum EnumResult {
    FAILURE(0) //失败
    , SUCCESS(1) //成功
    ;

    EnumResult(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
