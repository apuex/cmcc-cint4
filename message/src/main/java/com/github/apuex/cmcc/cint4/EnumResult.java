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

    public int getValue() {
        return this.value;
    }

    public static EnumResult fromValue(int v) {
        switch(v) {
        case 0: return FAILURE;
        case 1: return SUCCESS;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
