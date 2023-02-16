package com.github.apuex.cmcc.cint4;

/**
 * 资源工程状态
 */
public enum EnumNeStatusType {
    NeStatusType1(1) //现网有业务承载
    , NeStatusType2(2) //现网无业务承载
    , NeStatusType3(3) //退网
    , NeStatusType4(4) //工程
    , NeStatusType5(5) //删除
    , NeStatusType6(6) //未确认
    ;

    EnumNeStatusType(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
