package com.github.apuex.cmcc.cint4;

/**
 * 数据更新类型
 */
public enum EnumUpdateType {
    StoragePeriod(0) //存储周期
    , Absolute(1) //绝对阀值
    , Relative(2) //相对阀值
    , Static(3) //统计标志
    ;

    EnumUpdateType(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
