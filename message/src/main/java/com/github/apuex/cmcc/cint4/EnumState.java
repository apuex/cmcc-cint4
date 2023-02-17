package com.github.apuex.cmcc.cint4;

/**
 * 数据值的状态
 */
public enum EnumState {
    NOALARM(0) //正常数据
    , CRITICAL(1) //一级告警
    , MAJOR(2) //二级告警
    , MINOR(3) //三级告警
    , HINT(4) //四级告警
    , OPEVENT(5) //操作事件
    , INVALID(6) //无效数据
    ;

    EnumState(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumState fromValue(int v) {
        switch(v) {
        case 0: return NOALARM;
        case 1: return CRITICAL;
        case 2: return MAJOR;
        case 3: return MINOR;
        case 4: return HINT;
        case 5: return OPEVENT;
        case 6: return INVALID;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
