package com.github.apuex.cmcc.cint4;

/**
 * 下级实时数据访问的方式
 */
public enum EnumAccessMode {
    ASK_ANSWER(0) //一问一答方式
    , CHANGE_TRIGGER(1) //改变时自动发送数据方式
    , TIME_TRIGGER(2) //定时发送数据方式
    , STOP(3) //停止发送数据方式
    ;

    EnumAccessMode(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumAccessMode fromValue(int v) {
        switch(v) {
        case 0: return ASK_ANSWER;
        case 1: return CHANGE_TRIGGER;
        case 2: return TIME_TRIGGER;
        case 3: return STOP;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
