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

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
