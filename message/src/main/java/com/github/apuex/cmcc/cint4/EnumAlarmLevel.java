package com.github.apuex.cmcc.cint4;

/**
 * 告警的等级
 */
public enum EnumAlarmLevel {
    NOALARM(0) //无告警
    , CRITICAL(1) //一级告警
    , MAJOR(2) //二级告警
    , MINOR(3) //三级告警
    , HINT(4) //四级告警
    ;

    EnumAlarmLevel(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
