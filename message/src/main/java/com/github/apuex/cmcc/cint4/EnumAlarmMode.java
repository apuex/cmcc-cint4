package com.github.apuex.cmcc.cint4;

/**
 * 告警等级设定的模式
 */
public enum EnumAlarmMode {
    NOALARM(0) //不做告警上报
    , CRITICAL(1) //一级告警上报
    , MAJOR(2) //二级告警上报
    , MINOR(3) //三级告警上报
    , HINT(4) //四级告警上报
    ;

    EnumAlarmMode(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
