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

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
