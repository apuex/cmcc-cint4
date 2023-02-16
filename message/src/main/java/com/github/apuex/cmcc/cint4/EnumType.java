package com.github.apuex.cmcc.cint4;

/**
 * 监控系统数据的种类
 */
public enum EnumType {
    ALARM(0) //告警
    , DO(1) //数字输出量，遥控
    , AO(2) //模拟输出量，遥调
    , AI(3) //模拟输入量，遥测
    , DI(4) //数字输入量（包含多态数字输入量），遥信
    , DEVICE(5) //设备
    , ROOM(6) //机房
    , SITE(7) //站点
    , AREA(8) //区域
    ;

    EnumType(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
