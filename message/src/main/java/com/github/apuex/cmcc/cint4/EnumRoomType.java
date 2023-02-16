package com.github.apuex.cmcc.cint4;

/**
 * 机房类型
 */
public enum EnumRoomType {
    CONVERGE(1) //汇聚机房
    , BASESTATION(2) //基站机房
    , GENERATION(11) //发电机房
    , ELECTRIC(12) //电力机房
    , BATTERY(13) //电池机房
    , AIRCONDITION(14) //空调机房
    , TRANSFERS(51) //传输机房
    , EXCHANGE(52) //交换机房
    , DATA(53) //数据机房
    , IDC(54) //IDC机房
    , COLLIGATION(54) //综合机房
    ;

    EnumRoomType(int v) {
        this.value = v;
    }

    int getValue() {
        return this.value;
    }
 
    private final int value;
}
