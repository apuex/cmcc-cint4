/*
 * Copyright (c) 2021-2023 WINCOM.
 * Copyright (c) 2021-2023 Wangxy <xtwxy@hotmail.com>
 *
 * All rights reserved. 
 *
 * This program and the accompanying materials
 * are made available under the terms of the Mozilla Public License 2.0.
 *
 * Contributors:
 *   Wangxy - initial implementation and documentation.
*/

package com.github.apuex.cmcc.cint4;

/**
 * 机房类型
 *
 * @author Wangxy
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
    , COLLIGATION(55) //综合机房
    ;

    EnumRoomType(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumRoomType fromValue(int v) {
        switch(v) {
        case 1: return CONVERGE;
        case 2: return BASESTATION;
        case 11: return GENERATION;
        case 12: return ELECTRIC;
        case 13: return BATTERY;
        case 14: return AIRCONDITION;
        case 51: return TRANSFERS;
        case 52: return EXCHANGE;
        case 53: return DATA;
        case 54: return IDC;
        case 55: return COLLIGATION;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
