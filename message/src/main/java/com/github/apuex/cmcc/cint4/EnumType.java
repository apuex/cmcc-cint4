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
 * 监控系统数据的种类
 *
 * @author Wangxy
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

    public int getValue() {
        return this.value;
    }

    public static EnumType fromValue(int v) {
        switch(v) {
        case 0: return ALARM;
        case 1: return DO;
        case 2: return AO;
        case 3: return AI;
        case 4: return DI;
        case 5: return DEVICE;
        case 6: return ROOM;
        case 7: return SITE;
        case 8: return AREA;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
