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
 * 资源工程状态
 *
 * @author Wangxy
 */
public enum EnumNeStatusType {
    NeStatusType1(1) //现网有业务承载
    , NeStatusType2(2) //现网无业务承载
    , NeStatusType3(3) //退网
    , NeStatusType4(4) //工程
    , NeStatusType5(5) //删除
    , NeStatusType6(6) //未确认
    ;

    EnumNeStatusType(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumNeStatusType fromValue(int v) {
        switch(v) {
        case 1: return NeStatusType1;
        case 2: return NeStatusType2;
        case 3: return NeStatusType3;
        case 4: return NeStatusType4;
        case 5: return NeStatusType5;
        case 6: return NeStatusType6;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
