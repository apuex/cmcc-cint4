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
 * 数据更新类型
 *
 * @author Wangxy
 */
public enum EnumUpdateType {
    StoragePeriod(0) //存储周期
    , Absolute(1) //绝对阀值
    , Relative(2) //相对阀值
    , Static(3) //统计标志
    ;

    EnumUpdateType(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumUpdateType fromValue(int v) {
        switch(v) {
        case 0: return StoragePeriod;
        case 1: return Absolute;
        case 2: return Relative;
        case 3: return Static;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
