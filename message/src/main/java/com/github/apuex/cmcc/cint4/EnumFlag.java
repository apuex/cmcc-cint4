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
 * 告警标志
 *
 * @author Wangxy
 */
public enum EnumFlag {
    BEGIN(0) //开始
    , END(1) //结束
    ;

    EnumFlag(int v) {
        this.value = v;
    }

    public int getValue() {
        return this.value;
    }

    public static EnumFlag fromValue(int v) {
        switch(v) {
        case 0: return BEGIN;
        case 1: return END;
        default: throw new IllegalArgumentException(String.format("%d is an invalid enum value.", v));
        }
    }
 
    private final int value;
}
